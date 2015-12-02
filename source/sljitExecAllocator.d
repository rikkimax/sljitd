/*
 *    Stack-less Just-In-Time compiler
 *
 *    Copyright 2009-2012 Zoltan Herczeg (hzmester@freemail.hu). All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 *
 *   1. Redistributions of source code must retain the above copyright notice, this list of
 *      conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright notice, this list
 *      of conditions and the following disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER(S) AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE COPYRIGHT HOLDER(S) OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
module sljitExecAllocator;
import sljitConfigInternal;
import sljitUtils;

extern(C):

/*
   This file contains a simple executable memory allocator

   It is assumed, that executable code blocks are usually medium (or sometimes
   large) memory blocks, and the allocator is not too frequently called (less
   optimized than other allocators). Thus, using it as a generic allocator is
   not suggested.

   How does it work:
     Memory is allocated in continuous memory areas called chunks by alloc_chunk()
     Chunk format:
     [ block ][ block ] ... [ block ][ block terminator ]

   All blocks and the block terminator is started with block_header. The block
   header contains the size of the previous and the next block. These sizes
   can also contain special values.
     Block size:
       0 - The block is a free_block, with a different size member.
       1 - The block is a block terminator.
       n - The block is used at the moment, and the value contains its size.
     Previous block size:
       0 - This is the first block of the memory chunk.
       n - The size of the previous block.

   Using these size values we can go forward or backward on the block chain.
   The unused blocks are stored in a chain list pointed by free_blocks. This
   list is useful if we need to find a suitable memory area when the allocator
   is called.

   When a block is freed, the new free block is connected to its adjacent free
   blocks if possible.

     [ free block ][ used block ][ free block ]
   and "used block" is freed, the three blocks are connected together:
     [           one big free block           ]
*/

/* --------------------------------------------------------------------- */
/*  System (OS) functions                                                */
/* --------------------------------------------------------------------- */

/* 64 KByte. */
enum CHUNK_SIZE = 0x10000;

/*
   alloc_chunk / free_chunk :
     * allocate executable system memory chunks
     * the size is always divisible by CHUNK_SIZE
   allocator_grab_lock / allocator_release_lock :
     * make the allocator thread safe
     * can be empty if the OS (or the application) does not support threading
     * only the allocator requires this lock, sljit is fully thread safe
       as it only uses local variables
*/

version(Windows) {
    void* alloc_chunk(sljit_uw size)
    {
        import core.sys.windows.windows : VirtualAlloc, MEM_COMMIT, MEM_RESERVE, PAGE_EXECUTE_READWRITE;
        return VirtualAlloc(null, size, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);
    }
    
    void free_chunk(void* chunk, sljit_uw size)
    {
        import core.sys.windows.windows : VirtualFree, MEM_RELEASE;
        VirtualFree(chunk, 0, MEM_RELEASE);
    }
} else {
    void* alloc_chunk(sljit_uw size)
    {
        import core.sys.posix.sys.mman;
        void* retval;
        
        if (dev_zero < 0) {
            if (open_dev_zero())
                return NULL;
        }
        retval = mmap(null, size, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE, dev_zero, 0);
        
        return (retval != MAP_FAILED) ? retval : NULL;
    }
    
    void free_chunk(void* chunk, sljit_uw size)
    {
        munmap(chunk, size);
    }
}

/* --------------------------------------------------------------------- */
/*  Common functions                                                     */
/* --------------------------------------------------------------------- */

enum CHUNK_MASK = ~(CHUNK_SIZE - 1);

struct block_header {
    sljit_uw size;
    sljit_uw prev_size;
}

struct free_block {
    block_header header;
    free_block *next;
    free_block *prev;
    sljit_uw size;
}

block_header* AS_BLOCK_HEADER(void* base, sljit_uw offset) {
    return cast(block_header*)((cast(sljit_ub*)base) + offset);
}

free_block* AS_FREE_BLOCK(void* base, sljit_uw offset) {
    return cast(free_block*)((cast(sljit_ub*)base) + offset);
}

void* MEM_START(void* base) {
    return cast(void*)((cast(sljit_ub*)base)+block_header.sizeof);
}

sljit_uw ALIGN_SIZE(sljit_uw size) {
    return (size + block_header.sizeof + 7) & ~7;
}

static __gshared {
    free_block* free_blocks;
    sljit_uw allocated_size;
    sljit_uw total_size;
}

void sljit_insert_free_block(free_block *free_block, sljit_uw size)
{
    free_block.header.size = 0;
    free_block.size = size;
    
    free_block.next = free_blocks;
    free_block.prev = null;
    if (free_blocks)
        free_blocks.prev = free_block;
    free_blocks = free_block;
}

void sljit_remove_free_block(free_block *free_block)
{
    if (free_block.next)
        free_block.next.prev = free_block.prev;
    
    if (free_block.prev)
        free_block.prev.next = free_block.next;
    else {
        SLJIT_ASSERT(free_blocks == free_block);
        free_blocks = free_block.next;
    }
}

void* sljit_malloc_exec(sljit_uw size)
{
    block_header *header;
    block_header *next_header;
    free_block *free_block;
    sljit_uw chunk_size;
    
    allocator_grab_lock();
    if (size < free_block.sizeof)
        size = free_block.sizeof;
    size = ALIGN_SIZE(size);
    
    free_block = free_blocks;
    while (free_block) {
        if (free_block.size >= size) {
            chunk_size = free_block.size;
            if (chunk_size > size + 64) {
                /* We just cut a block from the end of the free block. */
                chunk_size -= size;
                free_block.size = chunk_size;
                header = AS_BLOCK_HEADER(free_block, chunk_size);
                header.prev_size = chunk_size;
                AS_BLOCK_HEADER(header, size).prev_size = size;
            }
            else {
                sljit_remove_free_block(free_block);
                header = cast(block_header*)free_block;
                size = chunk_size;
            }
            allocated_size += size;
            header.size = size;
            allocator_release_lock();
            return MEM_START(header);
        }
        free_block = free_block.next;
    }
    
    chunk_size = (size + block_header.sizeof + CHUNK_SIZE - 1) & CHUNK_MASK;
    header = cast(block_header*)alloc_chunk(chunk_size);
    if (!header) {
        allocator_release_lock();
        return null;
    }
    
    chunk_size -= block_header.sizeof;
    total_size += chunk_size;
    
    header.prev_size = 0;
    if (chunk_size > size + 64) {
        /* Cut the allocated space into a free and a used block. */
        allocated_size += size;
        header.size = size;
        chunk_size -= size;
        
        free_block = AS_FREE_BLOCK(header, size);
        free_block.header.prev_size = size;
        sljit_insert_free_block(free_block, chunk_size);
        next_header = AS_BLOCK_HEADER(free_block, chunk_size);
    }
    else {
        /* All space belongs to this allocation. */
        allocated_size += chunk_size;
        header.size = chunk_size;
        next_header = AS_BLOCK_HEADER(header, chunk_size);
    }
    next_header.size = 1;
    next_header.prev_size = chunk_size;
    allocator_release_lock();
    return MEM_START(header);
}

void sljit_free_exec(void* ptr)
{
    block_header *header;
    free_block* free_block_v;

    allocator_grab_lock();
    header = AS_BLOCK_HEADER(ptr, -cast(sljit_sw)block_header.sizeof);
    allocated_size -= header.size;
    
    /* Connecting free blocks together if possible. */

    /* If header.prev_size == 0, free_block will equal to header.
       In this case, free_block.header.size will be > 0. */
    free_block_v = AS_FREE_BLOCK(header, -cast(sljit_sw)header.prev_size);
    if (SLJIT_UNLIKELY(!free_block_v.header.size)) {
        free_block_v.size += header.size;
        header = AS_BLOCK_HEADER(free_block_v, free_block_v.size);
        header.prev_size = free_block_v.size;
    }
    else {
        free_block_v = cast(free_block*)header;
        sljit_insert_free_block(free_block_v, header.size);
    }
    
    header = AS_BLOCK_HEADER(free_block_v, free_block_v.size);
    if (SLJIT_UNLIKELY(!header.size)) {
        free_block_v.size += (cast(free_block*)header).size;
        sljit_remove_free_block(cast(free_block*)header);
        header = AS_BLOCK_HEADER(free_block_v, free_block_v.size);
        header.prev_size = free_block_v.size;
    }
    
    /* The whole chunk is free. */
    if (SLJIT_UNLIKELY(!free_block_v.header.prev_size && header.size == 1)) {
        /* If this block is freed, we still have (allocated_size / 2) free space. */
        if (total_size - free_block_v.size > (allocated_size * 3 / 2)) {
            total_size -= free_block_v.size;
            sljit_remove_free_block(free_block_v);
            free_chunk(free_block_v, free_block_v.size + block_header.sizeof);
        }
    }
    
    allocator_release_lock();
}

void sljit_free_unused_memory_exec()
{
    free_block* free_block_v;
    free_block* next_free_block;
    
    allocator_grab_lock();
    
    free_block_v = free_blocks;
    while (free_block_v) {
        next_free_block = free_block_v.next;
        if (!free_block_v.header.prev_size && 
            AS_BLOCK_HEADER(free_block_v, free_block_v.size).size == 1) {
            total_size -= free_block_v.size;
            sljit_remove_free_block(free_block_v);
            free_chunk(free_block_v, free_block_v.size + block_header.sizeof);
        }
        free_block_v = next_free_block;
    }
    
    SLJIT_ASSERT((total_size && free_blocks) || (!total_size && !free_blocks));
    allocator_release_lock();
}
