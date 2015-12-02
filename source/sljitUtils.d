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
module sljitUtils;
import sljitConfigInternal;
import sljitLir_h;

extern(C):

/* ------------------------------------------------------------------------ */
/*  Locks                                                                   */
/* ------------------------------------------------------------------------ */

static if (SLJIT_EXECUTABLE_ALLOCATOR || SLJIT_UTIL_GLOBAL_LOCK) {
    static if (SLJIT_SINGLE_THREADED) {
        static if (SLJIT_EXECUTABLE_ALLOCATOR) {
            void allocator_grab_lock()
            {
                /* Always successful. */
            }
            
            void allocator_release_lock()
            {
                /* Always successful. */
            }
        } else static if (SLJIT_UTIL_GLOBAL_LOCK) {
            void sljit_grab_lock()
            {
                /* Always successful. */
            }
            
            void sljit_release_lock()
            {
                /* Always successful. */
            }
        }
    } else version(Windows) {
        import core.sys.windows.windows : HANDLE, WaitForSingleObject, INFINITE;
        extern(Windows) {
            HANDLE CreateMutex(void*, bool, void*);
            bool ReleaseMutex(HANDLE);
        }

        static if (SLJIT_EXECUTABLE_ALLOCATOR) {
            static __gshared HANDLE allocator_mutex;

            void allocator_grab_lock()
            {
                /* No idea what to do if an error occures. Static mutexes should never fail... */
                if (allocator_mutex is null)
                    allocator_mutex = CreateMutex(null, true, null);
                else
                    WaitForSingleObject(allocator_mutex, INFINITE);
            }

            void allocator_release_lock()
            {
                ReleaseMutex(allocator_mutex);
            }
        } else static if (SLJIT_UTIL_GLOBAL_LOCK) {
            static __gshared HANDLE global_mutex;
            
            void allocator_grab_lock()
            {
                /* No idea what to do if an error occures. Static mutexes should never fail... */
                if (allocator_mutex is null)
                    global_mutex = CreateMutex(null, true, null);
                else
                    WaitForSingleObject(global_mutex, INFINITE);
            }
            
            void allocator_release_lock()
            {
                ReleaseMutex(global_mutex);
            }
        }
    } else {
        import core.sys.posix.pthread;

        static if (SLJIT_EXECUTABLE_ALLOCATOR) {
            static __gshared pthread_mutex_t allocator_mutex = PTHREAD_MUTEX_INITIALIZER;
            
            void allocator_grab_lock()
            {
                pthread_mutex_lock(&allocator_mutex);
            }
            
            void allocator_release_lock()
            {
                pthread_mutex_unlock(&allocator_mutex);
            }
        } else static if (SLJIT_UTIL_GLOBAL_LOCK) {
            static __gshared pthread_mutex_t global_mutex = PTHREAD_MUTEX_INITIALIZER;
            
            void sljit_grab_lock()
            {
                pthread_mutex_lock(&global_mutex);
            }
            
            void sljit_release_lock()
            {
                pthread_mutex_unlock(&global_mutex);
            }
        }
    }

    /* ------------------------------------------------------------------------ */
    /*  Stack                                                                   */
    /* ------------------------------------------------------------------------ */

    static if (SLJIT_UTIL_STACK || SLJIT_EXECUTABLE_ALLOCATOR) {
        version(Windows) {
            import core.sys.windows.windows;
        } else {
            /* Provides mmap function. */
            import core.sys.posix.sys.mman;
            /* For detecting the page size. */
            import core.sys.posix.unistd;

            import core.sys.posix.fcntl;
            static __gshared sljit_si dev_zero = -1;

            static if(SLJIT_SINGLE_THREADED) {
                sljit_si open_dev_zero()
                {
                    dev_zero = open(cast(char*)"/dev/zero\0".ptr, O_RDWR);
                    return dev_zero < 0;
                }
            } else {
                static __gshared pthread_mutex_t dev_zero_mutex = PTHREAD_MUTEX_INITIALIZER;

                sljit_si open_dev_zero()
                {
                    pthread_mutex_lock(&dev_zero_mutex);
                    dev_zero = open(cast(char*)"/dev/zero\0".ptr, O_RDWR);
                    pthread_mutex_unlock(&dev_zero_mutex);
                    return dev_zero < 0;
                }
            }
        }
    }

    static if (SLJIT_UTIL_STACK) {
        /* Planning to make it even more clever in the future. */
        static __gshared sljit_sw sljit_page_align = 0;

        sljit_stack* sljit_allocate_stack(sljit_uw limit, sljit_uw max_limit)
        {
            sljit_stack *stack;
            union base_t {
                void *ptr;
                sljit_uw uw;
            }
            base_t base;

            version(Windows) {
                SYSTEM_INFO si;
            }
            
            if (limit > max_limit || limit < 1)
                return null;
            
            version(Windows) {
                if (!sljit_page_align) {
                    GetSystemInfo(&si);
                    sljit_page_align = si.dwPageSize - 1;
                }
            } else {
                if (!sljit_page_align) {
                    sljit_page_align = sysconf(_SC_PAGESIZE);
                    /* Should never happen. */
                    if (sljit_page_align < 0)
                        sljit_page_align = 4096;
                    sljit_page_align--;
                }
            }
            
            /* Align limit and max_limit. */
            max_limit = (max_limit + sljit_page_align) & ~sljit_page_align;
            
            stack = cast(sljit_stack*)SLJIT_MALLOC(sljit_stack.sizeof);
            if (!stack)
                return null;
            
            version(Windows) {
                base.ptr = VirtualAlloc(null, max_limit, MEM_RESERVE, PAGE_READWRITE);
                if (!base.ptr) {
                    SLJIT_FREE(stack);
                    return null;
                }
                stack.base = base.uw;
                stack.limit = stack.base;
                stack.max_limit = stack.base + max_limit;
                if (sljit_stack_resize(stack, stack.base + limit)) {
                    sljit_free_stack(stack);
                    return null;
                }
            } else {
                if (dev_zero < 0) {
                    if (open_dev_zero()) {
                        SLJIT_FREE(stack);
                        return null;
                    }
                }
                base.ptr = mmap(NULL, max_limit, PROT_READ | PROT_WRITE, MAP_PRIVATE, dev_zero, 0);
            
                if (base.ptr == MAP_FAILED) {
                    SLJIT_FREE(stack);
                    return null;
                }
                stack.base = base.uw;
                stack.limit = stack.base + limit;
                stack.max_limit = stack.base + max_limit;
            }
            stack.top = stack.base;
            return stack;
        }

        void sljit_free_stack(sljit_stack* stack)
        {
            version(Windows) {
                VirtualFree(cast(void*)stack.base, 0, MEM_RELEASE);
            } else {
                munmap(cast(void*)stack.base, stack.max_limit - stack.base);
            }
            SLJIT_FREE(stack);
        }

        sljit_sw sljit_stack_resize(sljit_stack* stack, sljit_uw new_limit)
        {
            sljit_uw aligned_old_limit;
            sljit_uw aligned_new_limit;
            
            if ((new_limit > stack.max_limit) || (new_limit < stack.base))
                return -1;

            version(Windows) {
                aligned_new_limit = (new_limit + sljit_page_align) & ~sljit_page_align;
                aligned_old_limit = (stack.limit + sljit_page_align) & ~sljit_page_align;
                if (aligned_new_limit != aligned_old_limit) {
                    if (aligned_new_limit > aligned_old_limit) {
                        if (!VirtualAlloc(cast(void*)aligned_old_limit, aligned_new_limit - aligned_old_limit, MEM_COMMIT, PAGE_READWRITE))
                            return -1;
                    }
                    else {
                        if (!VirtualFree(cast(void*)aligned_new_limit, aligned_old_limit - aligned_new_limit, MEM_DECOMMIT))
                            return -1;
                    }
                }
                stack.limit = new_limit;
                return 0;
            } else {
                if (new_limit >= stack.limit) {
                    stack.limit = new_limit;
                    return 0;
                }
                aligned_new_limit = (new_limit + sljit_page_align) & ~sljit_page_align;
                aligned_old_limit = (stack.limit + sljit_page_align) & ~sljit_page_align;
                /* If madvise is available, we release the unnecessary space. */
    /*#if defined(MADV_DONTNEED)
                if (aligned_new_limit < aligned_old_limit)
                    madvise(cast(void*)aligned_new_limit, aligned_old_limit - aligned_new_limit, MADV_DONTNEED);
    #elif defined(POSIX_MADV_DONTNEED)
                if (aligned_new_limit < aligned_old_limit)
                    posix_madvise(cast(void*)aligned_new_limit, aligned_old_limit - aligned_new_limit, POSIX_MADV_DONTNEED);
    #endif*/
                stack.limit = new_limit;
                return 0;
            }
        }
    }
}
