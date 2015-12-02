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
module sljitConfigInternal;
public import sljitConfig;

extern(C):

/*
   SLJIT defines the following architecture dependent types and macros:

   Types:
     sljit_sb, sljit_ub : signed and unsigned 8 bit byte
     sljit_sh, sljit_uh : signed and unsigned 16 bit half-word (short) type
     sljit_si, sljit_ui : signed and unsigned 32 bit integer type
     sljit_sw, sljit_uw : signed and unsigned machine word, enough to store a pointer
     sljit_p : unsgined pointer value (usually the same as sljit_uw, but
               some 64 bit ABIs may use 32 bit pointers)
     sljit_s : single precision floating point value
     sljit_d : double precision floating point value

   Macros for feature detection (boolean):
     SLJIT_32BIT_ARCHITECTURE : 32 bit architecture
     SLJIT_64BIT_ARCHITECTURE : 64 bit architecture
     SLJIT_LITTLE_ENDIAN : little endian architecture
     SLJIT_BIG_ENDIAN : big endian architecture
     SLJIT_UNALIGNED : allows unaligned memory accesses for non-fpu operations (only!)
     SLJIT_INDIRECT_CALL : see SLJIT_FUNC_OFFSET() for more information

   Constants:
     SLJIT_NUMBER_OF_REGISTERS : number of available registers
     SLJIT_NUMBER_OF_SCRATCH_REGISTERS : number of available scratch registers
     SLJIT_NUMBER_OF_SAVED_REGISTERS : number of available saved registers
     SLJIT_NUMBER_OF_FLOAT_REGISTERS : number of available floating point registers
     SLJIT_NUMBER_OF_SCRATCH_FLOAT_REGISTERS : number of available floating point scratch registers
     SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS : number of available floating point saved registers
     SLJIT_WORD_SHIFT : the shift required to apply when accessing a sljit_sw/sljit_uw array by index
     SLJIT_DOUBLE_SHIFT : the shift required to apply when accessing
                          a double precision floating point array by index
     SLJIT_SINGLE_SHIFT : the shift required to apply when accessing
                          a single precision floating point array by index
     SLJIT_LOCALS_OFFSET : local space starting offset (SLJIT_SP + SLJIT_LOCALS_OFFSET)
     SLJIT_RETURN_ADDRESS_OFFSET : a return instruction always adds this offset to the return address

   Other macros:
     SLJIT_CALL : C calling convention define for both calling JIT form C and C callbacks for JIT
     SLJIT_W(number) : defining 64 bit constants on 64 bit architectures (compiler independent helper)
*/

/*****************/
/* Sanity check. */
/*****************/

static if(!((SLJIT_CONFIG_X86_32)
|| (SLJIT_CONFIG_X86_64)
|| (SLJIT_CONFIG_ARM_V5)
|| (SLJIT_CONFIG_ARM_V7)
|| (SLJIT_CONFIG_ARM_THUMB2)
|| (SLJIT_CONFIG_ARM_64)
|| (SLJIT_CONFIG_PPC_32)
|| (SLJIT_CONFIG_PPC_64)
|| (SLJIT_CONFIG_MIPS_32)
|| (SLJIT_CONFIG_MIPS_64)
|| (SLJIT_CONFIG_SPARC_32)
|| (SLJIT_CONFIG_TILEGX)
|| (SLJIT_CONFIG_AUTO)
|| (SLJIT_CONFIG_UNSUPPORTED))) {
    static assert(0, "An architecture must be selected");
}

static if ((SLJIT_CONFIG_X86_32)
+ (SLJIT_CONFIG_X86_64)
+ (SLJIT_CONFIG_ARM_V5)
+ (SLJIT_CONFIG_ARM_V7)
+ (SLJIT_CONFIG_ARM_THUMB2)
+ (SLJIT_CONFIG_ARM_64)
+ (SLJIT_CONFIG_PPC_32)
+ (SLJIT_CONFIG_PPC_64)
+ (SLJIT_CONFIG_TILEGX)
+ (SLJIT_CONFIG_MIPS_32)
+ (SLJIT_CONFIG_MIPS_64)
+ (SLJIT_CONFIG_SPARC_32)
+ (SLJIT_CONFIG_AUTO)
+ (SLJIT_CONFIG_UNSUPPORTED) >= 2) {
    static assert(0, "Multiple architectures are selected");
}

/********************************************************/
/* Automatic CPU detection (requires compiler support). */
/********************************************************/

static if(SLJIT_CONFIG_AUTO) {
    version(Windows) {
        version(X86_64) {
            enum SLJIT_CONFIG_X86_64 = true;
        } else version(X86) {
            enum SLJIT_CONFIG_X86_32 = true;
        } else version(ARM) {
            enum SLJIT_CONFIG_ARM_V5 = true;
        } else {
            enum SLJIT_CONFIG_UNSUPPORTED = true;
        }
    } else {
        version(X86) {
            enum SLJIT_CONFIG_X86_32 = true;
        } else version(X86_64) {
            enum SLJIT_CONFIG_X86_64 = true;
        } else version(ARM) {
            version(ARM_Thumb) {
                enum SLJIT_CONFIG_ARM_THUMB2 = true;
            } else version(ARM_V7) {
                enum SLJIT_CONFIG_ARM_V7 = true;
            } else {
                enum SLJIT_CONFIG_ARM_V5 = true;
            }
        } else version(AArch64) {
            enum SLJIT_CONFIG_ARM_64 = true;
        } else version(PPC64) {
            enum SLJIT_CONFIG_PPC_64 = true;
        } else version(PPC) {
            enum SLJIT_CONFIG_PPC_32 = true;
        } else version(MIPS64) {
            enum SLJIT_CONFIG_MIPS_64 = true;
        } else version(MIPS32) {
            enum SLJIT_CONFIG_MIPS_32 = true;
        } else version(SPARC) {
            enum SLJIT_CONFIG_SPARC_32 = true;
        } else version(TileGX) {
            enum SLJIT_CONFIG_TILEGX = true;
        } else {
            enum SLJIT_CONFIG_UNSUPPORTED = true;
        }
    }
}

static if (SLJIT_CONFIG_UNSUPPORTED) {
    enum SLJIT_EXECUTABLE_ALLOCATOR = false;
}

/******************************/
/* CPU family type detection. */
/******************************/

static if ((SLJIT_CONFIG_ARM_V5) || (SLJIT_CONFIG_ARM_V7)
|| (SLJIT_CONFIG_ARM_THUMB2)) {
    enum SLJIT_CONFIG_ARM_32 = true;
} else {
    enum SLJIT_CONFIG_ARM_32 = false;
}

static if ((SLJIT_CONFIG_X86_32) || (SLJIT_CONFIG_X86_64)) {
    enum SLJIT_CONFIG_X86 = true;

    enum SLJIT_CONFIG_ARM = false;
    enum SLJIT_CONFIG_PPC = false;
    enum SLJIT_CONFIG_MIPS = false;
    enum SLJIT_CONFIG_SPARC = false;
} else static if ((SLJIT_CONFIG_ARM_32) || (SLJIT_CONFIG_ARM_64)) {
    enum SLJIT_CONFIG_ARM = true;

    enum SLJIT_CONFIG_X86 = false;
    enum SLJIT_CONFIG_PPC = false;
    enum SLJIT_CONFIG_MIPS = false;
    enum SLJIT_CONFIG_SPARC = false;
} else static if ((SLJIT_CONFIG_PPC_32) || (SLJIT_CONFIG_PPC_64)) {
    enum SLJIT_CONFIG_PPC = true;

    enum SLJIT_CONFIG_X86 = false;
    enum SLJIT_CONFIG_ARM = false;
    enum SLJIT_CONFIG_MIPS = false;
    enum SLJIT_CONFIG_SPARC = false;
} else static if ((SLJIT_CONFIG_MIPS_32) || (SLJIT_CONFIG_MIPS_64)) {
    enum SLJIT_CONFIG_MIPS = true;

    enum SLJIT_CONFIG_X86 = false;
    enum SLJIT_CONFIG_ARM = false;
    enum SLJIT_CONFIG_PPC = false;
    enum SLJIT_CONFIG_SPARC = false;
} else static if ((SLJIT_CONFIG_SPARC_32) || (SLJIT_CONFIG_SPARC_64)) {
    enum SLJIT_CONFIG_SPARC = true;

    enum SLJIT_CONFIG_X86 = false;
    enum SLJIT_CONFIG_ARM = false;
    enum SLJIT_CONFIG_PPC = false;
    enum SLJIT_CONFIG_MIPS = false;
}

/**********************************/
/* External function definitions. */
/**********************************/

import core.stdc.stdlib;
import core.stdc.string;

alias SLJIT_MALLOC = malloc;
alias SLJIT_FREE = free;
alias SLJIT_MEMMOVE = memmove;

auto SLJIT_ZEROMEM(void* s, size_t n) @system nothrow pure { return memset(s, 0, n); }

/***************************/
/* Compiler helper macros. */
/***************************/

version(GNU) {
    auto SLJIT_LIKELY(T)(T x) { return __builtin_expect(x, 1); }
    auto SLJIT_UNLIKELY(T)(T x) { return __builtin_expect(x, 0); }
} else {
    auto SLJIT_LIKELY(T)(T x) { return x; }
    auto SLJIT_UNLIKELY(T)(T x) { return x; }
}

// Ignore this: SLJIT_INLINE
// Just use const for SLJIT_CONST
// Ignore this: SLJIT_UNUSED_ARG

/*********************************/
/* Type of public API functions. */
/*********************************/

// Ignore this: SLJIT_API_FUNC_ATTRIBUTE

/****************************/
/* Instruction cache flush. */
/****************************/

static if (SLJIT_CONFIG_X86) {
    void SLJIT_CACHE_FLUSH(void* t, size_t u) {}
} else version(OSX) {
    void SLJIT_CACHE_FLUSH(void* t, size_t u) {
        sys_icache_invalidate(t, t - u);
    }
} else version(Android) {
    void SLJIT_CACHE_FLUSH(void* t, size_t u) {
        cacheflush(t, cast(int)u, 0);
    }
} else static if (SLJIT_CONFIG_PPC) {
    void SLJIT_CACHE_FLUSH(void* t, size_t u) {
        ppc_cache_flush(t, u);
    }
} else static if (SLJIT_CONFIG_SPARC_32) {
    void SLJIT_CACHE_FLUSH(void* t, size_t u) {
        sparc_cache_flush(t, u);
    }
} else {
    void SLJIT_CACHE_FLUSH(void* t, size_t u) {
        __clear_cache(t, cast(void*)u);
    }
}

/******************************************************/
/* Byte/half/int/word/single/double type definitions. */
/******************************************************/

/* 8 bit byte type. */
alias sljit_ub = ubyte;
alias sljit_sb = byte;

/* 16 bit half-word type. */
alias sljit_uh = ushort;
alias sljit_sh = short;

/* 32 bit integer type. */
alias sljit_ui = uint;
alias sljit_si = int;

/* Machine word type. Enough for storing a pointer.
     32 bit for 32 bit machines.
     64 bit for 64 bit machines. */

static if (SLJIT_CONFIG_UNSUPPORTED) {
    /* Just to have something. */
    enum SLJIT_WORD_SHIFT = 0;
    enum SLJIT_32BIT_ARCHITECTURE = false;
    enum SLJIT_64BIT_ARCHITECTURE = false;

    alias sljit_uw = size_t;
    alias sljit_sw = ptrdiff_t;
} else static if (!(SLJIT_CONFIG_X86_64 || SLJIT_CONFIG_ARM_64 || SLJIT_CONFIG_PPC_64 || SLJIT_CONFIG_MIPS_64 || SLJIT_CONFIG_TILEGX)) {
    enum SLJIT_32BIT_ARCHITECTURE = true;
    enum SLJIT_64BIT_ARCHITECTURE = false;
    enum SLJIT_WORD_SHIFT = 2;

    alias sljit_uw = uint;
    alias sljit_sw = int;
} else {
    enum SLJIT_32BIT_ARCHITECTURE = false;
    enum SLJIT_64BIT_ARCHITECTURE = true;
    enum SLJIT_WORD_SHIFT = 2;

    alias sljit_uw = ulong;
    alias sljit_sw = long;
}

alias sljit_p = sljit_uw;

/* Floating point types. */
alias sljit_s = float;
alias sljit_d = double;

/* Shift for pointer sized data. */
alias SLJIT_POINTER_SHIFT = SLJIT_WORD_SHIFT;

/* Shift for double precision sized data. */
enum SLJIT_DOUBLE_SHIFT = 3;
enum SLJIT_SINGLE_SHIFT = 2;

// Ignore SLJIT_W

/*************************/
/* Endianness detection. */
/*************************/

version(BigEndian) {
    enum SLJIT_BIG_ENDIAN = true;
    enum SLJIT_LITTLE_ENDIAN = false;
} else {
    enum SLJIT_BIG_ENDIAN = false;
    enum SLJIT_LITTLE_ENDIAN = true;
}

static if (SLJIT_CONFIG_X86_32 || SLJIT_CONFIG_X86_64 || SLJIT_CONFIG_ARM_V7 || SLJIT_CONFIG_ARM_THUMB2 || SLJIT_CONFIG_ARM_64 || SLJIT_CONFIG_PPC_32 || SLJIT_CONFIG_PPC_64) {
    enum SLJIT_UNALIGNED = true;
} else {
    enum SLJIT_UNALIGNED = false;
}

static if(SLJIT_CONFIG_X86_32) {
    /* Auto detect SSE2 support using CPUID.
   On 64 bit x86 cpus, sse2 must be present. */
    enum SLJIT_DETECT_SSE2 = true;
} else {
    enum SLJIT_DETECT_SSE2 = false;
}

/*****************************************************************************************/
/* Calling convention of functions generated by SLJIT or called from the generated code. */
/*****************************************************************************************/

// Ignore: SLJIT_CALL
enum SLJIT_X86_32_FASTCALL = false;

static if (SLJIT_CONFIG_PPC_64 || SLJIT_BIG_ENDIAN || SLJIT_CONFIG_PPC_32) {
    version(AIX) {
        /* It seems certain ppc compilers use an indirect addressing for functions
            which makes things complicated. */
        enum SLJIT_INDIRECT_CALL = true;
    } else {
        enum SLJIT_INDIRECT_CALL = false;
    }
} else {
    enum SLJIT_INDIRECT_CALL = false;
}

/* The offset which needs to be substracted from the return address to
determine the next executed instruction after return. */

static if (SLJIT_CONFIG_SPARC_32) {
    enum SLJIT_RETURN_ADDRESS_OFFSET = 8;
} else {
    enum SLJIT_RETURN_ADDRESS_OFFSET = 8;
}

/***************************************************/
/* Functions of the built-in executable allocator. */
/***************************************************/

extern(C) {
    void* sljit_malloc_exec(sljit_uw size);
    void sljit_free_exec(void* ptr);
    void sljit_free_unused_memory_exec();

    alias SLJIT_MALLOC_EXEC = sljit_malloc_exec;
    alias SLJIT_FREE_EXEC = sljit_free_exec;
}

/**********************************************/
/* Registers and locals offset determination. */
/**********************************************/

static if (SLJIT_CONFIG_X86_32) {
    enum SLJIT_NUMBER_OF_REGISTERS = 10;
    enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 7;

    static if (SLJIT_X86_32_FASTCALL) {
        enum SLJIT_LOCALS_OFFSET_BASE = (2 + 4) * sljit_sw.sizeof;
    } else {
        /* Maximum 3 arguments are passed on the stack, +1 for double alignment. */
        enum SLJIT_LOCALS_OFFSET_BASE = (3 + 1 + 4) * sljit_sw.sizeof;
    }
} else static if (SLJIT_CONFIG_X86_64) {
    version(Windows) {
        enum SLJIT_NUMBER_OF_REGISTERS = 12;
        enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 8;
        enum SLJIT_LOCALS_OFFSET_BASE = (4 + 2) * sljit_sw.sizeof;
    } else {
        enum SLJIT_NUMBER_OF_REGISTERS = 12;
        enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 6;
        enum SLJIT_LOCALS_OFFSET_BASE = sljit_sw.sizeof;
    }
} else static if (SLJIT_CONFIG_ARM_V5 || SLJIT_CONFIG_ARM_V7) {
    enum SLJIT_NUMBER_OF_REGISTERS = 11;
    enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 8;
    enum SLJIT_LOCALS_OFFSET_BASE = 0; 
} else static if (SLJIT_CONFIG_ARM_THUMB2) {
    enum SLJIT_NUMBER_OF_REGISTERS = 11;
    enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 7;
    enum SLJIT_LOCALS_OFFSET_BASE = 0;
} else static if (SLJIT_CONFIG_ARM_64) {
    enum SLJIT_NUMBER_OF_REGISTERS = 25;
    enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 10;
    enum SLJIT_LOCALS_OFFSET_BASE = 2 * sljit_sw.sizeof;
} else static if (SLJIT_CONFIG_PPC) {
    enum SLJIT_NUMBER_OF_REGISTERS = 22;
    enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 17;

    static if (SLJIT_CONFIG_PPC_64) {
        enum SLJIT_LOCALS_OFFSET_BASE = (6 + 8) * sljit_sw.sizeof;
    } else version(AIX) {
        enum SLJIT_LOCALS_OFFSET_BASE = (6 + 8) * sljit_sw.sizeof;
    } else static if (SLJIT_CONFIG_PPC_32) {
        enum SLJIT_LOCALS_OFFSET_BASE = (3 + 1) * sljit_sw.sizeof;
    } else {
        enum SLJIT_LOCALS_OFFSET_BASE = 3 * sljit_sw.sizeof;
    }
} else static if (SLJIT_CONFIG_MIPS) {
    enum SLJIT_NUMBER_OF_REGISTERS = 17;
    enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 8;

    static if (SLJIT_CONFIG_MIPS_32) {
        enum SLJIT_LOCALS_OFFSET_BASE = 4 * sljit_sw.sizeof;
    } else {
        enum SLJIT_LOCALS_OFFSET_BASE = 0;
    }
} else static if (SLJIT_CONFIG_SPARC) {
    enum SLJIT_NUMBER_OF_REGISTERS = 18;
    enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 14;

    static if (SLJIT_CONFIG_SPARC_32) {
        /* Add +1 for double alignment. */
        enum SLJIT_LOCALS_OFFSET_BASE = (23 + 1) * sljit_sw.sizeof;
    }
} else static if (SLJIT_CONFIG_UNSUPPORTED) {
    enum SLJIT_NUMBER_OF_REGISTERS = 0;
    enum SLJIT_NUMBER_OF_SAVED_REGISTERS = 0;
    enum SLJIT_LOCALS_OFFSET_BASE = 0;
}

alias SLJIT_LOCALS_OFFSET = SLJIT_LOCALS_OFFSET_BASE;
enum SLJIT_NUMBER_OF_SCRATCH_REGISTERS = SLJIT_NUMBER_OF_REGISTERS - SLJIT_NUMBER_OF_SAVED_REGISTERS;

enum SLJIT_NUMBER_OF_FLOAT_REGISTERS = 6;

version(Win64) {
    enum SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS = 1;
} else {
    enum SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS = 0;
}

enum SLJIT_NUMBER_OF_SCRATCH_FLOAT_REGISTERS = SLJIT_NUMBER_OF_FLOAT_REGISTERS - SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS;

/*************************************/
/* Debug and verbose related macros. */
/*************************************/

static if (SLJIT_VERBOSE) {
    import core.stdc.stdio;
}

static if (SLJIT_DEBUG) {
    /* SLJIT_HALT_PROCESS must halt the process. */
    import core.stdc.stdlib : abort;
    alias SLJIT_HALT_PROCESS = abort;

    void SLJIT_ASSERT(lazy bool x, string mod=__MODULE__, int line = __LINE__) {
        import std.stdio : writeln;

        do {
            if (SLJIT_UNLIKELY(!x)) {
                writeln("Assertion failed at ", mod, ":", line);
                SLJIT_HALT_PROCESS();
            }
        } while(0);
    }

    void SLJIT_ASSERT_STOP(string mod=__MODULE__, int line = __LINE__) {
        import std.stdio : writeln;
        
        do {
            writeln("Should never been reached ", mod, ":", line);
            SLJIT_HALT_PROCESS();
        } while(0);
    }
} else {
    void SLJIT_ASSERT(lazy bool x, string mod=__MODULE__, int line = __LINE__) {
        do {
        } while(0);
    }
    
    void SLJIT_ASSERT_STOP(string mod=__MODULE__, int line = __LINE__) {
        do {
        } while(0);
    }
}

void SLJIT_COMPILE_ASSERT(lazy bool b, char* description) {
    SLJIT_ASSERT(b);
}