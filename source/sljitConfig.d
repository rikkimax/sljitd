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
module sljitConfig;

/* --------------------------------------------------------------------- */
/*  Custom defines                                                       */
/* --------------------------------------------------------------------- */

/* Put your custom defines here. This empty section will never change
   which helps maintaining patches (with diff / patch utilities). */

/* --------------------------------------------------------------------- */
/*  Architecture                                                         */
/* --------------------------------------------------------------------- */

/* Architecture selection. */

enum SLJIT_CONFIG_X86_32 = false;
enum SLJIT_CONFIG_X86_64 = false;
enum SLJIT_CONFIG_ARM_V5 = false;
enum SLJIT_CONFIG_ARM_V7 = false;
enum SLJIT_CONFIG_ARM_THUMB2 = false;
enum SLJIT_CONFIG_ARM_64 = false;
enum SLJIT_CONFIG_PPC_32 = false;
enum SLJIT_CONFIG_PPC_64 = false;
enum SLJIT_CONFIG_MIPS_32 = false;
enum SLJIT_CONFIG_MIPS_64 = false;
enum SLJIT_CONFIG_SPARC_32 = false;
enum SLJIT_CONFIG_TILEGX = false;

enum SLJIT_CONFIG_AUTO = true;
enum SLJIT_CONFIG_UNSUPPORTED = false;

/* --------------------------------------------------------------------- */
/*  Utilities                                                            */
/* --------------------------------------------------------------------- */

/* Useful for thread-safe compiling of global functions. */
version(SLJIT_UTIL_GLOBAL_LOCK) {
    enum SLJIT_UTIL_GLOBAL_LOCK = false;
} else {
    /* Enabled by default */
    enum SLJIT_UTIL_GLOBAL_LOCK = true;
}

/* Implements a stack like data structure (by using mmap / VirtualAlloc). */
version(SLJIT_UTIL_STACK) {
    enum SLJIT_UTIL_STACK = false;
} else {
    /* Enabled by default */
    enum SLJIT_UTIL_STACK = true;
}

/* Single threaded application. Does not require any locks. */
version(SLJIT_SINGLE_THREADED) {
    enum SLJIT_SINGLE_THREADED = true;
} else {
    /* Disabled by default. */
    enum SLJIT_SINGLE_THREADED = false;
}

/* --------------------------------------------------------------------- */
/*  Configuration                                                        */
/* --------------------------------------------------------------------- */

/* If SLJIT_STD_MACROS_DEFINED is not defined, the application should
   define SLJIT_MALLOC, SLJIT_FREE, SLJIT_MEMMOVE, and NULL. */
version(SLJIT_STD_MACROS_DEFINED) {
    enum SLJIT_STD_MACROS_DEFINED = true;
} else {
    /* Disabled by default. */
    enum SLJIT_STD_MACROS_DEFINED = false;
}

/* Executable code allocation:
   If SLJIT_EXECUTABLE_ALLOCATOR is not defined, the application should
   define both SLJIT_MALLOC_EXEC and SLJIT_FREE_EXEC. */
version(SLJIT_EXECUTABLE_ALLOCATOR) {
    enum SLJIT_EXECUTABLE_ALLOCATOR = false;
} else {
    /* Enabled by default. */
    enum SLJIT_EXECUTABLE_ALLOCATOR = true;
}

/* Return with error when an invalid argument is passed. */
version(SLJIT_ARGUMENT_CHECKS) {
    enum SLJIT_ARGUMENT_CHECKS = true;
} else {
    /* Disabled by default */
    enum SLJIT_ARGUMENT_CHECKS = false;
}

/* Debug checks (assertions, etc.). */
version(SLJIT_DEBUG) {
    enum SLJIT_DEBUG = false;
} else {
    /* Enabled by default */
    enum SLJIT_DEBUG = true;
}

/* Verbose operations. */
version(SLJIT_VERBOSE) {
    enum SLJIT_VERBOSE = false;
} else {
    /* Enabled by default */
    enum SLJIT_VERBOSE = true;
}

/*
  SLJIT_IS_FPU_AVAILABLE
    The availability of the FPU can be controlled by SLJIT_IS_FPU_AVAILABLE.
      zero value - FPU is NOT present.
      nonzero value - FPU is present.
*/

/* For further configurations, see the beginning of sljitConfigInternal.h */