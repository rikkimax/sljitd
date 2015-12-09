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
module sljitNativeX86_common;
import sljitConfigInternal;
import sljitLir_c;

extern(C):

const(char*) sljit_get_platform_name() {
    enum FORMAT = "x86" ~ SLJIT_CPUINFO;
    return FORMAT;
}

/*
   32b register indexes:
     0 - EAX
     1 - ECX
     2 - EDX
     3 - EBX
     4 - none
     5 - EBP
     6 - ESI
     7 - EDI
*/

/*
   64b register indexes:
     0 - RAX
     1 - RCX
     2 - RDX
     3 - RBX
     4 - none
     5 - RBP
     6 - RSI
     7 - RDI
     8 - R8   - From now on REX prefix is required
     9 - R9
    10 - R10
    11 - R11
    12 - R12
    13 - R13
    14 - R14
    15 - R15
*/

static if (SLJIT_CONFIG_X86_32) {
    enum TMP_REG1 = SLJIT_NUMBER_OF_REGISTERS + 2;

    const(sljit_ub[SLJIT_NUMBER_OF_REGISTERS + 3]) reg_map = [
        0, 0, 2, 1, 0, 0, 0, 0, 7, 6, 3, 4, 5
    ];

    string CHECK_EXTRA_REGS(string p, string w, string do_) pure {
        return `
    if (p >= SLJIT_R3 && p <= SLJIT_R6) {
        ` ~ w ~ ` = SLJIT_LOCALS_OFFSET + ((` ~ p ~ `) - (SLJIT_R3 + 4)) * sljit_sw.sizeof;
        ` ~ p ~ ` = SLJIT_MEM1(SLJIT_SP);
        ` ~ do_ ~ `;
    }
`;
    }

} else {
    /* Last register + 1. */
    enum TMP_REG1 = SLJIT_NUMBER_OF_REGISTERS + 2;
    enum TMP_REG2 = SLJIT_NUMBER_OF_REGISTERS + 3;
    enum TMP_REG3 = SLJIT_NUMBER_OF_REGISTERS + 4;

    /* Note: r12 & 0x7 == 0b100, which decoded as SIB byte present
   Note: avoid to use r12 and r13 for memory addessing
   therefore r12 is better for SAVED_EREG than SAVED_REG. */

    version(Win64) {
        /* 1st passed in rcx, 2nd argument passed in rdx, 3rd in r8. */
        const(sljit_ub[SLJIT_NUMBER_OF_REGISTERS + 5]) reg_map = [
            0, 0, 2, 1, 11, 12, 5, 13, 14, 15, 7, 6, 3, 4, 10, 8, 9
        ];
        /* low-map. reg_map & 0x7. */
        const(sljit_ub[SLJIT_NUMBER_OF_REGISTERS + 5]) reg_lmap = [
            0, 0, 2, 1, 3,  4,  5,  5, 6,  7,  7, 6, 3, 4, 2,  0, 1
        ];
    } else {
        /* 1st passed in rdi, 2nd argument passed in rsi, 3rd in rdx. */
        const(sljit_ub[SLJIT_NUMBER_OF_REGISTERS + 5]) reg_map = [
            0, 0, 6, 1, 8, 11, 10, 12, 5, 13, 14, 15, 3, 4, 2, 7, 9
        ];
        /* low-map. reg_map & 0x7. */
        const(sljit_ub[SLJIT_NUMBER_OF_REGISTERS + 5]) reg_lmap = [
            0, 0, 6, 1, 0, 3,  2,  4,  5,  5,  6,  7, 3, 4, 2, 7, 1
        ];
    }

    enum REX_W = 0x48;
    enum REX_R = 0x44;
    enum REX_X = 0x42;
    enum REX_B = 0x41;
    enum REX = 0x40;

    version(Win64) {
        enum HALFWORD_MAX = 0x7fffffff;
        enum HALFWORD_MIN = -0x80000000;
    } else {
        enum HALFWORD_MAX = 0x7fffffff;
        enum HALFWORD_MIN = -0x80000000;
    }

    bool IS_HALF_WORD(T)(T x) {
        return x <= HALFWORD_MAX && x >= HALFWORD_MIN; }
    bool NOT_HALFWORD(T)(T x) {
        return x > HALFWORD_MAX || x < HALFWORD_MIN; }

    string CHECK_EXTRA_REGS(string p, string w, string do_) pure {
        return ``; }
}

enum TMP_FREG = 0;

/* Size flags for emit_x86_instruction: */
enum EX86_BIN_INS = 0x0010;
enum EX86_SHIFT_INS = 0x0020;
enum EX86_REX = 0x0040;
enum EX86_NO_REXW = 0x0080;
enum EX86_BYTE_ARG = 0x0100;
enum EX86_HALF_ARG = 0x0200;
enum EX86_PREF_66 = 0x0400;
enum EX86_PREF_F2 = 0x0800;
enum EX86_PREF_F3 = 0x1000;
enum EX86_SSE2_OP1 = 0x2000;
enum EX86_SSE2_OP2 = 0x4000;
enum EX86_SSE2 = (EX86_SSE2_OP1 | EX86_SSE2_OP2);

/* --------------------------------------------------------------------- */
/*  Instrucion forms                                                     */
/* --------------------------------------------------------------------- */

