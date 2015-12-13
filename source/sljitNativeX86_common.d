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
import sljitLir_h;
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

enum {
    ADD = 0 << 3,
    ADD_EAX_i32 = 0x05,
    ADD_r_rm = 0x03,
    ADD_rm_r = 0x01,
    ADDSD_x_xm = 0x58,
    ADC = 2 << 3,
    ADC_EAX_i32 = 0x15,
    ADC_r_rm = 0x13,
    ADC_rm_r = 0x11,
    AND = 4 << 3,
    AND_EAX_i32 = 0x25,
    AND_r_rm = 0x23,
    AND_rm_r = 0x21,
    ANDPD_x_xm = 0x54,
    BSR_x_xm = 0xbd,
    CALL_i32 = 0xe8,
    CALL_rm = 2 << 3,
    CDQ = 0x99,
    CMOVNE_r_rm = 0x45,
    CMP = 7 << 3,
    CMP_EAX_i32 = 0x3d,
    CMP_r_rm = 0x3b,
    CMP_rm_r = 0x39,
    CVTPD2PS_x_xm = 0x5a,
    CVTSI2SD_x_rm = 0x2a,
    CVTTSD2SI_r_xm = 0x2c,
    DIV = 6 << 3,
    DIVSD_x_xm = 0x5e,
    INT3 = 0xcc,
    IDIV = 7 << 3,
    IMUL = 5 << 3,
    IMUL_r_rm = 0xaf,
    IMUL_r_rm_i8 = 0x6b,
    JE_i8 = 0x74,
    JNE_i8 = 0x75,
    JMP_i8 = 0xeb,
    JMP_i32 = 0xe9,
    JMP_rm = 4 << 3,
    LEA_r_m = 0x8d,
    MOV_r_rm = 0x8b,
    MOV_r_i32 = 0xb8,
    MOV_rm8_i8 = 0xc6,
    MOV_rm8_r8 = 0x88,
    MOVSD_x_xm = 0x10,
    MOVSD_xm_x = 0x11,
    MOVSXD_r_rm = 0x63,
    MOVSX_r_rm8 = 0xbe,
    MOVSX_r_rm16 = 0xbf,
    MOVZX_r_rm8 = 0xb6,
    MOVZX_r_rm16 = 4 << 3,
    MUL = 4 << 3,
    MULSD_x_xm = 0x59,
    NEG_rm = 3 << 3,
    NOP = 0x90,
    NOT_rm  = 2 << 3,
    OR = 1 << 3,
    OR_r_rm = 0x0b,
    OR_EAX_i32 = 0x0d,
    OR_rm_r = 0x09,
    OR_rm8_r8 = 0x08,
    POP_r = 0x59,
    POP_rm = 0x8f,
    POPF = 0x9d,
    PUSH_i32 = 0x68,
    PUSH_r = 0x50,
    PUSH_rm = 6 << 3,
    PUSHF = 0x9c,
    RET_near = 0xc3,
    RET_i16 = 0xc2,
    SBB = 3 << 3,
    SBB_EAX_i32 = 0x1d,
    SBB_r_rm = 0x1b,
    SBB_rm_r = 0x19,
    SAR = 7 << 3,
    SHL = 4 << 3,
    SHR = 5 << 3,
    SUB = 5 << 3,
    SUB_EAX_i32 = 0x2d,
    SUB_r_rm = 0x2b,
    SUB_rm_r = 0x29,
    SUBSD_x_xm = 0x5c,
    TEST_EAX_i32 = 0xa9,
    TEST_rm_r = 0x85,
    UCOMISD_x_xm = 0x2e,
    UNPCKLPD_x_xm = 0x14,
    XCHG_EAX_r = 0x90,
    XCHG_r_rm = 0x87,
    XOR = 6 << 3,
    XOR_EAX_i32 = 0x35,
    XOR_r_rm = 0x33,
    XOR_rm_r = 0x31,
    XORPD_x_xm = 0x57,

    GROUP_0F = 0x0f,
    GROUP_F7 = 0xf7,
    GROUP_FF = 0xff,
    GROUP_BINARY_81 = 0x81,
    GROUP_BINARY_83 = 0x83,
    GROUP_SHIFT_1 = 0xd1,
    GROUP_SHIFT_N = 0xc1,
    GROUP_SHIFT_CL = 0xd3,

    MOD_REG = 0xc0,
    MOD_DISP8 = 0x40,
}

string INC_SIZE(string s) pure {
    return `*inst++ = ` ~ s ~ `; compuiler.size += ` ~ s ~ `;`; }
string PUSH_REG(string r) pure {
    return `*inst++ = (PUSH_r + ` ~ r ~ `);`; }
string POP_REG(string r) pure {
    return `*inst++ = (POP_r + ` ~ r ~ `);`; }
enum RET = `*inst++ = RET_near;`;
string RET_I16(string n) pure {
    return `*inst++ = RET_i16; *inst++ = ` ~ n ~ `; *inst++ = 0;`; }
/* r32, r/m32 */
string MOV_RM(string mod, string reg, string rm) pure {
    return `
*inst++ = MOD_r_rm;
*inst++ = ` ~ mod ~ ` << 6 | ` ~ reg ~ ` << 3 | rm;`;
}

/* Multithreading does not affect these static variables, since they store
   built-in CPU features. Therefore they can be overwritten by different threads
   if they detect the CPU features in the same time. */

static if (SLJIT_DETECT_SSE2) {
    static __gshared sljit_si cpu_has_sse2 = -1;
}

static __gshared sljit_si cpu_has_cmov = -1;

void get_cpu_features() {
    import core.cpuid : sse2, hasCmov;
    static if (SLJIT_DETECT_SSE2) {
        cpu_has_sse2 = cast(sljit_si)sse2();
    }

    cpu_has_cmov = cast(sljit_si)hasCmov();
}

sljit_ub get_jump_code(sljit_si type)
{
    switch (type) {
        case SLJIT_EQUAL:
        case SLJIT_D_EQUAL:
            return 0x84 /* je */;
            
        case SLJIT_NOT_EQUAL:
        case SLJIT_D_NOT_EQUAL:
            return 0x85 /* jne */;
            
        case SLJIT_LESS:
        case SLJIT_D_LESS:
            return 0x82 /* jc */;
            
        case SLJIT_GREATER_EQUAL:
        case SLJIT_D_GREATER_EQUAL:
            return 0x83 /* jae */;
            
        case SLJIT_GREATER:
        case SLJIT_D_GREATER:
            return 0x87 /* jnbe */;
            
        case SLJIT_LESS_EQUAL:
        case SLJIT_D_LESS_EQUAL:
            return 0x86 /* jbe */;
            
        case SLJIT_SIG_LESS:
            return 0x8c /* jl */;
            
        case SLJIT_SIG_GREATER_EQUAL:
            return 0x8d /* jnl */;
            
        case SLJIT_SIG_GREATER:
            return 0x8f /* jnle */;
            
        case SLJIT_SIG_LESS_EQUAL:
            return 0x8e /* jle */;
            
        case SLJIT_OVERFLOW:
        case SLJIT_MUL_OVERFLOW:
            return 0x80 /* jo */;
            
        case SLJIT_NOT_OVERFLOW:
        case SLJIT_MUL_NOT_OVERFLOW:
            return 0x81 /* jno */;
            
        case SLJIT_D_UNORDERED:
            return 0x8a /* jp */;
            
        case SLJIT_D_ORDERED:
            return 0x8b /* jpo */;
        default:
            return 0;
    }
}

///