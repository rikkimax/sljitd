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
module sljitLir_c;
import sljitLir_h;
import sljitConfigInternal;

extern(C):

enum CHECK_ERROR = q{
    do {
        if (SLJIT_UNLIKELY(compiler.error))
            return compiler.error;
    } while(0);
};

enum CHECK_ERROR_PTR = q{
    do {
        if (SLJIT_UNLIKELY(compiler.error))
            return null;
    } while(0);
};

string FAIL_IF(string expr) pure {
    return `
    do {
        if (SLJIT_UNLIKELY(` ~ expr ~ `))
            return compiler.error;
    } while(0);
`;
}

string PTR_FAIL_IF(string expr) pure {
    return `
    do {
        if (SLJIT_UNLIKELY(` ~ expr ~ `))
            return null;
    } while(0);
`;
}

string FAIL_IF_NULL(string ptr) pure {
    return `
    do {
        if (SLJIT_UNLIKELY(` ~ ptr ~ ` is null)) {
            compiler.error = SLJIT_ERR_ALLOC_FAILED;
            return SLJIT_ERR_ALLOC_FAILED;
        }
    } while(0);
`;
}

string PTR_FAIL_IF_NULL(string ptr) pure {
    return `
    do {
        if (SLJIT_UNLIKELY(` ~ ptr ~ ` is null)) {
            compiler.error = SLJIT_ERR_ALLOC_FAILED;
            return null;
        }
    } while(0);
`;
}

string PTR_FAIL_WITH_EXEC_IF(string ptr) pure {
    return `
    do {
        if (SLJIT_UNLIKELY(` ~ ptr ~ ` is null)) {
            compiler.error = SLJIT_ERR_EX_ALLOC_FAILED;
            return null;
        }
    } while(0);
`;
}

static if (!SLJIT_CONFIG_UNSUPPORTED) {
    int GET_OPCODE(int op) pure {
        return op & ~(SLJIT_INT_OP | SLJIT_SET_E | SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_O | SLJIT_SET_C | SLJIT_KEEP_FLAGS); }

    int GET_FLAGS(int op) pure {
        return op & ~(SLJIT_SET_E | SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_O | SLJIT_SET_C); }

    int GET_ALL_FLAGS(int op) pure {
        return op & ~(SLJIT_INT_OP | SLJIT_SET_E | SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_O | SLJIT_SET_C | SLJIT_KEEP_FLAGS); }

    int TYPE_CAST_NEEDED(int op) pure {
        return (op >= SLJIT_MOV_UB && op <= SLJIT_MOV_SH) || (op >= SLJIT_MOVU_UB && op <= SLJIT_MOVU_SH); }

    enum BUF_SIZE = 4096;

    static if (SLJIT_32BIT_ARCHITECTURE)
        enum ABUF_SIZE = 2048;
    else
        enum ABUF_SIZE = 4096;

    enum REG_MASK = 0x3f;

    int OFFS_REG(int reg) pure {
        return (reg >> 8) & REG_MASK; }

    enum OFFS_REG_MASK = REG_MASK << 8;

    int TO_OFFS_REG(int reg) pure {
        return reg << 8; }

    bool FAST_IS_REG(int reg) pure {
        return reg <= REG_MASK; }

    bool SLOW_IS_REG(int reg) pure {
        return reg > 0 && reg <= REG_MASK; }

    enum JUMP_LABEL = 0x1;
    enum JUMP_ADDR = 0x2;

    static if (SLJIT_CONFIG_X86) {
        enum PATCH_MB = 0x4;
        enum PATCH_MW = 0x8;
    } else static if (SLJIT_CONFIG_X86_64) {
        enum PATCH_MD = 0x10;
    } else static if (SLJIT_CONFIG_ARM_V5) {
        enum CPOOL_SIZE = 512;

        enum IS_BL = 0x4;
        enum PATCH_B = 0x8;
    } else static if (SLJIT_CONFIG_ARM_V7) {
        enum IS_BL = 0x4;
        enum PATCH_B = 0x8;
    } else static if (SLJIT_CONFIG_ARM_THUMB2) {
        enum IS_COND = 0x04;
        enum IS_BL = 0x08;
        enum PATCH_TYPE1 = 0x10;
        enum PATCH_TYPE2 = 0x20;
        enum PATCH_TYPE3 = 0x30;
        enum PATCH_TYPE4 = 0x40;
        enum PATCH_TYPE5 = 0x50;
        enum PATCH_BL = 0x60;
    } else static if (SLJIT_CONFIG_ARM_64) {
        enum IS_COND = 0x04;
        enum IS_CBZ = 0x008;
        enum IS_BL = 0x010;
        enum PATCH_B = 0x020;
        enum PATCH_COND = 0x040;
        enum PATCH_ABS48 = 0x080;
        enum PATCH_ABS64 = 0x100;
    } else static if (SLJIT_CONFIG_PPC) {
        enum IS_COND = 0x004;
        enum IS_CALL = 0x008;
        enum PATCH_B = 0x010;
        enum PATCH_ABS_B = 0x020;

        static if (SLJIT_CONFIG_PPC_64) {
            enum PATCH_ABS32 = 0x040;
            enum PATCH_ABS48 = 0x080;
        }

        enum REMOVE_COND = 0x100;
    } else static if (SLJIT_CONFIG_MIPS) {
        enum IS_MOVABLE = 0x004;
        enum IS_JAL = 0x008;
        enum IS_CALL = 0x010;
        enum IS_BIT26_COND = 0x020;
        enum IS_BIT16_COND = 0x040;

        enum IS_COND = IS_BIT26_COND | IS_BIT16_COND;

        enum PATCH_B = 0x080;
        enum PATCH_J = 0x100;

        static if (SLJIT_CONFIG_MIPS_64) {
            enum PATCH_ABS32 = 0x200;
            enum PATCH_ABS48 = 0x400;
        }

        enum MOVABLE_INS = 0;
        enum UNMOVABLE_INS = 32;
        enum FCSR_FCC = 33;
    } else static if (SLJIT_CONFIG_TILEGX) {
        enum IS_JAL = 0x04;
        enum IS_COND = 0x08;

        enum PATCH_B = 0x10;
        enum PATCH_J = 0x20;
    } else static if (SLJIT_CONFIG_SPARC_32) {
        enum IS_MOVABLE = 0x04;
        enum IS_COND = 0x08;
        enum IS_CALL = 0x10;

        enum PATCH_B = 0x20;
        enum PATCH_CALL = 0x40;

        enum MOVABLE_INS = 0;
        enum UNMOVABLE_INS = 32;

        enum DST_INS_MASK = 0xff;

        enum ICC_IS_SET = 1 << 23;
        enum FCC_IS_SET = 1 << 24;
    }

    void GET_SAVED_REGISTERS_SIZE(ARG1, ARG2, ARG3)(ARG1 scratches, ARG2 saveds, ARG3 extra) {  
        (((scratches < SLJIT_NUMBER_OF_SCRATCH_REGISTERS ? 0 : (scratches - SLJIT_NUMBER_OF_SCRATCH_REGISTERS)) + 
                (saveds < SLJIT_NUMBER_OF_SAVED_REGISTERS ? saveds : SLJIT_NUMBER_OF_SAVED_REGISTERS) + 
                extra) * sljit_sw.sizeof); }
    
    void ADJUST_LOCAL_OFFSET(sljit_sw p, sljit_sw i) {  
        if (p == (SLJIT_MEM1(SLJIT_SP))) 
            i += SLJIT_LOCALS_OFFSET; }
}

import sljitUtils;

static if (!SLJIT_CONFIG_UNSUPPORTED) {
    static if (SLJIT_EXECUTABLE_ALLOCATOR) {
        import sljitExecAllocator;
    }

    static if (SLJIT_ARGUMENT_CHECKS) {
        string CHECK_ARGUMENT(string x) pure {
            return `
                do {
                    if (SLJIT_UNLIKELY(!` ~ x ~ `))
                        return 1;
                } while(0);`;
        }

        alias CHECK_RETURN_TYPE = sljit_si;

        enum CHECK_RETURN_OK = `return 0;`;

        string CHECK(string x) pure {
            return `
                do {
                    if (SLJIT_UNLIKELY(` ~ x ~ `)) {
                        compiler.error = SLJIT_ERR_BAD_ARGUMENT;
                        return SLJIT_ERR_BAD_ARGUMENT;
                    }
                } while(0);`;
        }

        string CHECK_PTR(string x) pure {
            return `
                do {
                    if (SLJIT_UNLIKELY(` ~ x ~ `)) {
                        compiler.error = SLJIT_ERR_BAD_ARGUMENT;
                        return null;
                    }
                } while(0);`;
        }

        string CHECK_REG_INDEX(string x) pure {
            return `
                do {
                    if (SLJIT_UNLIKELY(` ~ x ~ `)) {
                        return -2;
                    }
                } while(0);`;
        }
    } else static if (SLJIT_DEBUG) {
        enum SLJIT_ARGUMENT_CHECKS = true;

        string CHECK_ARGUMENT(string x) pure {
            return "SLJIT_ASSERT(false);"; }

        alias CHECK_RETURN_TYPE = void;
        enum CHECK_RETURN_OK = `return;`;

        string CHECK(string x) pure {
            return x; }
        string CHECK_PTR(string x) pure {
            return x; }
        string CHECK_REG_INDEX(string x) pure {
            return x; }
    } else static if (SLJIT_VERBOSE) {
        string CHECK_ARGUMENT(string x) pure {
            return "SLJIT_ASSERT(false);"; }

        alias CHECK_RETURN_TYPE = void;
        enum CHECK_RETURN_OK = `return;`;
        
        string CHECK(string x) pure {
            return x; }
        string CHECK_PTR(string x) pure {
            return x; }
        string CHECK_REG_INDEX(string x) pure {
            return x; }
    } else {
        string CHECK(string x) pure {
            return ""; }
        string CHECK_PTR(string x) pure {
            return ""; }
        string CHECK_REG_INDEX(string x) pure {
            return ""; }
    }

    /* --------------------------------------------------------------------- */
    /*  Public functions                                                     */
    /* --------------------------------------------------------------------- */

    static if (SLJIT_CONFIG_ARM_V5 || SLJIT_CONFIG_X86) {
        enum SLJIT_NEEDS_COMPILER_INIT = true;
        __gshared sljit_si compiler_initialized = 0;
        void init_compiler();
    }

    sljit_compiler* sljit_create_compiler() {
        sljit_compiler *compiler = cast(sljit_compiler*)SLJIT_MALLOC(sljit_compiler.sizeof);
        if (!compiler)
            return null;
        SLJIT_ZEROMEM(compiler, sljit_compiler.sizeof);
        
        SLJIT_COMPILE_ASSERT(
            sljit_sb.sizeof == 1 && sljit_ub.sizeof == 1
            && sljit_sh.sizeof == 2 && sljit_uh.sizeof == 2
            && sljit_si.sizeof == 4 && sljit_ui.sizeof == 4
            && (sljit_p.sizeof == 4 || sljit_p.sizeof == 8)
            && sljit_p.sizeof <= sljit_sw.sizeof
            && (sljit_sw.sizeof == 4 || sljit_sw.sizeof == 8)
            && (sljit_uw.sizeof == 4 || sljit_uw.sizeof == 8),
            cast(char*)"invalid_integer_types");
        SLJIT_COMPILE_ASSERT(SLJIT_INT_OP == SLJIT_SINGLE_OP,
            cast(char*)"int_op_and_single_op_must_be_the_same");
        SLJIT_COMPILE_ASSERT(SLJIT_REWRITABLE_JUMP != SLJIT_SINGLE_OP,
            cast(char*)"rewritable_jump_and_single_op_must_not_be_the_same");
        
        /* Only the non-zero members must be set. */
        compiler.error = SLJIT_SUCCESS;
        
        compiler.buf = cast(sljit_memory_fragment*)SLJIT_MALLOC(BUF_SIZE);
        compiler.abuf = cast(sljit_memory_fragment*)SLJIT_MALLOC(ABUF_SIZE);
        
        if (!compiler.buf || !compiler.abuf) {
            if (compiler.buf)
                SLJIT_FREE(compiler.buf);
            if (compiler.abuf)
                SLJIT_FREE(compiler.abuf);
            SLJIT_FREE(compiler);
            return null;
        }
        
        compiler.buf.next = null;
        compiler.buf.used_size = 0;
        compiler.abuf.next = null;
        compiler.abuf.used_size = 0;
        
        compiler.scratches = -1;
        compiler.saveds = -1;
        compiler.fscratches = -1;
        compiler.fsaveds = -1;
        compiler.local_size = -1;
        
        static if (SLJIT_CONFIG_X86_32) {
            compiler.args = -1;
        }

        static if (SLJIT_CONFIG_ARM_V5) {
            compiler.cpool = cast(sljit_uw*)SLJIT_MALLOC(CPOOL_SIZE * sljit_uw.sizeof + CPOOL_SIZE * sljit_ub.sizeof);
            if (!compiler.cpool) {
                SLJIT_FREE(compiler.buf);
                SLJIT_FREE(compiler.abuf);
                SLJIT_FREE(compiler);
                return null;
            }
            compiler.cpool_unique = cast(sljit_ub*)(compiler.cpool + CPOOL_SIZE);
            compiler.cpool_diff = 0xffffffff;
        }
        
        static if (SLJIT_CONFIG_MIPS) {
            compiler.delay_slot = UNMOVABLE_INS;
        }
        
        static if (SLJIT_CONFIG_SPARC_32) {
            compiler.delay_slot = UNMOVABLE_INS;
        }

        static if (SLJIT_NEEDS_COMPILER_INIT) {
            if (!compiler_initialized) {
                init_compiler();
                compiler_initialized = 1;
            }
        }
        
        return compiler;
    }
}

void sljit_free_compiler(sljit_compiler *compiler)
{
    sljit_memory_fragment *buf;
    sljit_memory_fragment *curr;
    
    buf = compiler.buf;
    while (buf) {
        curr = buf;
        buf = buf.next;
        SLJIT_FREE(curr);
    }
    
    buf = compiler.abuf;
    while (buf) {
        curr = buf;
        buf = buf.next;
        SLJIT_FREE(curr);
    }
    
    static if (SLJIT_CONFIG_ARM_V5) {
        SLJIT_FREE(compiler.cpool);
    }
    SLJIT_FREE(compiler);
}

static if (SLJIT_CONFIG_ARM_THUMB2) {
    void sljit_free_code(void* code)
    {
        /* Resolve indirection. */
        code = cast(void*)(*cast(sljit_uw*)code);
        SLJIT_FREE_EXEC(code);
    }
} else static if (SLJIT_INDIRECT_CALL) {
    void sljit_free_code(void* code)
    {
        SLJIT_FREE_EXEC(code);
    }
}

void sljit_set_label(sljit_jump *jump, sljit_label* label)
{
    if (SLJIT_LIKELY(!!jump) && SLJIT_LIKELY(!!label)) {
        jump.flags &= ~JUMP_ADDR;
        jump.flags |= JUMP_LABEL;
        jump.u.label = label;
    }
}

void sljit_set_target(sljit_jump *jump, sljit_uw target)
{
    if (SLJIT_LIKELY(!!jump)) {
        jump.flags &= ~JUMP_LABEL;
        jump.flags |= JUMP_ADDR;
        jump.u.target = target;
    }
}

/* --------------------------------------------------------------------- */
/*  Private functions                                                    */
/* --------------------------------------------------------------------- */

void* ensure_buf(sljit_compiler *compiler, sljit_uw size)
{
    sljit_ub *ret;
    sljit_memory_fragment *new_frag;

    SLJIT_ASSERT(size <= 256);
    if (compiler.buf.used_size + size <= (BUF_SIZE - cast(sljit_uw)SLJIT_OFFSETOF!(sljit_memory_fragment, "memory"))) {
        ret = compiler.buf.memory + compiler.buf.used_size;
        compiler.buf.used_size += size;
        return ret;
    }
    new_frag = cast(sljit_memory_fragment*)SLJIT_MALLOC(BUF_SIZE);
    mixin(PTR_FAIL_IF_NULL("new_frag"));
    new_frag.next = compiler.buf;
    compiler.buf = new_frag;
    new_frag.used_size = size;
    return new_frag.memory;
}

void* ensure_abuf(sljit_compiler *compiler, sljit_uw size)
{
    sljit_ub *ret;
    sljit_memory_fragment *new_frag;

    SLJIT_ASSERT(size <= 256);
    if (compiler.abuf.used_size + size <= (ABUF_SIZE - cast(sljit_uw)SLJIT_OFFSETOF!(sljit_memory_fragment, "memory"))) {
        ret = compiler.abuf.memory + compiler.abuf.used_size;
        compiler.abuf.used_size += size;
        return ret;
    }
    new_frag = cast(sljit_memory_fragment*)SLJIT_MALLOC(ABUF_SIZE);
    mixin(PTR_FAIL_IF_NULL("new_frag"));
    new_frag.next = compiler.abuf;
    compiler.abuf = new_frag;
    new_frag.used_size = size;
    return new_frag.memory;
}

void* sljit_alloc_memory(sljit_compiler *compiler, sljit_si size)
{
    mixin(CHECK_ERROR_PTR);
    
    static if(SLJIT_64BIT_ARCHITECTURE) {
        if (size <= 0 || size > 128)
            return null;
        size = (size + 7) & ~7;
    } else {
        if (size <= 0 || size > 64)
            return null;
        size = (size + 3) & ~3;
    }
    return ensure_abuf(compiler, size);
}

void reverse_buf(sljit_compiler *compiler)
{
    sljit_memory_fragment *buf = compiler.buf;
    sljit_memory_fragment *prev = null;
    sljit_memory_fragment *tmp;
    
    do {
        tmp = buf.next;
        buf.next = prev;
        prev = buf;
        buf = tmp;
    } while (buf !is null);
    
    compiler.buf = prev;
}

void set_emit_enter(sljit_compiler *compiler,
    sljit_si options, sljit_si args, sljit_si scratches, sljit_si saveds,
    sljit_si fscratches, sljit_si fsaveds, sljit_si local_size)
{
    compiler.options = options;
    compiler.scratches = scratches;
    compiler.saveds = saveds;
    compiler.fscratches = fscratches;
    compiler.fsaveds = fsaveds;
    static if (SLJIT_ARGUMENT_CHECKS) {
        compiler.logical_local_size = local_size;
    }
}

void set_set_context(sljit_compiler *compiler,
    sljit_si options, sljit_si args, sljit_si scratches, sljit_si saveds,
    sljit_si fscratches, sljit_si fsaveds, sljit_si local_size)
{
    compiler.options = options;
    compiler.scratches = scratches;
    compiler.saveds = saveds;
    compiler.fscratches = fscratches;
    compiler.fsaveds = fsaveds;
    static if (SLJIT_ARGUMENT_CHECKS) {
        compiler.logical_local_size = local_size;
    }
}

void set_label(sljit_label *label, sljit_compiler *compiler)
{
    label.next = null;
    label.size = compiler.size;
    if (compiler.last_label)
        compiler.last_label.next = label;
    else
        compiler.labels = label;
    compiler.last_label = label;
}

void set_jump(sljit_jump *jump, sljit_compiler *compiler, sljit_si flags)
{
    jump.next = null;
    jump.flags = flags;
    if (compiler.last_jump)
        compiler.last_jump.next = jump;
    else
        compiler.jumps = jump;
    compiler.last_jump = jump;
}

void set_const(sljit_const *const_, sljit_compiler *compiler)
{
    const_.next = null;
    const_.addr = compiler.size;
    if (compiler.last_const)
        compiler.last_const.next = const_;
    else
        compiler.consts = const_;
    compiler.last_const = const_;
}

bool ADDRESSING_DEPENDS_ON(int exp, int reg) {
    return (exp & SLJIT_MEM) && ((exp & REG_MASK) == reg || OFFS_REG(exp) == reg);
}

static if (SLJIT_ARGUMENT_CHECKS) {
    enum FUNCTION_CHECKS_OP = q{
        mixin(CHECK_ARGUMENT("!GET_FLAGS(op) || !(op & SLJIT_FLAGS)"));
        switch(GET_OPCODE(op)) {
            case SLJIT_NOT:
            case SLJIT_CLZ:
            case SLJIT_AND:
            case SLJIT_OR:
            case SLJIT_XOR:
            case SLJIT_SHL:
            case SLJIT_LSHR:
            case SLJIT_ASHR:
                mixin(CHECK_ARGUMENT("!(op & (SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_O | SLJIT_SET_C))"));
                break;
            case SLJIT_NEG:
                mixin(CHECK_ARGUMENT("!(op & (SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_C))"));
                break;
            case SLJIT_MUL:
                mixin(CHECK_ARGUMENT("!(op & (SLJIT_SET_E | SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_C))"));
                break;
            case SLJIT_ADD:
                mixin(CHECK_ARGUMENT("!(op & (SLJIT_SET_U | SLJIT_SET_S))"));
                break;
            case SLJIT_SUB:
                break;
            case SLJIT_ADDC:
            case SLJIB_SUBC:
                mixin(CHECK_ARGUMENT("!(op & (SLJIT_SET_E | SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_O))"));
                break;
            case SLJIT_BREAKPOINT:
            case SLJIT_NOP:
            case SLJIT_LUMUL:
            case SLJIT_LSMUL:
            case SLJIT_MOV:
            case SLJIT_MOV_UI:
            case SLJIT_MOV_P:
            case SLJIT_MOVU:
            case SLJIT_MOVU_UI:
            case SLJIT_MOVU_P:
                mixin(CHECK_ARGUMENT("!(op & (SLJIT_INT_OP | SLJIT_SET_E | SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_O | SLJIT_SET_C | SLJIT_KEEP_FLAGS))"));
                break;
            default:
                mixin(CHECK_ARGUMENT("!(op & (SLJIT_SET_E | SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_O | SLJIT_SET_C | SLJIT_KEEP_FLAGS))"));
                break;
        }
    };

    enum FUNCTION_CHECK_FOP = q{
        mixin(CHECK_ARGUMENT("!GET_FLAGS(op) || !(op & SLJIT_KEEP_FLAGS)"));
        switch(GET_OPCODE(op)) {
            case SLJIT_DCMP:
                mixin(CHECK_ARGUMENT("!(op & (SLJIT_SET_U | SLJIT_SET_O | SLJIT_SET_C | SLJIT_KEEP_FLAGS))"));
                mixin(CHECK_ARGUMENT("(op & (SLJIT_SET_E | SLJIT_SET_S))"));
                break;
            default:
                mixin(CHECK_ARGUMENT("!(op & (SLJIT_SET_E | SLJIT_SET_U | SLJIT_SET_S | SLJIT_SET_O | SLJIT_SET_C | SLJIT_KEEP_FLAGS))"));
                break;
        }
    };

    string FUNCTION_CHECK_IS_REG(string r) pure {
        return `
(` ~ r ~ ` >= SLJIT_R0 && ` ~ r ~ ` < (SLJIT_R0 + compiler.scratches)) ||
(` ~ r ~ ` > (SLJIT_S0 - compiler.saveds) && ` ~ r ~ ` <= SLJIT_S0);`;
    }

    string FUNCTION_CHECK_IS_REG_OR_UNUSED(string r) pure {
        return `
` ~ r ~ ` == SLJIT_UNUSED ||
(` ~ r ~ ` >= SLJIT_R0 && ` ~ r ~ ` < (SLJIT_R0 + compiler.scratches)) ||
(` ~ r ~ ` > (SLJIT_S0 - compiler.saveds) && ` ~ r ~ ` <= SLJIT_S0);`;
    }

    static if (SLJIT_CONFIG_X86_32) {
        string CHECK_NOT_VIRTUAL_REGISTER(string p) pure {
            return `CHECK_ARGUMENT("` ~ p ~ ` < SLJIT_R3 || ` ~ p ~ ` > SLJIT_R6")`;
        }
    } else {
        string CHECK_NOT_VIRTUAL_REGISTER(string p) pure {
            return "{}";
        }
    }

    string FUNCTION_CHECK_SRC(string p, string i) pure {
        return `
` ~ CHECK_ARGUMENT("compiler.scratches != -1 && compiler.saveds != -1") ~ `
if (` ~ FUNCTION_CHECK_IS_REG(p) ~ `) {
    ` ~ CHECK_ARGUMENT(i ~ " == 0") ~ `
} else if (` ~ p ~ ` == SLJIT_IMM) {
} else if (` ~ p ~ ` == SLJIT_MEM1(SLJIT_SP)) {
    ` ~ CHECK_ARGUMENT(i ~ " >= 0 && " ~ i ~ " < compiler.logical_local_size") ~ `
} else {
    ` ~ CHECK_ARGUMENT(p ~ " & SLJIT_MEM") ~ `
    mixin(CHECK_ARGUMENT(FUNCTION_CHECK_IS_REG_OR_UNUSED(` ~ p ~ ` & REG_MASK)));
    ` ~ CHECK_NOT_VIRTUAL_REGISTER(p ~ " & REG_MASK") ~ `
    if (` ~ p ~ ` & OFFS_REG_MASK) {
        ` ~ CHECK_ARGUMENT("(" ~ p ~ " & REG_MASK) != SLJIT_UNUSED") ~ `
        mixin(CHECK_ARGUMENT(FUNCTION_CHECK_IS_REG(OFFS_REG(` ~ p ~ `))));
        mixin(CHECK_NOT_VIRTUAL_REGISTER(OFFS_REG(` ~ p ~ `)));
        ` ~ CHECK_ARGUMENT("!(" ~ i ~ " & ~0x3)") ~ `
    }
    ` ~ CHECK_ARGUMENT("!(" ~ p ~ " & ~(SLJIT_MEM | SLJIT_IMM | REG_MASK | OFFS_REG_MASK))") ~ `
}`;
    }

    string FUNCTION_CHECK_DST(string p, string i) pure {
        return `
` ~ CHECK_ARGUMENT("compiler.scratches != -1 && compiler.saveds != -1") ~ `
if (` ~ FUNCTION_CHECK_IS_REG_OR_UNUSED(p) ~ `) {
    ` ~ CHECK_ARGUMENT(i ~ " == 0") ~ `
} else if (` ~ p ~ ` == SLJIT_MEM1(SLJIT_SP)) {
    ` ~ CHECK_ARGUMENT(i ~ " >= 0 && " ~ i ~ " < compiler.logical_local_size") ~ `
} else {
    ` ~ CHECK_ARGUMENT(p ~ " & SLJIT_MEM") ~ `
    mixin(CHECK_ARGUMENT(FUNCTION_CHECK_IS_REG_OR_UNUSED(` ~ p ~ ` & REG_MASK)));
    ` ~ CHECK_NOT_VIRTUAL_REGISTER(p ~ " & REG_MASK") ~ `
    if (` ~ p ~ ` & OFFS_REG_MASK) {
        ` ~ CHECK_ARGUMENT("(" ~ p ~ " & REG_MASK) != SLJIT_UNUSED") ~ `
        mixin(CHECK_ARGUMENT(FUNCTION_CHECK_IS_REG(OFFS_REG(` ~ p ~ `))));
        mixin(CHECK_NOT_VIRTUAL_REGISTER(OFFS_REG(` ~ p ~ `)));
        ` ~ CHECK_ARGUMENT("!(" ~ i ~ " & ~0x3)") ~ `
    }
    ` ~ CHECK_ARGUMENT("!(" ~ p ~ " & ~(SLJIT_MEM | SLJIT_IMM | REG_MASK | OFFS_REG_MASK))") ~ `
}`;
    }

    string FUNCTION_FCHECK(string p, string i) pure {
        return `
` ~ CHECK_ARGUMENT("compiler.scratches != -1 && compiler.saveds != -1") ~ `

if ((` ~ p ~ ` >= SLJIT_FR0 && p < (SLJIT_FR0 + compiler.fscratches)) || (p > (SLJIT_FS0 - compiler.fsaveds) && p <= SLJIT_FS0)) {
    ` ~ CHECK_ARGUMENT(i ~ " == 0") ~ `
} else if (` ~ p ~ ` == SLJIT_MEM1(SLJIT_SP)) {
    ` ~ CHECK_ARGUMENT(i ~ " >= 0 && " ~ i ~ " < compiler.logical_local_size") ~ `
} else {
    ` ~ CHECK_ARGUMENT(p ~ " & SLJIT_MEM") ~ `
    mixin(CHECK_ARGUMENT(FUNCTION_CHECK_IS_REG_OR_UNUSED(` ~ p ~ ` & REG_MASK)));
    ` ~ CHECK_NOT_VIRTUAL_REGISTER(p ~ " & REG_MASK") ~ `
    if (` ~ p ~ ` & OFFS_REG_MASK) {
        ` ~ CHECK_ARGUMENT("(" ~ p ~ " & REG_MASK) != SLJIT_UNUSED") ~ `
        mixin(CHECK_ARGUMENT(FUNCTION_CHECK_IS_REG(OFFS_REG(` ~ p ~ `))));
        mixin(CHECK_NOT_VIRTUAL_REGISTER(OFFS_REG(` ~ p ~ `)));
        ` ~ CHECK_ARGUMENT("(" ~ p ~ " & OFFS_REG_MASK) != TO_OFFS_REG(SLJIT_SP) && !(i & ~0x3)") ~ `
    }
    ` ~ CHECK_ARGUMENT("!(" ~ p ~ " & ~(SLJIT_MEM | SLJIT_IMM | REG_MASK | OFFS_REG_MASK))") ~ `
}`;
    }

    enum FUNCTION_CHECK_OP1 = `
        if (GET_OPCODE(op) >= SLJIT_MOVU && GET_OPCODE(op) <= SLJIT_MOVU_P) {
            ` ~ CHECK_ARGUMENT("!(src & SLJIT_MEM) || (src & REG_MASK) != SLJIT_SP") ~ `
            ` ~ CHECK_ARGUMENT("!(dst & SLJIT_MEM) || (dst & REG_MASK) != SLJIT_SP") ~ `
            if ((src & SLJIT_MEM) && (src & REG_MASK))
                ` ~ CHECK_ARGUMENT("(dst & REG_MASK) != (src & REG_MASK) && OFFS_REG(dst) != (src & REG_MASK)") ~ `
        }
    `;
}

static if (SLJIT_VERBOSE) {
    void sljit_compiler_verbose(sljit_compiler* compiler, FILE* verbose) {
        compiler.verbose = verbose;
    }
}

static if (SLJIT_64BIT_ARCHITECTURE) {
    version(Windows) {
        enum SLJIT_PRINT_D = "I64";
    } else {
        enum SLJIT_PRINT_D = "l";
    }
} else {
    enum SLJIT_PRINT_D = "";
}

void sljit_verbose_reg(sljit_compiler* compiler, int r) {
    import core.stdc.stdio : fprintf;

    if (r < (SLJIT_R0 + compiler.scratches)) {
        fprintf(compiler.verbose, cast(char*)"r%d", r - SLJIT_R0);
    } else {
        fprintf(compiler.verbose, cast(char*)"s%d", SLJIT_NUMBER_OF_REGISTERS - r);
    }
}

void sljit_verbose_param(sljit_compiler* compiler, int p, int i) {
    import core.stdc.stdio;

    if (p & SLJIT_IMM) {
        enum FORMAT = "#%" ~ SLJIT_PRINT_D ~ "d";
        fprintf(compiler.verbose, cast(char*)FORMAT, i);
    } else if (p & SLJIT_MEM) {
        fputc('[', compiler.verbose);
        sljit_verbose_reg(compiler, p & REG_MASK);
        if (p & OFFS_REG_MASK) {
            fprintf(compiler.verbose, cast(char*)" + ");
            sljit_verbose_reg(compiler, OFFS_REG(p));
            if (i)
                fprintf(compiler.verbose, cast(char*)" * %d", 1 << i);
        } else if (i) {
            enum FORMAT = " + %" ~ SLJIT_PRINT_D ~ "d";
            fprintf(compiler.verbose, cast(char*)FORMAT, i);
        }
        fputc(']', compiler.verbose);
    } else if (p) {
        sljit_verbose_reg(compiler, p);
    } else {
        fprintf(compiler.verbose, cast(char*)"unused");
    }
}

void sljit_verbose_fparam(sljit_compiler* compiler, int p, int i) {
    import core.stdc.stdio;
    
    if (p & SLJIT_MEM) {
        if (p & REG_MASK) {
            fputc('[', compiler.verbose);
            sljit_verbose_reg(compiler, p & REG_MASK);
            if (p & OFFS_REG_MASK) {
                fprintf(compiler.verbose, cast(char*)" + ");
                sljit_verbose_reg(compiler, OFFS_REG(p));
                if (i)
                    fprintf(compiler.verbose, cast(char*)" * %d", 1 << i);
            } else if (i) {
                enum FORMAT = " + %" ~ SLJIT_PRINT_D ~ "d";
                fprintf(compiler.verbose, cast(char*)FORMAT, i);
            }
            fputc(']', compiler.verbose);
        } else {
            enum FORMAT = "[#%" ~ SLJIT_PRINT_D ~ "d]";
            fprintf(compiler.verbose, cast(char*)FORMAT, i);
        }
    } else {
        if (p < (SLJIT_FR0 + compiler.fscratches)) {
            fprintf(compiler.verbose, cast(char*)"fr%d", p - SLJIT_FR0);
        } else {
            fprintf(compiler.verbose, cast(char*)"fs%d", SLJIT_NUMBER_OF_FLOAT_REGISTERS - p);
        }
    }

    static const(char*)[] op0_names = [
        cast(char*)"breakpoint", cast(char*)"nop",
        cast(char*)"lumul", cast(char*)"lsmul", cast(char*)"ludiv", cast(char*)"lsdiv",
    ];
    
    static const(char*)[] op1_names = [
        cast(char*)"mov", cast(char*)"mov_ub", cast(char*)"mov_sb", cast(char*)"mov_uh",
        cast(char*)"mov_sh", cast(char*)"mov_ui", cast(char*)"mov_si", cast(char*)"mov_p",
        cast(char*)"movu", cast(char*)"movu_ub", cast(char*)"movu_sb", cast(char*)"movu_uh",
        cast(char*)"movu_sh", cast(char*)"movu_ui", cast(char*)"movu_si", cast(char*)"movu_p",
        cast(char*)"not", cast(char*)"neg", cast(char*)"clz",
    ];
    
    static const(char*)[] op2_names = [
        cast(char*)"add", cast(char*)"addc", cast(char*)"sub", cast(char*)"subc",
        cast(char*)"mul", cast(char*)"and", cast(char*)"or", cast(char*)"xor",
        cast(char*)"shl", cast(char*)"lshr", cast(char*)"ashr",
    ];
    
    static const(char*)[] fop1_names = [
        cast(char*)"mov", cast(char*)"conv", cast(char*)"conv", cast(char*)"conv",
        cast(char*)"conv", cast(char*)"conv", cast(char*)"cmp", cast(char*)"neg",
        cast(char*)"abs",
    ];
    
    static const(char*)[] fop2_names = [
        cast(char*)"add", cast(char*)"sub", cast(char*)"mul", cast(char*)"div"
    ];

    string JUMP_PREFIX(int type) pure {
        return (type & 0xff) <= SLJIT_MUL_NOT_OVERFLOW ? ((type & SLJIT_INT_OP) ? "i_" : "")
            : (type  & 0xff) <= SLJIT_D_ORDERED ? ((type & SLJIT_SINGLE_OP) ? "s_" : "d_") : "";
    }

    static char*[] jump_names = [
        cast(char*)"equal", cast(char*)"not_equal",
        cast(char*)"less", cast(char*)"greater_equal",
        cast(char*)"greater", cast(char*)"less_equal",
        cast(char*)"sig_less", cast(char*)"sig_greater_equal",
        cast(char*)"sig_greater", cast(char*)"sig_less_equal",
        cast(char*)"overflow", cast(char*)"not_overflow",
        cast(char*)"mul_overflow", cast(char*)"mul_not_overflow",
        cast(char*)"equal", cast(char*)"not_equal",
        cast(char*)"less", cast(char*)"greater_equal",
        cast(char*)"greater", cast(char*)"less_equal",
        cast(char*)"unordered", cast(char*)"ordered",
        cast(char*)"jump", cast(char*)"fast_call",
        cast(char*)"call0", cast(char*)"call1", cast(char*)"call2", cast(char*)"call3"
    ];
}

/* --------------------------------------------------------------------- */
/*  Arch dependent                                                       */
/* --------------------------------------------------------------------- */

static if (SLJIT_ARGUMENT_CHECKS || SLJIT_VERBOSE) {
    CHECK_RETURN_TYPE check_sljit_generate_code(sljit_compiler* compiler) {
        static if (SLJIT_ARGUMENT_CHECKS) {
            sljit_jump* jump;
        }

        static if (SLJIT_ARGUMENT_CHECKS) {
            mixin(CHECK_ARGUMENT("compiler.size > 0"));
            jump = compiler.jumps;
            while(jump !is null) {
                mixin(CHECK_ARGUMENT("jump.flags & (JUMP_LABEL | JUMP_ADDR)"));
                jump = jump.next;
            }
        }

        mixin(CHECK_RETURN_OK);
    }

    CHECK_RETURN_TYPE check_sljit_emit_enter(sljit_compiler* compiler,
        sljit_si options, sljit_si args, sljit_si scratches, sljit_si saveds,
        sljit_si fscratches, sljit_si fsaveds, sljit_si local_size) {

        // TODO

        mixin(CHECK_RETURN_OK);
    }
}
