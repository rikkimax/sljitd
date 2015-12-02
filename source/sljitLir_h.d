module sljitLir_h;
import sljitConfigInternal;

extern(C):

/* --------------------------------------------------------------------- */
/*  Error codes                                                          */
/* --------------------------------------------------------------------- */

/* Indicates no error. */
enum SLJIT_SUCCESS = 0;
/* After the call of sljit_generate_code(), the error code of the compiler
   is set to this value to avoid future sljit calls (in debug mode at least).
   The complier should be freed after sljit_generate_code(). */
enum SLJIT_ERR_COMPILED = 1;
/* Cannot allocate non executable memory. */
enum SLJIT_ERR_ALLOC_FAILED = 2;
/* Cannot allocate executable memory.
   Only for sljit_generate_code() */
enum SLJIT_ERR_EX_ALLOC_FAILED = 3;
/* Return value for SLJIT_CONFIG_UNSUPPORTED placeholder architecture. */
enum SLJIT_ERR_UNSUPPORTED = 4;
/* An ivalid argument is passed to any SLJIT function. */
enum SLJIT_ERR_BAD_ARGUMENT = 5;

/* --------------------------------------------------------------------- */
/*  Registers                                                            */
/* --------------------------------------------------------------------- */

/*
  Scratch (R) registers: registers whose may not preserve their values
  across function calls.

  Saved (S) registers: registers whose preserve their values across
  function calls.

  The scratch and saved register sets are overlap. The last scratch register
  is the first saved register, the one before the last is the second saved
  register, and so on.

  If an architecture provides two scratch and three saved registers,
  its scratch and saved register sets are the following:

     R0   |  [S4]  |   R0 and S4 represent the same physical register
     R1   |  [S3]  |   R1 and S3 represent the same physical register
    [R2]  |   S2   |   R2 and S2 represent the same physical register
    [R3]  |   S1   |   R3 and S1 represent the same physical register
    [R4]  |   S0   |   R4 and S0 represent the same physical register

  Note: SLJIT_NUMBER_OF_SCRATCH_REGISTERS would be 2 and
        SLJIT_NUMBER_OF_SAVED_REGISTERS would be 3 for this architecture.

  Note: On all supported architectures SLJIT_NUMBER_OF_REGISTERS >= 10
        and SLJIT_NUMBER_OF_SAVED_REGISTERS >= 5. However, 4 registers
        are virtual on x86-32. See below.

  The purpose of this definition is convenience. Although a register
  is either scratch register or saved register, SLJIT allows accessing
  them from the other set. For example, four registers can be used as
  scratch registers and the fifth one as saved register on the architecture
  above. Of course the last two scratch registers (R2 and R3) from this
  four will be saved on the stack, because they are defined as saved
  registers in the application binary interface. Still R2 and R3 can be
  used for referencing to these registers instead of S2 and S1, which
  makes easier to write platform independent code. Scratch registers
  can be saved registers in a similar way, but these extra saved
  registers will not be preserved across function calls! Hence the
  application must save them on those platforms, where the number of
  saved registers is too low. This can be done by copy them onto
  the stack and restore them after a function call.

  Note: To emphasize that registers assigned to R2-R4 are saved
        registers, they are enclosed by square brackets. S3-S4
        are marked in a similar way.

  Note: sljit_emit_enter and sljit_set_context defines whether a register
        is S or R register. E.g: when 3 scratches and 1 saved is mapped
        by sljit_emit_enter, the allowed register set will be: R0-R2 and
        S0. Although S2 is mapped to the same position as R2, it does not
        available in the current configuration. Furthermore the R3 (S1)
        register does not available as well.
*/

/* When SLJIT_UNUSED is specified as destination, the result is discarded. */
enum SLJIT_UNUSED = 0;

/* Scratch registers. */
enum SLJIT_R0 = 1;
enum SLJIT_R1 = 2;
enum SLJIT_R2 = 3;
/* Note: on x86-32, R3 - R6 (same as S3 - S6) are emulated (they
   are allocated on the stack). These registers are called virtual
   and cannot be used for memory addressing (cannot be part of
   any SLJIT_MEM1, SLJIT_MEM2 construct). There is no such
   limitation on other CPUs. See sljit_get_register_index(). */
enum SLJIT_R3 = 4;
enum SLJIT_R4 = 5;
enum SLJIT_R5 = 6;
enum SLJIT_R6 = 7;
enum SLJIT_R7 = 8;
enum SLJIT_R8 = 9;
enum SLJIT_R9 = 10;
/* All R registers provided by the architecture can be accessed by SLJIT_R(i)
   The i parameter must be >= 0 and < SLJIT_NUMBER_OF_REGISTERS. */
int SLJIT_R(int i) { return i + 1; }

/* Saved registers. */
enum SLJIT_S0 = (SLJIT_NUMBER_OF_REGISTERS);
enum SLJIT_S1 = (SLJIT_NUMBER_OF_REGISTERS - 1);
enum SLJIT_S2 = (SLJIT_NUMBER_OF_REGISTERS - 2);
/* Note: on x86-32, S3 - S6 (same as R3 - R6) are emulated (they
   are allocated on the stack). These registers are called virtual
   and cannot be used for memory addressing (cannot be part of
   any SLJIT_MEM1, SLJIT_MEM2 construct). There is no such
   limitation on other CPUs. See sljit_get_register_index(). */
enum SLJIT_S3 = (SLJIT_NUMBER_OF_REGISTERS - 3);
enum SLJIT_S4 = (SLJIT_NUMBER_OF_REGISTERS - 4);
enum SLJIT_S5 = (SLJIT_NUMBER_OF_REGISTERS - 5);
enum SLJIT_S6 = (SLJIT_NUMBER_OF_REGISTERS - 6);
enum SLJIT_S7 = (SLJIT_NUMBER_OF_REGISTERS - 7);
enum SLJIT_S8 = (SLJIT_NUMBER_OF_REGISTERS - 8);
enum SLJIT_S9 = (SLJIT_NUMBER_OF_REGISTERS - 9);
/* All S registers provided by the architecture can be accessed by SLJIT_S(i)
   The i parameter must be >= 0 and < SLJIT_NUMBER_OF_SAVED_REGISTERS. */
int SLJIT_S(int i) { return (SLJIT_NUMBER_OF_REGISTERS - (i)); }

/* Registers >= SLJIT_FIRST_SAVED_REG are saved registers. */
enum SLJIT_FIRST_SAVED_REG = (SLJIT_S0 - SLJIT_NUMBER_OF_SAVED_REGISTERS + 1);

/* The SLJIT_SP provides direct access to the linear stack space allocated by
   sljit_emit_enter. It can only be used in the following form: SLJIT_MEM1(SLJIT_SP).
   The immediate offset is extended by the relative stack offset automatically.
   The sljit_get_local_base can be used to obtain the absolute offset. */
enum SLJIT_SP = (SLJIT_NUMBER_OF_REGISTERS + 1);

/* Return with machine word. */

enum SLJIT_RETURN_REG = SLJIT_R0;

/* x86 prefers specific registers for special purposes. In case of shift
   by register it supports only SLJIT_R2 for shift argument
   (which is the src2 argument of sljit_emit_op2). If another register is
   used, sljit must exchange data between registers which cause a minor
   slowdown. Other architectures has no such limitation. */

enum SLJIT_PREF_SHIFT_REG = SLJIT_R2;

/* --------------------------------------------------------------------- */
/*  Floating point registers                                             */
/* --------------------------------------------------------------------- */

/* Each floating point register can store a double or single precision
   value. The FR and FS register sets are overlap in the same way as R
   and S register sets. See above. */

/* Note: SLJIT_UNUSED as destination is not valid for floating point
   operations, since they cannot be used for setting flags. */

/* Floating point scratch registers. */
enum SLJIT_FR0 = 1;
enum SLJIT_FR1 = 2;
enum SLJIT_FR2 = 3;
enum SLJIT_FR3 = 4;
enum SLJIT_FR4 = 5;
enum SLJIT_FR5 = 6;
/* All FR registers provided by the architecture can be accessed by SLJIT_FR(i)
   The i parameter must be >= 0 and < SLJIT_NUMBER_OF_FLOAT_REGISTERS. */
int SLJIT_FR(int i) { return (1 + (i)); }

/* Floating point saved registers. */
enum SLJIT_FS0 = (SLJIT_NUMBER_OF_FLOAT_REGISTERS);
enum SLJIT_FS1 = (SLJIT_NUMBER_OF_FLOAT_REGISTERS - 1);
enum SLJIT_FS2 = (SLJIT_NUMBER_OF_FLOAT_REGISTERS - 2);
enum SLJIT_FS3 = (SLJIT_NUMBER_OF_FLOAT_REGISTERS - 3);
enum SLJIT_FS4 = (SLJIT_NUMBER_OF_FLOAT_REGISTERS - 4);
enum SLJIT_FS5 = (SLJIT_NUMBER_OF_FLOAT_REGISTERS - 5);
/* All S registers provided by the architecture can be accessed by SLJIT_FS(i)
   The i parameter must be >= 0 and < SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS. */
int SLJIT_FS(int i) { return (SLJIT_NUMBER_OF_FLOAT_REGISTERS - (i)); }

/* Float registers >= SLJIT_FIRST_SAVED_FLOAT_REG are saved registers. */
enum SLJIT_FIRST_SAVED_FLOAT_REG = (SLJIT_FS0 - SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS + 1);

/* --------------------------------------------------------------------- */
/*  Main structures and functions                                        */
/* --------------------------------------------------------------------- */

/*
    The following structures are private, and can be changed in the
    future. Keeping them here allows code inlining.
*/

struct sljit_memory_fragment {
    sljit_memory_fragment *next;
    sljit_uw used_size;
    /* Must be aligned to sljit_sw. */
    sljit_ub* memory;
}

struct sljit_label {
    sljit_label *next;
    sljit_uw addr;
    /* The maximum size difference. */
    sljit_uw size;
}

struct sljit_jump {
    sljit_jump *next;
    sljit_uw addr;
    sljit_sw flags;
    union U {
        sljit_uw target;
        sljit_label* label;
    }
    U u;
}

struct sljit_const {
    sljit_const *next;
    sljit_uw addr;
}

struct sljit_compiler {
    sljit_si error;
    sljit_si options;
    
    sljit_label *labels;
    sljit_jump *jumps;
    sljit_const *consts;
    sljit_label *last_label;
    sljit_jump *last_jump;
    sljit_const *last_const;
    
    sljit_memory_fragment *buf;
    sljit_memory_fragment *abuf;
    
    /* Used scratch registers. */
    sljit_si scratches;
    /* Used saved registers. */
    sljit_si saveds;
    /* Used float scratch registers. */
    sljit_si fscratches;
    /* Used float saved registers. */
    sljit_si fsaveds;
    /* Local stack size. */
    sljit_si local_size;
    /* Code size. */
    sljit_uw size;
    /* For statistical purposes. */
    sljit_uw executable_size;
    
    static if (SLJIT_CONFIG_X86_32) {
        sljit_si args;
    }
    
    static if (SLJIT_CONFIG_X86_64) {
        sljit_si mode32;
    }
    
    static if (SLJIT_CONFIG_X86) {
        sljit_si flags_saved;
    }
    
    static if (SLJIT_CONFIG_ARM_V5) {
        /* Constant pool handling. */
        sljit_uw *cpool;
        sljit_ub *cpool_unique;
        sljit_uw cpool_diff;
        sljit_uw cpool_fill;
        /* Other members. */
        /* Contains pointer, "ldr pc, [...]" pairs. */
        sljit_uw patches;
    }

    static if (SLJIT_CONFIG_ARM_V5 || SLJIT_CONFIG_ARM_V7) {
        /* Temporary fields. */
        sljit_uw shift_imm;
        sljit_si cache_arg;
        sljit_sw cache_argw;
    }
    
    static if (SLJIT_CONFIG_ARM_THUMB2) {
        sljit_si cache_arg;
        sljit_sw cache_argw;
    }
    
    static if (SLJIT_CONFIG_ARM_64) {
        sljit_si cache_arg;
        sljit_sw cache_argw;
    }
    static if (SLJIT_CONFIG_PPC) {
        sljit_sw imm;
        sljit_si cache_arg;
        sljit_sw cache_argw;
    }
    
    static if (SLJIT_CONFIG_MIPS) {
        sljit_si delay_slot;
        sljit_si cache_arg;
        sljit_sw cache_argw;
    }
    
    static if (SLJIT_CONFIG_SPARC_32) {
        sljit_si delay_slot;
        sljit_si cache_arg;
        sljit_sw cache_argw;
    }

    static if (SLJIT_CONFIG_TILEGX) {
        sljit_si cache_arg;
        sljit_sw cache_argw;
    }

    static if (SLJIT_VERBOSE) {
        import core.stdc.stdio : FILE;
        FILE* verbose;
    }

    static if (SLJIT_ARGUMENT_CHECKS || SLJIT_DEBUG) {
        /* Local size passed to the functions. */
        sljit_si logical_local_size;
    }

    static if (SLJIT_ARGUMENT_CHECKS || SLJIT_DEBUG || SLJIT_VERBOSE) {
        sljit_si skip_checks;
    }
}

/* --------------------------------------------------------------------- */
/*  Main functions                                                       */
/* --------------------------------------------------------------------- */

/* Creates an sljit compiler.
   Returns NULL if failed. */
sljit_compiler* sljit_create_compiler();

/* Free everything except the compiled machine code. */
void sljit_free_compiler(sljit_compiler *compiler);

/* Returns the current error code. If an error is occurred, future sljit
   calls which uses the same compiler argument returns early with the same
   error code. Thus there is no need for checking the error after every
   call, it is enough to do it before the code is compiled. Removing
   these checks increases the performance of the compiling process. */
sljit_si sljit_get_compiler_error(sljit_compiler *compiler) { return compiler.error; }

/*
   Allocate a small amount of memory. The size must be <= 64 bytes on 32 bit,
   and <= 128 bytes on 64 bit architectures. The memory area is owned by the
   compiler, and freed by sljit_free_compiler. The returned pointer is
   sizeof(sljit_sw) aligned. Excellent for allocating small blocks during
   the compiling, and no need to worry about freeing them. The size is
   enough to contain at most 16 pointers. If the size is outside of the range,
   the function will return with NULL. However, this return value does not
   indicate that there is no more memory (does not set the current error code
   of the compiler to out-of-memory status).
*/
void* sljit_alloc_memory(sljit_compiler *compiler, sljit_si size);

static if (SLJIT_VERBOSE) {
    import core.stdc.stdio : FILE;
    /* Passing NULL disables verbose. */
    void sljit_compiler_verbose(sljit_compiler *compiler, FILE* verbose);
}

void* sljit_generate_code(sljit_compiler *compiler);
void sljit_free_code(void* code);

/*
   After the machine code generation is finished we can retrieve the allocated
   executable memory size, although this area may not be fully filled with
   instructions depending on some optimizations. This function is useful only
   for statistical purposes.

   Before a successful code generation, this function returns with 0.
*/
sljit_uw sljit_get_generated_code_size(sljit_compiler *compiler) { return compiler.executable_size; }

/* Instruction generation. Returns with any error code. If there is no
   error, they return with SLJIT_SUCCESS. */

/*
   The executable code is a function call from the viewpoint of the C
   language. The function calls must obey to the ABI (Application
   Binary Interface) of the platform, which specify the purpose of
   all machine registers and stack handling among other things. The
   sljit_emit_enter function emits the necessary instructions for
   setting up a new context for the executable code and moves function
   arguments to the saved registers. Furthermore the options argument
   can be used to pass configuration options to the compiler. The
   available options are listed before sljit_emit_enter.

   The number of sljit_sw arguments passed to the generated function
   are specified in the "args" parameter. The number of arguments must
   be less than or equal to 3. The first argument goes to SLJIT_S0,
   the second goes to SLJIT_S1 and so on. The register set used by
   the function must be declared as well. The number of scratch and
   saved registers used by the function must be passed to sljit_emit_enter.
   Only R registers between R0 and "scratches" argument can be used
   later. E.g. if "scratches" is set to 2, the register set will be
   limited to R0 and R1. The S registers and the floating point
   registers ("fscratches" and "fsaveds") are specified in a similar
   way. The sljit_emit_enter is also capable of allocating a stack
   space for local variables. The "local_size" argument contains the
   size in bytes of this local area and its staring address is stored
   in SLJIT_SP. The memory area between SLJIT_SP (inclusive) and
   SLJIT_SP + local_size (exclusive) can be modified freely until
   the function returns. The stack space is not initialized.

   Note: the following conditions must met:
         0 <= scratches <= SLJIT_NUMBER_OF_REGISTERS
         0 <= saveds <= SLJIT_NUMBER_OF_REGISTERS
         scratches + saveds <= SLJIT_NUMBER_OF_REGISTERS
         0 <= fscratches <= SLJIT_NUMBER_OF_FLOAT_REGISTERS
         0 <= fsaveds <= SLJIT_NUMBER_OF_FLOAT_REGISTERS
         fscratches + fsaveds <= SLJIT_NUMBER_OF_FLOAT_REGISTERS

   Note: every call of sljit_emit_enter and sljit_set_context
         overwrites the previous context.
*/

/* The absolute address returned by sljit_get_local_base with
offset 0 is aligned to sljit_d. Otherwise it is aligned to sljit_uw. */
enum SLJIT_DOUBLE_ALIGNMENT = 0x00000001;

/* The local_size must be >= 0 and <= SLJIT_MAX_LOCAL_SIZE. */
enum SLJIT_MAX_LOCAL_SIZE = 65536;

sljit_si sljit_emit_enter(sljit_compiler *compiler,
    sljit_si options, sljit_si args, sljit_si scratches, sljit_si saveds,
    sljit_si fscratches, sljit_si fsaveds, sljit_si local_size);

/* The machine code has a context (which contains the local stack space size,
   number of used registers, etc.) which initialized by sljit_emit_enter. Several
   functions (like sljit_emit_return) requres this context to be able to generate
   the appropriate code. However, some code fragments (like inline cache) may have
   no normal entry point so their context is unknown for the compiler. Their context
   can be provided to the compiler by the sljit_set_context function.

   Note: every call of sljit_emit_enter and sljit_set_context overwrites
         the previous context. */

sljit_si sljit_set_context(sljit_compiler *compiler,
    sljit_si options, sljit_si args, sljit_si scratches, sljit_si saveds,
    sljit_si fscratches, sljit_si fsaveds, sljit_si local_size);

/* Return from machine code.  The op argument can be SLJIT_UNUSED which means the
   function does not return with anything or any opcode between SLJIT_MOV and
   SLJIT_MOV_P (see sljit_emit_op1). As for src and srcw they must be 0 if op
   is SLJIT_UNUSED, otherwise see below the description about source and
   destination arguments. */

sljit_si sljit_emit_return(sljit_compiler *compiler, sljit_si op,
    sljit_si src, sljit_sw srcw);

/* Fast calling mechanism for utility functions (see SLJIT_FAST_CALL). All registers and
   even the stack frame is passed to the callee. The return address is preserved in
   dst/dstw by sljit_emit_fast_enter (the type of the value stored by this function
   is sljit_p), and sljit_emit_fast_return can use this as a return value later. */

/* Note: only for sljit specific, non ABI compilant calls. Fast, since only a few machine
   instructions are needed. Excellent for small uility functions, where saving registers
   and setting up a new stack frame would cost too much performance. However, it is still
   possible to return to the address of the caller (or anywhere else). */

/* Note: flags are not changed (unlike sljit_emit_enter / sljit_emit_return). */

/* Note: although sljit_emit_fast_return could be replaced by an ijump, it is not suggested,
   since many architectures do clever branch prediction on call / return instruction pairs. */

sljit_si sljit_emit_fast_enter(sljit_compiler *compiler, sljit_si dst, sljit_sw dstw);
sljit_si sljit_emit_fast_return(sljit_compiler *compiler, sljit_si src, sljit_sw srcw);

/*
   Source and destination values for arithmetical instructions
    imm              - a simple immediate value (cannot be used as a destination)
    reg              - any of the registers (immediate argument must be 0)
    [imm]            - absolute immediate memory address
    [reg+imm]        - indirect memory address
    [reg+(reg<<imm)] - indirect indexed memory address (shift must be between 0 and 3)
                       useful for (byte, half, int, sljit_sw) array access
                       (fully supported by both x86 and ARM architectures, and cheap operation on others)
*/

/*
   IMPORATNT NOTE: memory access MUST be naturally aligned except
                   SLJIT_UNALIGNED macro is defined and its value is 1.

     length | alignment
   ---------+-----------
     byte   | 1 byte (any physical_address is accepted)
     half   | 2 byte (physical_address & 0x1 == 0)
     int    | 4 byte (physical_address & 0x3 == 0)
     word   | 4 byte if SLJIT_32BIT_ARCHITECTURE is defined and its value is 1
            | 8 byte if SLJIT_64BIT_ARCHITECTURE is defined and its value is 1
    pointer | size of sljit_p type (4 byte on 32 bit machines, 4 or 8 byte
            | on 64 bit machines)

   Note:   Different architectures have different addressing limitations.
           A single instruction is enough for the following addressing
           modes. Other adrressing modes are emulated by instruction
           sequences. This information could help to improve those code
           generators which focuses only a few architectures.

   x86:    [reg+imm], -2^32+1 <= imm <= 2^32-1 (full address space on x86-32)
           [reg+(reg<<imm)] is supported
           [imm], -2^32+1 <= imm <= 2^32-1 is supported
           Write-back is not supported
   arm:    [reg+imm], -4095 <= imm <= 4095 or -255 <= imm <= 255 for signed
                bytes, any halfs or floating point values)
           [reg+(reg<<imm)] is supported
           Write-back is supported
   arm-t2: [reg+imm], -255 <= imm <= 4095
           [reg+(reg<<imm)] is supported
           Write back is supported only for [reg+imm], where -255 <= imm <= 255
   ppc:    [reg+imm], -65536 <= imm <= 65535. 64 bit loads/stores and 32 bit
                signed load on 64 bit requires immediates divisible by 4.
                [reg+imm] is not supported for signed 8 bit values.
           [reg+reg] is supported
           Write-back is supported except for one instruction: 32 bit signed
                load with [reg+imm] addressing mode on 64 bit.
   mips:   [reg+imm], -65536 <= imm <= 65535
   sparc:  [reg+imm], -4096 <= imm <= 4095
           [reg+reg] is supported
*/

/* Register output: simply the name of the register.
   For destination, you can use SLJIT_UNUSED as well. */
enum SLJIT_MEM = 0x80;
int SLJIT_MEM0() { return (SLJIT_MEM); }
int SLJIT_MEM1(int r1) { return (SLJIT_MEM | (r1)); }
int SLJIT_MEM2(int r1, int r2) { return (SLJIT_MEM | (r1) | ((r2) << 8)); }
enum SLJIT_IMM = 0x40;

/* Set 32 bit operation mode (I) on 64 bit CPUs. The flag is totally ignored on
   32 bit CPUs. If this flag is set for an arithmetic operation, it uses only the
   lower 32 bit of the input register(s), and set the CPU status flags according
   to the 32 bit result. The higher 32 bits are undefined for both the input and
   output. However, the CPU might not ignore those higher 32 bits, like MIPS, which
   expects it to be the sign extension of the lower 32 bit. All 32 bit operations
   are undefined, if this condition is not fulfilled. Therefore, when SLJIT_INT_OP
   is specified, all register arguments must be the result of other operations with
   the same SLJIT_INT_OP flag. In other words, although a register can hold either
   a 64 or 32 bit value, these values cannot be mixed. The only exceptions are
   SLJIT_IMOV and SLJIT_IMOVU (SLJIT_MOV_SI/SLJIT_MOVU_SI with SLJIT_INT_OP flag)
   which can convert any source argument to SLJIT_INT_OP compatible result. This
   conversion might be unnecessary on some CPUs like x86-64, since the upper 32
   bit is always ignored. In this case SLJIT is clever enough to not generate any
   instructions if the source and destination operands are the same registers.
   Affects sljit_emit_op0, sljit_emit_op1 and sljit_emit_op2. */
enum SLJIT_INT_OP = 0x100;

/* Single precision mode (SP). This flag is similar to SLJIT_INT_OP, just
   it applies to floating point registers (it is even the same bit). When
   this flag is passed, the CPU performs single precision floating point
   operations. Similar to SLJIT_INT_OP, all register arguments must be the
   result of other floating point operations with this flag. Affects
   sljit_emit_fop1, sljit_emit_fop2 and sljit_emit_fcmp. */
enum SLJIT_SINGLE_OP = 0x100;

/* Common CPU status flags for all architectures (x86, ARM, PPC)
    - carry flag
    - overflow flag
    - zero flag
    - negative/positive flag (depends on arc)
   On mips, these flags are emulated by software. */

/* By default, the instructions may, or may not set the CPU status flags.
   Forcing to set or keep status flags can be done with the following flags: */

/* Note: sljit tries to emit the minimum number of instructions. Using these
   flags can increase them, so use them wisely to avoid unnecessary code generation. */

/* Set Equal (Zero) status flag (E). */
enum SLJIT_SET_E = 0x0200;
/* Set unsigned status flag (U). */
enum SLJIT_SET_U = 0x0400;
/* Set signed status flag (S). */
enum SLJIT_SET_S = 0x0800;
/* Set signed overflow flag (O). */
enum SLJIT_SET_O = 0x1000;
/* Set carry flag (C).
   Note: Kinda unsigned overflow, but behaves differently on various cpus. */
enum SLJIT_SET_C = 0x2000;
/* Do not modify the flags (K).
   Note: This flag cannot be combined with any other SLJIT_SET_* flag. */
enum SLJIT_KEEP_FLAGS = 0x4000;

/* Notes:
     - you cannot postpone conditional jump instructions except if noted that
       the instruction does not set flags (See: SLJIT_KEEP_FLAGS).
     - flag combinations: '|' means 'logical or'. */

/* Starting index of opcodes for sljit_emit_op0. */
enum SLJIT_OP0_BASE = 0;

/* Flags: - (never set any flags)
   Note: breakpoint instruction is not supported by all architectures (namely ppc)
         It falls back to SLJIT_NOP in those cases. */
enum SLJIT_BREAKPOINT = (SLJIT_OP0_BASE + 0);
/* Flags: - (never set any flags)
   Note: may or may not cause an extra cycle wait
         it can even decrease the runtime in a few cases. */
enum SLJIT_NOP = (SLJIT_OP0_BASE + 1);
/* Flags: - (may destroy flags)
   Unsigned multiplication of SLJIT_R0 and SLJIT_R1.
   Result goes to SLJIT_R1:SLJIT_R0 (high:low) word */
enum SLJIT_LUMUL = (SLJIT_OP0_BASE + 2);
/* Flags: - (may destroy flags)
   Signed multiplication of SLJIT_R0 and SLJIT_R1.
   Result goes to SLJIT_R1:SLJIT_R0 (high:low) word */
enum SLJIT_LSMUL = (SLJIT_OP0_BASE + 3);
/* Flags: I - (may destroy flags)
   Unsigned divide of the value in SLJIT_R0 by the value in SLJIT_R1.
   The result is placed in SLJIT_R0 and the remainder goes to SLJIT_R1.
   Note: if SLJIT_R1 contains 0, the behaviour is undefined. */
enum SLJIT_LUDIV = (SLJIT_OP0_BASE + 4);
enum SLJIT_ILUDIV = (SLJIT_LUDIV | SLJIT_INT_OP);
/* Flags: I - (may destroy flags)
   Signed divide of the value in SLJIT_R0 by the value in SLJIT_R1.
   The result is placed in SLJIT_R0 and the remainder goes to SLJIT_R1.
   Note: if SLJIT_R1 contains 0, the behaviour is undefined. */
enum SLJIT_LSDIV = (SLJIT_OP0_BASE + 5);
enum SLJIT_ILSDIV = (SLJIT_LSDIV | SLJIT_INT_OP);

sljit_si sljit_emit_op0(sljit_compiler *compiler, sljit_si op);

/* Starting index of opcodes for sljit_emit_op1. */
enum SLJIT_OP1_BASE = 32;

/* Notes for MOV instructions:
   U = Mov with update (pre form). If source or destination defined as SLJIT_MEM1(r1)
       or SLJIT_MEM2(r1, r2), r1 is increased by the sum of r2 and the constant argument
   UB = unsigned byte (8 bit)
   SB = signed byte (8 bit)
   UH = unsigned half (16 bit)
   SH = signed half (16 bit)
   UI = unsigned int (32 bit)
   SI = signed int (32 bit)
   P  = pointer (sljit_p) size */

/* Flags: - (never set any flags) */
enum SLJIT_MOV = (SLJIT_OP1_BASE + 0);
/* Flags: I - (never set any flags) */
enum SLJIT_MOV_UB = (SLJIT_OP1_BASE + 1);
enum SLJIT_IMOV_UB = (SLJIT_MOV_UB | SLJIT_INT_OP);
/* Flags: I - (never set any flags) */
enum SLJIT_MOV_SB = (SLJIT_OP1_BASE + 2);
enum SLJIT_IMOV_SB = (SLJIT_MOV_SB | SLJIT_INT_OP);
/* Flags: I - (never set any flags) */
enum SLJIT_MOV_UH = (SLJIT_OP1_BASE + 3);
enum SLJIT_IMOV_UH = (SLJIT_MOV_UH | SLJIT_INT_OP);
/* Flags: I - (never set any flags) */
enum SLJIT_MOV_SH = (SLJIT_OP1_BASE + 4);
enum SLJIT_IMOV_SH = (SLJIT_MOV_SH | SLJIT_INT_OP);
/* Flags: I - (never set any flags)
   Note: see SLJIT_INT_OP for further details. */
enum SLJIT_MOV_UI = (SLJIT_OP1_BASE + 5);
/* No SLJIT_INT_OP form, since it is the same as SLJIT_IMOV. */
/* Flags: I - (never set any flags)
   Note: see SLJIT_INT_OP for further details. */
enum SLJIT_MOV_SI = (SLJIT_OP1_BASE + 6);
enum SLJIT_IMOV = (SLJIT_MOV_SI | SLJIT_INT_OP);
/* Flags: - (never set any flags) */
enum SLJIT_MOV_P = (SLJIT_OP1_BASE + 7);
/* Flags: - (never set any flags) */
enum SLJIT_MOVU = (SLJIT_OP1_BASE + 8);
/* Flags: I - (never set any flags) */
enum SLJIT_MOVU_UB = (SLJIT_OP1_BASE + 9);
enum SLJIT_IMOVU_UB = (SLJIT_MOVU_UB | SLJIT_INT_OP);
/* Flags: I - (never set any flags) */
enum SLJIT_MOVU_SB = (SLJIT_OP1_BASE + 10);
enum SLJIT_IMOVU_SB = (SLJIT_MOVU_SB | SLJIT_INT_OP);
/* Flags: I - (never set any flags) */
enum SLJIT_MOVU_UH = (SLJIT_OP1_BASE + 11);
enum SLJIT_IMOVU_UH = (SLJIT_MOVU_UH | SLJIT_INT_OP);
/* Flags: I - (never set any flags) */
enum SLJIT_MOVU_SH = (SLJIT_OP1_BASE + 12);
enum SLJIT_IMOVU_SH = (SLJIT_MOVU_SH | SLJIT_INT_OP);
/* Flags: I - (never set any flags)
   Note: see SLJIT_INT_OP for further details. */
enum SLJIT_MOVU_UI = (SLJIT_OP1_BASE + 13);
/* No SLJIT_INT_OP form, since it is the same as SLJIT_IMOVU. */
/* Flags: I - (never set any flags)
   Note: see SLJIT_INT_OP for further details. */
enum SLJIT_MOVU_SI = (SLJIT_OP1_BASE + 14);
enum SLJIT_IMOVU = (SLJIT_MOVU_SI | SLJIT_INT_OP);
/* Flags: - (never set any flags) */
enum SLJIT_MOVU_P = (SLJIT_OP1_BASE + 15);
/* Flags: I | E | K */
enum SLJIT_NOT = (SLJIT_OP1_BASE + 16);
enum SLJIT_INOT = (SLJIT_NOT | SLJIT_INT_OP);
/* Flags: I | E | O | K */
enum SLJIT_NEG = (SLJIT_OP1_BASE + 17);
enum SLJIT_INEG = (SLJIT_NEG | SLJIT_INT_OP);
/* Count leading zeroes
   Flags: I | E | K
   Important note! Sparc 32 does not support K flag, since
   the required popc instruction is introduced only in sparc 64. */
enum SLJIT_CLZ = (SLJIT_OP1_BASE + 18);
enum SLJIT_ICLZ = (SLJIT_CLZ | SLJIT_INT_OP);

sljit_si sljit_emit_op1(sljit_compiler *compiler, sljit_si op,
    sljit_si dst, sljit_sw dstw,
    sljit_si src, sljit_sw srcw);

/* Starting index of opcodes for sljit_emit_op2. */
enum SLJIT_OP2_BASE = 96;

/* Flags: I | E | O | C | K */
enum SLJIT_ADD = (SLJIT_OP2_BASE + 0);
enum SLJIT_IADD = (SLJIT_ADD | SLJIT_INT_OP);
/* Flags: I | C | K */
enum SLJIT_ADDC = (SLJIT_OP2_BASE + 1);
enum SLJIT_IADDC = (SLJIT_ADDC | SLJIT_INT_OP);
/* Flags: I | E | U | S | O | C | K */
enum SLJIT_SUB = (SLJIT_OP2_BASE + 2);
enum SLJIT_ISUB = (SLJIT_SUB | SLJIT_INT_OP);
/* Flags: I | C | K */
enum SLJIT_SUBC = (SLJIT_OP2_BASE + 3);
enum SLJIT_ISUBC = (SLJIT_SUBC | SLJIT_INT_OP);
/* Note: integer mul
   Flags: I | O (see SLJIT_C_MUL_*) | K */
enum SLJIT_MUL = (SLJIT_OP2_BASE + 4);
enum SLJIT_IMUL = (SLJIT_MUL | SLJIT_INT_OP);
/* Flags: I | E | K */
enum SLJIT_AND = (SLJIT_OP2_BASE + 5);
enum SLJIT_IAND = (SLJIT_AND | SLJIT_INT_OP);
/* Flags: I | E | K */
enum SLJIT_OR = (SLJIT_OP2_BASE + 6);
enum SLJIT_IOR = (SLJIT_OR | SLJIT_INT_OP);
/* Flags: I | E | K */
enum SLJIT_XOR = (SLJIT_OP2_BASE + 7);
enum SLJIT_IXOR = (SLJIT_XOR | SLJIT_INT_OP);
/* Flags: I | E | K
   Let bit_length be the length of the shift operation: 32 or 64.
   If src2 is immediate, src2w is masked by (bit_length - 1).
   Otherwise, if the content of src2 is outside the range from 0
   to bit_length - 1, the result is undefined. */
enum SLJIT_SHL = (SLJIT_OP2_BASE + 8);
enum SLJIT_ISHL = (SLJIT_SHL | SLJIT_INT_OP);
/* Flags: I | E | K
   Let bit_length be the length of the shift operation: 32 or 64.
   If src2 is immediate, src2w is masked by (bit_length - 1).
   Otherwise, if the content of src2 is outside the range from 0
   to bit_length - 1, the result is undefined. */
enum SLJIT_LSHR = (SLJIT_OP2_BASE + 9);
enum SLJIT_ILSHR = (SLJIT_LSHR | SLJIT_INT_OP);
/* Flags: I | E | K
   Let bit_length be the length of the shift operation: 32 or 64.
   If src2 is immediate, src2w is masked by (bit_length - 1).
   Otherwise, if the content of src2 is outside the range from 0
   to bit_length - 1, the result is undefined. */
enum SLJIT_ASHR = (SLJIT_OP2_BASE + 10);
enum SLJIT_IASHR = (SLJIT_ASHR | SLJIT_INT_OP);

sljit_si sljit_emit_op2(sljit_compiler *compiler, sljit_si op,
    sljit_si dst, sljit_sw dstw,
    sljit_si src1, sljit_sw src1w,
    sljit_si src2, sljit_sw src2w);

/* The following function is a helper function for sljit_emit_op_custom.
   It returns with the real machine register index ( >=0 ) of any SLJIT_R,
   SLJIT_S and SLJIT_SP registers.

   Note: it returns with -1 for virtual registers (only on x86-32). */

sljit_si sljit_get_register_index(sljit_si reg);

/* The following function is a helper function for sljit_emit_op_custom.
   It returns with the real machine register index of any SLJIT_FLOAT register.

   Note: the index is always an even number on ARM (except ARM-64), MIPS, and SPARC. */

sljit_si sljit_get_float_register_index(sljit_si reg);

/* Any instruction can be inserted into the instruction stream by
   sljit_emit_op_custom. It has a similar purpose as inline assembly.
   The size parameter must match to the instruction size of the target
   architecture:

         x86: 0 < size <= 15. The instruction argument can be byte aligned.
      Thumb2: if size == 2, the instruction argument must be 2 byte aligned.
              if size == 4, the instruction argument must be 4 byte aligned.
   Otherwise: size must be 4 and instruction argument must be 4 byte aligned. */

sljit_si sljit_emit_op_custom(sljit_compiler *compiler,
    void *instruction, sljit_si size);

/* Returns with non-zero if fpu is available. */

sljit_si sljit_is_fpu_available();

/* Starting index of opcodes for sljit_emit_fop1. */
enum SLJIT_FOP1_BASE = 128;

/* Flags: SP - (never set any flags) */
enum SLJIT_DMOV = (SLJIT_FOP1_BASE + 0);
enum SLJIT_SMOV = (SLJIT_DMOV | SLJIT_SINGLE_OP);
/* Convert opcodes: CONV[DST_TYPE].FROM[SRC_TYPE]
   SRC/DST TYPE can be: D - double, S - single, W - signed word, I - signed int
   Rounding mode when the destination is W or I: round towards zero. */
/* Flags: SP - (never set any flags) */
enum SLJIT_CONVD_FROMS = (SLJIT_FOP1_BASE + 1);
enum SLJIT_CONVS_FROMD = (SLJIT_CONVD_FROMS | SLJIT_SINGLE_OP);
/* Flags: SP - (never set any flags) */
enum SLJIT_CONVW_FROMD = (SLJIT_FOP1_BASE + 2);
enum SLJIT_CONVW_FROMS = (SLJIT_CONVW_FROMD | SLJIT_SINGLE_OP);
/* Flags: SP - (never set any flags) */
enum SLJIT_CONVI_FROMD = (SLJIT_FOP1_BASE + 3);
enum SLJIT_CONVI_FROMS = (SLJIT_CONVI_FROMD | SLJIT_SINGLE_OP);
/* Flags: SP - (never set any flags) */
enum SLJIT_CONVD_FROMW = (SLJIT_FOP1_BASE + 4);
enum SLJIT_CONVS_FROMW = (SLJIT_CONVD_FROMW | SLJIT_SINGLE_OP);
/* Flags: SP - (never set any flags) */
enum SLJIT_CONVD_FROMI = (SLJIT_FOP1_BASE + 5);
enum SLJIT_CONVS_FROMI = (SLJIT_CONVD_FROMI | SLJIT_SINGLE_OP);
/* Note: dst is the left and src is the right operand for SLJIT_CMPD.
   Note: NaN check is always performed. If SLJIT_C_FLOAT_UNORDERED flag
         is set, the comparison result is unpredictable.
   Flags: SP | E | S (see SLJIT_C_FLOAT_*) */
enum SLJIT_DCMP = (SLJIT_FOP1_BASE + 6);
enum SLJIT_SCMP = (SLJIT_DCMP | SLJIT_SINGLE_OP);
/* Flags: SP - (never set any flags) */
enum SLJIT_DNEG = (SLJIT_FOP1_BASE + 7);
enum SLJIT_SNEG = (SLJIT_DNEG | SLJIT_SINGLE_OP);
/* Flags: SP - (never set any flags) */
enum SLJIT_DABS = (SLJIT_FOP1_BASE + 8);
enum SLJIT_SABS = (SLJIT_DABS | SLJIT_SINGLE_OP);

sljit_si sljit_emit_fop1(sljit_compiler *compiler, sljit_si op,
    sljit_si dst, sljit_sw dstw,
    sljit_si src, sljit_sw srcw);

/* Starting index of opcodes for sljit_emit_fop2. */
enum SLJIT_FOP2_BASE = 160;

    /* Flags: SP - (never set any flags) */
enum SLJIT_DADD = (SLJIT_FOP2_BASE + 0);
enum SLJIT_SADD = (SLJIT_DADD | SLJIT_SINGLE_OP);
/* Flags: SP - (never set any flags) */
enum SLJIT_DSUB = (SLJIT_FOP2_BASE + 1);
enum SLJIT_SSUB = (SLJIT_DSUB | SLJIT_SINGLE_OP);
/* Flags: SP - (never set any flags) */
enum SLJIT_DMUL = (SLJIT_FOP2_BASE + 2);
enum SLJIT_SMUL = (SLJIT_DMUL | SLJIT_SINGLE_OP);
/* Flags: SP - (never set any flags) */
enum SLJIT_DDIV = (SLJIT_FOP2_BASE + 3);
enum SLJIT_SDIV = (SLJIT_DDIV | SLJIT_SINGLE_OP);

sljit_si sljit_emit_fop2(sljit_compiler *compiler, sljit_si op,
    sljit_si dst, sljit_sw dstw,
    sljit_si src1, sljit_sw src1w,
    sljit_si src2, sljit_sw src2w);

/* Label and jump instructions. */

sljit_label* sljit_emit_label(sljit_compiler *compiler);

/* Invert (negate) conditional type: xor (^) with 0x1 */

/* Integer comparison types. */
enum SLJIT_EQUAL = 0;
enum SLJIT_I_EQUAL = (SLJIT_EQUAL | SLJIT_INT_OP);
enum SLJIT_ZERO = 0;
enum SLJIT_I_ZERO = (SLJIT_ZERO | SLJIT_INT_OP);
enum SLJIT_NOT_EQUAL = 1;
enum SLJIT_I_NOT_EQUAL = (SLJIT_NOT_EQUAL | SLJIT_INT_OP);
enum SLJIT_NOT_ZERO = 1;
enum SLJIT_I_NOT_ZERO = (SLJIT_NOT_ZERO | SLJIT_INT_OP);

enum SLJIT_LESS = 2;
enum SLJIT_I_LESS = (SLJIT_LESS | SLJIT_INT_OP);
enum SLJIT_GREATER_EQUAL = 3;
enum SLJIT_I_GREATER_EQUAL = (SLJIT_GREATER_EQUAL | SLJIT_INT_OP);
enum SLJIT_GREATER = 4;
enum SLJIT_I_GREATER = (SLJIT_GREATER | SLJIT_INT_OP);
enum SLJIT_LESS_EQUAL = 5;
enum SLJIT_I_LESS_EQUAL = (SLJIT_LESS_EQUAL | SLJIT_INT_OP);
enum SLJIT_SIG_LESS = 6;
enum SLJIT_I_SIG_LESS = (SLJIT_SIG_LESS | SLJIT_INT_OP);
enum SLJIT_SIG_GREATER_EQUAL = 7;
enum SLJIT_I_SIG_GREATER_EQUAL = (SLJIT_SIG_GREATER_EQUAL | SLJIT_INT_OP);
enum SLJIT_SIG_GREATER = 8;
enum SLJIT_I_SIG_GREATER = (SLJIT_SIG_GREATER | SLJIT_INT_OP);
enum SLJIT_SIG_LESS_EQUAL = 9;
enum SLJIT_I_SIG_LESS_EQUAL = (SLJIT_SIG_LESS_EQUAL | SLJIT_INT_OP);

enum SLJIT_OVERFLOW = 10;
enum SLJIT_I_OVERFLOW = (SLJIT_OVERFLOW | SLJIT_INT_OP);
enum SLJIT_NOT_OVERFLOW = 11;
enum SLJIT_I_NOT_OVERFLOW = (SLJIT_NOT_OVERFLOW | SLJIT_INT_OP);

enum SLJIT_MUL_OVERFLOW = 12;
enum SLJIT_I_MUL_OVERFLOW = (SLJIT_MUL_OVERFLOW | SLJIT_INT_OP);
enum SLJIT_MUL_NOT_OVERFLOW = 13;
enum SLJIT_I_MUL_NOT_OVERFLOW = (SLJIT_MUL_NOT_OVERFLOW | SLJIT_INT_OP);

/* Floating point comparison types. */
enum SLJIT_D_EQUAL = 14;
enum SLJIT_S_EQUAL = (SLJIT_D_EQUAL | SLJIT_SINGLE_OP);
enum SLJIT_D_NOT_EQUAL = 15;
enum SLJIT_S_NOT_EQUAL = (SLJIT_D_NOT_EQUAL | SLJIT_SINGLE_OP);
enum SLJIT_D_LESS = 16;
enum SLJIT_S_LESS = (SLJIT_D_LESS | SLJIT_SINGLE_OP);
enum SLJIT_D_GREATER_EQUAL = 17;
enum SLJIT_S_GREATER_EQUAL = (SLJIT_D_GREATER_EQUAL | SLJIT_SINGLE_OP);
enum SLJIT_D_GREATER = 18;
enum SLJIT_S_GREATER = (SLJIT_D_GREATER | SLJIT_SINGLE_OP);
enum SLJIT_D_LESS_EQUAL = 19;
enum SLJIT_S_LESS_EQUAL = (SLJIT_D_LESS_EQUAL | SLJIT_SINGLE_OP);
enum SLJIT_D_UNORDERED = 20;
enum SLJIT_S_UNORDERED = (SLJIT_D_UNORDERED | SLJIT_SINGLE_OP);
enum SLJIT_D_ORDERED = 21;
enum SLJIT_S_ORDERED = (SLJIT_D_ORDERED | SLJIT_SINGLE_OP);

/* Unconditional jump types. */
enum SLJIT_JUMP = 22;
enum SLJIT_FAST_CALL = 23;
enum SLJIT_CALL0 = 24;
enum SLJIT_CALL1 = 25;
enum SLJIT_CALL2 = 26;
enum SLJIT_CALL3 = 27;

/* Fast calling method. See sljit_emit_fast_enter / sljit_emit_fast_return. */

/* The target can be changed during runtime (see: sljit_set_jump_addr). */
enum SLJIT_REWRITABLE_JUMP = 0x1000;


/* Emit a jump instruction. The destination is not set, only the type of the jump.
    type must be between SLJIT_EQUAL and SLJIT_CALL3
    type can be combined (or'ed) with SLJIT_REWRITABLE_JUMP
   Flags: - (never set any flags) for both conditional and unconditional jumps.
   Flags: destroy all flags for calls. */
sljit_jump* sljit_emit_jump(sljit_compiler *compiler, sljit_si type);

/* Basic arithmetic comparison. In most architectures it is implemented as
   an SLJIT_SUB operation (with SLJIT_UNUSED destination and setting
   appropriate flags) followed by a sljit_emit_jump. However some
   architectures (i.e: ARM64 or MIPS) may employ special optimizations here.
   It is suggested to use this comparison form when appropriate.
    type must be between SLJIT_EQUAL and SLJIT_I_SIG_LESS_EQUAL
    type can be combined (or'ed) with SLJIT_REWRITABLE_JUMP
   Flags: destroy flags. */
sljit_jump* sljit_emit_cmp(sljit_compiler *compiler, sljit_si type,
    sljit_si src1, sljit_sw src1w,
    sljit_si src2, sljit_sw src2w);

/* Basic floating point comparison. In most architectures it is implemented as
   an SLJIT_FCMP operation (setting appropriate flags) followed by a
   sljit_emit_jump. However some architectures (i.e: MIPS) may employ
   special optimizations here. It is suggested to use this comparison form
   when appropriate.
    type must be between SLJIT_D_EQUAL and SLJIT_S_ORDERED
    type can be combined (or'ed) with SLJIT_REWRITABLE_JUMP
   Flags: destroy flags.
   Note: if either operand is NaN, the behaviour is undefined for
         types up to SLJIT_S_LESS_EQUAL. */
sljit_jump* sljit_emit_fcmp(sljit_compiler *compiler, sljit_si type,
    sljit_si src1, sljit_sw src1w,
    sljit_si src2, sljit_sw src2w);

/* Set the destination of the jump to this label. */
void sljit_set_label(sljit_jump *jump, sljit_label* label);
/* Set the destination address of the jump to this label. */
void sljit_set_target(sljit_jump *jump, sljit_uw target);

/* Call function or jump anywhere. Both direct and indirect form
    type must be between SLJIT_JUMP and SLJIT_CALL3
    Direct form: set src to SLJIT_IMM() and srcw to the address
    Indirect form: any other valid addressing mode
   Flags: - (never set any flags) for unconditional jumps.
   Flags: destroy all flags for calls. */
sljit_si sljit_emit_ijump(sljit_compiler *compiler, sljit_si type, sljit_si src, sljit_sw srcw);

/* Perform the operation using the conditional flags as the second argument.
   Type must always be between SLJIT_EQUAL and SLJIT_S_ORDERED. The value
   represented by the type is 1, if the condition represented by the type
   is fulfilled, and 0 otherwise.

   If op == SLJIT_MOV, SLJIT_MOV_SI, SLJIT_MOV_UI:
     Set dst to the value represented by the type (0 or 1).
     Src must be SLJIT_UNUSED, and srcw must be 0
     Flags: - (never set any flags)
   If op == SLJIT_OR, op == SLJIT_AND, op == SLJIT_XOR
     Performs the binary operation using src as the first, and the value
     represented by type as the second argument.
     Important note: only dst=src and dstw=srcw is supported at the moment!
     Flags: I | E | K
   Note: sljit_emit_op_flags does nothing, if dst is SLJIT_UNUSED (regardless of op). */
sljit_si sljit_emit_op_flags(sljit_compiler *compiler, sljit_si op,
    sljit_si dst, sljit_sw dstw,
    sljit_si src, sljit_sw srcw,
    sljit_si type);

/* Copies the base address of SLJIT_SP + offset to dst.
   Flags: - (never set any flags) */
sljit_si sljit_get_local_base(sljit_compiler *compiler, sljit_si dst, sljit_sw dstw, sljit_sw offset);

/* The constant can be changed runtime (see: sljit_set_const)
   Flags: - (never set any flags) */
sljit_const* sljit_emit_const(sljit_compiler *compiler, sljit_si dst, sljit_sw dstw, sljit_sw init_value);

/* After the code generation the address for label, jump and const instructions
   are computed. Since these structures are freed by sljit_free_compiler, the
   addresses must be preserved by the user program elsewere. */
sljit_uw sljit_get_label_addr(sljit_label *label) { return label.addr; }
sljit_uw sljit_get_jump_addr(sljit_jump *jump) { return jump.addr; }
sljit_uw sljit_get_const_addr(sljit_const *const_) { return const_.addr; }

/* Only the address is required to rewrite the code. */
void sljit_set_jump_addr(sljit_uw addr, sljit_uw new_addr);
void sljit_set_const(sljit_uw addr, sljit_sw new_constant);

/* --------------------------------------------------------------------- */
/*  Miscellaneous utility functions                                      */
/* --------------------------------------------------------------------- */

enum SLJIT_MAJOR_VERSION = 0;
enum SLJIT_MINOR_VERSION = 92;

/* Get the human readable name of the platform. Can be useful on platforms
   like ARM, where ARM and Thumb2 functions can be mixed, and
   it is useful to know the type of the code generator. */
const(char*) sljit_get_platform_name();

/* Portable helper function to get an offset of a member. */
sljit_uw SLJIT_OFFSETOF(base, string member)() {
    mixin("return base." ~ member ~ ".offsetof;");
}

static if (SLJIT_UTIL_GLOBAL_LOCK) {
    /* This global lock is useful to compile common functions. */
    void sljit_grab_lock();
    void sljit_release_lock();
}

static if (SLJIT_UTIL_STACK) {
    /* The sljit_stack is a utiliy feature of sljit, which allocates a
       writable memory region between base (inclusive) and limit (exclusive).
       Both base and limit is a pointer, and base is always <= than limit.
       This feature uses the "address space reserve" feature
       of modern operating systems. Basically we don't need to allocate a
       huge memory block in one step for the worst case, we can start with
       a smaller chunk and extend it later. Since the address space is
       reserved, the data never copied to other regions, thus it is safe
       to store pointers here. */

    /* Note: The base field is aligned to PAGE_SIZE bytes (usually 4k or more).
       Note: stack growing should not happen in small steps: 4k, 16k or even
         bigger growth is better.
       Note: this structure may not be supported by all operating systems.
         Some kind of fallback mechanism is suggested when SLJIT_UTIL_STACK
         is not defined. */

    struct sljit_stack {
        /* User data, anything can be stored here.
           Starting with the same value as base. */
        sljit_uw top;
        /* These members are read only. */
        sljit_uw base;
        sljit_uw limit;
        sljit_uw max_limit;
    }

    /* Returns NULL if unsuccessful.
       Note: limit and max_limit contains the size for stack allocation
       Note: the top field is initialized to base. */
    sljit_stack* sljit_allocate_stack(sljit_uw limit, sljit_uw max_limit);
    void sljit_free_stack(sljit_stack* stack);

    /* Can be used to increase (allocate) or decrease (free) the memory area.
       Returns with a non-zero value if unsuccessful. If new_limit is greater than
       max_limit, it will fail. It is very easy to implement a stack data structure,
       since the growth ratio can be added to the current limit, and sljit_stack_resize
       will do all the necessary checks. The fields of the stack are not changed if
       sljit_stack_resize fails. */
    sljit_sw sljit_stack_resize(sljit_stack* stack, sljit_uw new_limit);
}

static if (!SLJIT_INDIRECT_CALL) {
    /* Get the entry address of a given function. */
    sljit_sw SLJIT_FUNC_OFFSET(string func_name)() {
        mixin("return cast(sljit_sw)&" ~ func_name ~ ";");
    }
} else {
    /* All JIT related code should be placed in the same context (library, binary, etc.). */

    sljit_sw SLJIT_FUNC_OFFSET(string func_name)() {
        mixin("return cast(sljit_sw)&" ~ func_name ~ ";");
    }

    /* For powerpc64, the function pointers point to a context descriptor. */
    struct sljit_function_context {
        sljit_sw addr;
        sljit_sw r2;
        sljit_sw r11;
    }

    /* Fill the context arguments using the addr and the function.
       If func_ptr is NULL, it will not be set to the address of context
       If addr is NULL, the function address also comes from the func pointer. */
    void sljit_set_function_context(void** func_ptr, sljit_function_context* context, sljit_sw addr, void* func);
}
