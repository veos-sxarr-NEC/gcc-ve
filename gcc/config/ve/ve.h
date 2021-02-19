/* Definitions of target machine for GNU compiler.  VE version.
   Copyright (C) 2007-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */
/* Changes by NEC Corporation for the VE port, 2017-2021 */

#include "config/ve/ve-opts.h"

#define TARGET_VE 1

#define VE_REGSYM "%"

/* Compiler working register, only for last resort */
#define VE_SCRATCH_REGNUM 12

/* linkage and text base register */
#define VE_LINKAGE_REGNUM 17

/* return address register */
#define VE_RETURN_REGNUM 10

/* thread pointer  */
#define VE_THREAD_POINTER_REGNUM 14

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 11

/* Register indicates stack limitation */
#define STACK_LIMIT_REGNUM 8

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 9

/* Smallest number of callee saved register */
#define VE_SMALLEST_CALLEE_SAVED_REGNUM 18


/* Register in which address to store a structure value
   is passed to a function.  
 #define VE_STRUCT_VALUE_REGNUM 0

 We do not use this since we set struct_value_rtx to NULL 
 which means it is passed as an extra argument 
*/

/* start of temporary (can be freely used) registers */
#define VE_BEGIN_TEMP_REGNUM 34

/* Register which holds the Global Offset Table, if any.  */
#define VE_GOT_REGNUM 15

/* Register which holds the Procedure Linkage Table, if any.  */
#define VE_PLT_REGNUM 16

/* Register which holds offset table for position-independent
   data references.  */

#define PIC_OFFSET_TABLE_REGNUM \
  (flag_pic ? VE_GOT_REGNUM : INVALID_REGNUM)

#ifdef SINGLE_LIBC
#define OPTION_GLIBC  (DEFAULT_LIBC == LIBC_GLIBC)
#define OPTION_UCLIBC (DEFAULT_LIBC == LIBC_UCLIBC)
#define OPTION_BIONIC (DEFAULT_LIBC == LIBC_BIONIC)
#undef OPTION_MUSL
#define OPTION_MUSL   (DEFAULT_LIBC == LIBC_MUSL)
#else
#define OPTION_GLIBC  (linux_libc == LIBC_GLIBC)
#define OPTION_UCLIBC (linux_libc == LIBC_UCLIBC)
#define OPTION_BIONIC (linux_libc == LIBC_BIONIC)
#undef OPTION_MUSL
#define OPTION_MUSL   (linux_libc == LIBC_MUSL)
#endif

/* Target CPU builtins.  */

#define TARGET_CPU_CPP_BUILTINS()                       \
  do                                                    \
    {                                                   \
        builtin_define ("__linux__");                   \
        builtin_define ("__linux");                     \
        builtin_define ("linux");                       \
        builtin_assert ("system=linux");                \
        builtin_define ("__gnu_linux__");               \
        builtin_define ("__unix__");                    \
        builtin_define ("__unix");                      \
        builtin_define ("unix");                        \
        builtin_assert ("system=unix");                 \
        builtin_assert ("system=posix");                \
        builtin_define ("__ve__");                      \
        builtin_define ("__ve");                        \
        builtin_define ("__VE__");                      \
        builtin_define ("__VE");                        \
        builtin_assert ("cpu=ve");                      \
        builtin_assert ("machine=ve");                  \
} while (0)

/* Conditional codes for branch instrunctions.
 Note:
   VE "ne" operation is "not equal and not nan"; this is "ltgt" of gcc
   VE "nenan" operation is "not equal or nan"; this is "ne" of gcc */
#define VE_COND_EQ "eq"
#define VE_COND_NE "ne"
#define VE_COND_FNE "nenan"
#define VE_COND_GT "gt"
#define VE_COND_GE "ge"
#define VE_COND_LT "lt"
#define VE_COND_LE "le"
#define VE_COND_GTU "gt"
#define VE_COND_GEU "ge"
#define VE_COND_LTU "lt"
#define VE_COND_LEU "le"
#define VE_COND_UNEQ "eqnan"
#define VE_COND_LTGT "ne"
#define VE_COND_UNGT "gtnan"
#define VE_COND_UNGE "genan"
#define VE_COND_UNLT "ltnan"
#define VE_COND_UNLE "lenan"
#define VE_COND_UNORDERED "nan"
#define VE_COND_ORDERED "num"

/* asm load store instrunction flag */
enum {
VE_DIR_LOAD, 
VE_DIR_STORE,
VE_DIR_QLOAD,
VE_DIR_QLOAD1,
VE_DIR_QLOAD2,
VE_DIR_QSTORE,
VE_DIR_QSTORE1,
VE_DIR_QSTORE2,
VE_DIR_PFCH,
VE_DIR_ATOMIC01,
VE_DIR_ATOMIC20
};

/* If the assembler supports thread-local storage, assume that the
   system does as well. */
#define TARGET_HAVE_TLS true
#define TARGET_TLS 1 
#undef TARGET_GNU_TLS
#define TARGET_GNU_TLS 1 

/* Target machine storage layout */

/* Define this macro to have the value 1, if most significant
   bit is lowest numbered in instructions that operate on
   numbered bit-fields. */
#define BITS_BIG_ENDIAN 0
	
/* Define this macro to have the value 1 if the most significant 
   byte in a word has the lowest number. */ 
#define BYTES_BIG_ENDIAN 0

/* Define this macro to have the value 1 if, in a multiword
   object, the most significant word has the lowest number.
   This applies to both memory locations and registers */
#define WORDS_BIG_ENDIAN 0

#define REG_WORDS_BIG_ENDIAN 1

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 8

/* Minimum number of units in a word.  If this is undefined, the default
   is UNITS_PER_WORD.  Otherwise, it is the constant value that is the
   smallest value that UNITS_PER_WORD can have at run-time.

   FIXME: This needs to be 4 when TARGET_64BIT is true to suppress the
   building of various TImode routines in libgcc.  The ve runtime
   specification doesn't provide the alignment requirements and calling
   conventions for TImode variables.  */
#define MIN_UNITS_PER_WORD 4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE) \
if (GET_MODE_CLASS (MODE) == MODE_INT           \
    && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)   \
{                                               \
  (MODE) = DImode;                              \
}

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 64

/* Allocation boundary (in *bits*) for storing arguments in argument list.  
   It cound be 64, but support of long double arise the bug 38395 problem
   PARM_BOUNDARY =64 < GET_MODE_ALIGNMENT(TFmode) */
#define PARM_BOUNDARY 64

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
/* if we change it to 128, parameter list rounds-up to 128byte */
#define STACK_BOUNDARY 128

/* Define this macro if you wish to preserve a certain alignment for
   the stack pointer, greater than what the hardware enforces.  The
   definition is a C expression for the desired alignment (measured
   in bits).  This macro must evaluate to a value equal to or larger
   than STACK_BOUNDARY. */
/* This must be 128 otherwise TFmode(long double) variable does not
   allocated on frame properly */
#define PREFERRED_STACK_BOUNDARY 128

/* Allocation boundary (in *bits*) for the code of a function.  */
/* 128 means 16 byte align */
#define FUNCTION_BOUNDARY 128
#if 0
#define FUNCTION_BOUNDARY 1024
#endif

/* biggest alignment of data. If we set it to 128, some optimization
   makes wrong code for TF mode when expand_asignment expand TFs mode
   variable to bit_fields on a litte endian mode */
#define BIGGEST_ALIGNMENT 128
#define BIGGEST_FIELD_ALIGNMENT 128

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32


/* Number of bits which any structure or union's size must be a
   multiple of.  Each structure or union's size is rounded up to a
   multiple of this.  */
/* The usage of 't' is tricky; this macro is used only once in 
   stor-layout.c where t is a variable of type tree */ 
#define STRUCTURE_SIZE_BOUNDARY (TARGET_PADSTRUCT ? 32 : 8)


/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.*/

#define STRICT_ALIGNMENT 1

/* If bit field type is int, don't let it cross an int,
   and give entire struct the alignment of an int.  */
/* Required on the 386 since it doesn't have bit-field insns.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 64
#define LONG_LONG_TYPE_SIZE 64
#define CHAR_TYPE_SIZE 8
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 128

/* Select _Bool size depends on compile option */
#define BOOL_TYPE_SIZE (TARGET_FOUR_BYTE_BOOL ? INT_TYPE_SIZE : CHAR_TYPE_SIZE)

/* Define default char to be signed. This is the same as that of x86
   but different from the sxc++ default */ 
#define DEFAULT_SIGNED_CHAR 1

/* long double is not a fixed mode, but the idea is that, if we
   support long double, we also want a 128-bit integer type.  */
#define MAX_FIXED_MODE_SIZE 64

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */
#define STORE_FLAG_VALUE 1

/* Define DEFAULT_PCC_STRUCT_RETURN to 1 if all structure and union return
   values must be in memory.  */
/* So, RETURN_IN_MEMORY is not used! When setting this to 0, struct {char c;} 
   is not treated as BLKmode and do not go to memory */
#define DEFAULT_PCC_STRUCT_RETURN 1

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
  return the mode to be used for the comparison.  For floating-point, CCFPmode
  should be used. */ 

/* A function address in a call instruction
 * is 8 byte address (all instruction length is 8 byte) */
#define FUNCTION_MODE DImode

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
/* Although truncations happen on the machine instructions 
   We do not set this macro, for the compatibility with the
   the VE proprietary compiler */ 
#define SHIFT_COUNT_TRUNCATED (TARGET_SHIFT_COUNT_FULL? 0 : 1)

#define Pmode DImode

/* On nec's ve c++ if -size_t64 is specified size_t becomes unsigned long long,
   otherwize it is unsigned int */
#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE   "int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE  32


/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 1

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE DImode

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated. 
   We represent all SI values as sign-extended DI values in
   registers.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) \
 ((INPREC) <= 32 || (OUTPREC) > 32)

/*
 * We can't load any constants as immediates.
 */


/*
 * Since all the registers are equivalent, 
 * a value can be reloaded in any register of the same
 * class.
 */

#define PREFERRED_RELOAD_CLASS(X,CLASS) (CLASS)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

/* Emit rtl for profiling.  */
#define PROFILE_HOOK(LABEL)   ve_profile_hook (LABEL)

/* All the work done in PROFILE_HOOK, but still required.  */
#define FUNCTION_PROFILER(FILE, LABELNO) do { } while (0)

/* ve uses a counter for mcount, but needs special treatment (slcomm). */
#define NO_PROFILE_COUNTERS 1

/* Define this macro if the code for function profiling should come
   before the function prologue.  Normally, the profiling code comes
   after.  */


/* memory copy with 64 bit words. */
#define MOVE_MAX 8

#define TEXT_SECTION_ASM_OP "\t.text"
#define DATA_SECTION_ASM_OP (TARGET_ASM_ALIGN ? \
  "\t.data\n\t.align\t4" : "\t.data\n\t.balign\t16" )
#define READONLY_DATA_SECTION_ASM_OP (TARGET_ASM_ALIGN ? \
  "\t.section\t.rodata\n\t.align\t4" : "\t.section\t.rodata\n\t.balign\t16")

#define CTORS_SECTION_ASM_OP "\t.section\t.init_array,\"aw\",@init_array"
#define DTORS_SECTION_ASM_OP "\t.section\t.fini_array,\"aw\",@fini_array"

#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
#define INIT_ARRAY_SECTION_ASM_OP CTORS_SECTION_ASM_OP
#define FINI_ARRAY_SECTION_ASM_OP DTORS_SECTION_ASM_OP

/* Since we use .init_array/.fini_array we don't need the markers at
   the start and end of the ctors/dtors arrays.  */
#define CTOR_LIST_BEGIN asm (CTORS_SECTION_ASM_OP)
#define CTOR_LIST_END           /* empty */
#define DTOR_LIST_BEGIN asm (DTORS_SECTION_ASM_OP)
#define DTOR_LIST_END           /* empty */


/* Some systems use __main in a way incompatible with its use in gcc, in these
 * cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
 * give the same symbol without quotes for an alternative entry point.  You
 * must define both, or neither.  */


/* Given a decl node or constant node, choose the section to output it in
   and select that section.  */
/* These are from mips.h, simplified somewhat. */

#define TARGET_ASM_NAMED_SECTION  default_elf_asm_named_section

/* Allow the use of the -frecord-gcc-switches switch via the
   elf_record_gcc_switches function defined in varasm.c.  */
#undef  TARGET_ASM_RECORD_GCC_SWITCHES
#define TARGET_ASM_RECORD_GCC_SWITCHES elf_record_gcc_switches

#define ASM_APP_ON "#APP\n"
#define ASM_APP_OFF "#NO_APP\n"
#define ASM_COMMENT_START "#"
#define ASM_LONGLONG "\t.quad\t"

#undef LOCAL_LABEL_PREFIX 
#define LOCAL_LABEL_PREFIX "."
#undef USER_LABEL_PREFIX 
#define USER_LABEL_PREFIX ""

/* If defined, a C expression to compute the alignment given to a
   constant that is being placed in memory.  CONSTANT is the constant
   and ALIGN is the alignment that the object would ordinarily have.
   The value of this macro is used instead of that alignment to align
   the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that `strcpy' calls that copy
   constants can be done inline.  */

#define CONSTANT_ALIGNMENT(EXP, ALIGN) \
  ve_constant_alignment ((EXP), (ALIGN))

/* : BITS_PER_WORD) */

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */
/* Stolen from mips.h */

#undef DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)                         \
  ((((ALIGN) < 4)                                           \
    && (TREE_CODE (TYPE) == UNION_TYPE                      \
        || TREE_CODE (TYPE) == RECORD_TYPE)) ? 4 : (ALIGN))

/* We need this for the same reason as DATA_ALIGNMENT, namely to cause
   character arrays to be word-aligned so that `strcpy' calls that copy
   constants to character arrays can be done inline, and 'strcmp' can be
   optimised to use word loads. */
#define LOCAL_ALIGNMENT(TYPE, ALIGN) \
  DATA_ALIGNMENT (TYPE, ALIGN)


#define ASM_OUTPUT_ALIGN(stream,val)                   \
{ if (TARGET_ASM_ALIGN)                                \
    fprintf(stream,"\t.align\t%d\n",(int)(val));       \
  else                                                 \
    fprintf(stream,"\t.balign\t%d\n",(1<<(int)(val)));}


#define ASM_OUTPUT_SKIP(stream,val)  \
         fprintf(stream,"\t.skip\t%d\n",(int)(val))


/*
 * Need to split up .ascii directives to avoid breaking 
 * the linker.
 */

/*
 * Nonzero value if GCC should output the constant pool
 * for a function before the code for the function.
 * Zero value if GCC should output the constant pool
 * after the function.
 */
#define CONSTANT_POOL_BEFORE_FUNCTION 0

#undef ASM_OUTPUT_FUNCTION_LABEL
#define ASM_OUTPUT_FUNCTION_LABEL(file, funname, fundecl)	\
  ve_asm_output_function_label (file, funname, fundecl)

#define ASM_OUTPUT_FUNCTION_PREFIX(stream, fnname)		\
    ve_asm_output_function_prefix(stream,fnname)

/* This says how to output an assembler line
   to define a global common symbol.  */


/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_ALIGNED_COMMON(stream, name, size, alignment)\
  { switch_to_section(data_section);				\
  fputs("\t.comm\t",stream); assemble_name(stream,name);	\
  fprintf(stream,", " HOST_WIDE_INT_PRINT_DEC ", %u\n",         \
       (size),(alignment)/BITS_PER_UNIT); }

#define ASM_OUTPUT_ALIGNED_LOCAL(stream, name, size, alignment)\
  { switch_to_section(data_section);				\
  fputs("\t.local\t",stream); assemble_name(stream,name);	\
  fputs("\n\t.comm\t",stream); assemble_name(stream,name);	\
  fprintf(stream,", " HOST_WIDE_INT_PRINT_UNSIGNED ", %u\n",    \
       (size),(alignment)/BITS_PER_UNIT); }

#undef  TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT      "@%s"

#undef SIZE_ASM_OP
#define SIZE_ASM_OP "\t.size\t"

#undef TYPE_ASM_OP
#define TYPE_ASM_OP "\t.type\t"


#define ASM_OUTPUT_TYPE_DIRECTIVE(STREAM, NAME, TYPE)                   \
  do                                                                    \
    {                                                                   \
      fputs (TYPE_ASM_OP, STREAM);                                      \
      assemble_name (STREAM, NAME);                                     \
      fputs (", ", STREAM);                                             \
      fprintf (STREAM, TYPE_OPERAND_FMT, TYPE);                         \
      putc ('\n', STREAM);                                              \
    }                                                                   \
  while (0)

/* This macro closes up a function definition for the assembler.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE,NAME,DECL) \
  ve_end_function(FILE,NAME,DECL)


#undef  ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)               \
  do                                                            \
    {                                                           \
      HOST_WIDE_INT size;                                       \
                                                                \
        ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");       \
                                                                \
      size_directive_output = 0;                                \
      if (!flag_inhibit_size_directive                          \
          && (DECL) && DECL_SIZE (DECL))                        \
        {                                                       \
          size_directive_output = 1;                            \
          size = int_size_in_bytes (TREE_TYPE (DECL));          \
          ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, size);         \
        }                                                       \
                                                                \
      ASM_OUTPUT_LABEL (FILE, NAME);                            \
    }                                                           \
  while (0)


#define ASM_OUTPUT_LABEL(STREAM, LABEL)                 \
{ assemble_name(STREAM, LABEL); fputs(":\n", STREAM); }

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl\t"


/* May add underscore before label */
#define ASM_OUTPUT_LABELREF(STREAM, NAME)                \
  asm_fprintf (STREAM, "%U%s", NAME)

#define ASM_OUTPUT_INTERNAL_LABEL(STREAM, NAME)		\
{ fprintf(STREAM, "%s:\n", NAME);}

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  do { char buf[100];                                   \
       fputs (ASM_LONGLONG, FILE);                      \
       ASM_GENERATE_INTERNAL_LABEL (buf, "L", VALUE);   \
       assemble_name (FILE, buf);                       \
       putc ('-', FILE);                                \
       ASM_GENERATE_INTERNAL_LABEL (buf, "L", REL);     \
       assemble_name (FILE, buf);                       \
       putc ('\n', FILE);                               \
     } while (0)


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE DImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE 1

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)  \
  sprintf((LABEL), "%s%s%ld", LOCAL_LABEL_PREFIX, (PREFIX), (long)(NUM))

/* Output for declaring the name of an external symbol name 
   which is referenced in this compilation but not defined. 
   This need not be defined since it is treated as external by 
   as/ld default. This is to make sure of it.  
*/

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME) \
  default_elf_asm_output_external (FILE, DECL, NAME);

/* This is how we tell the assembler that two symbols have the same value.  */

#undef  ASM_OUTPUT_DEF
#define ASM_OUTPUT_DEF(stream, alias, name) \
  do {                                      \
    fputs("\t.set\t", stream);              \
    assemble_name(stream,alias);            \
    fputs(",", stream);                     \
    assemble_name(stream,name);             \
    fputs("\n", stream);                    \
  } while (0)

/* Define this so that jump tables go in same section as 
   the current function */
#define JUMP_TABLES_IN_TEXT_SECTION 1


/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   We define all 64 integer/floating registers, there are no other
   registers.  */

#define FIRST_PSEUDO_REGISTER 64


#define FIRST_S_REG 0
#define LAST_S_REG 63

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   Fixed registers are s8(stack limit), s9(frame pointer),
   s11(stack pointer), s14(thread pointer), 
   s15(global offset table), s16(procedure linkage table),
   and s17(linkage-area register) */

/* s12(spill registers used for last resort at/after reload 
   when no free register is available */

#define FIXED_REGISTERS { \
 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1,     \
 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     \
 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     \
 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     \
 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.
   s8(stack limit), s9(frame pointer),
   s11(stack pointer), s14(thread descriptor)
   s15(got), s16(plt), s17(linkage/text base), 
   and
   s0 - s7 (parameter/result reg.) 
   s10(return PC), s12(entry/ scratch), s13(symbol information /scratch)
   s34-s63 (free regs.)         */

#define CALL_USED_REGISTERS { \
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     \
 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
 }

#define REGISTER_NAMES                                    \
{ "%s0" , "%s1" , "%s2" , "%s3" , "%s4" , "%s5" , "%s6" , "%s7",  \
  "%sl" , "%fp" , "%lr",  "%sp",  "%s12", "%s13", "%tp",  "%got", \
  "%plt", "%s17", "%s18", "%s19", "%s20", "%s21", "%s22", "%s23", \
  "%s24", "%s25", "%s26", "%s27", "%s28", "%s29", "%s30", "%s31", \
  "%s32", "%s33", "%s34", "%s35", "%s36", "%s37", "%s38", "%s39", \
  "%s40", "%s41", "%s42", "%s43", "%s44", "%s45", "%s46", "%s47", \
  "%s48", "%s49", "%s50", "%s51", "%s52", "%s53", "%s54", "%s55", \
  "%s56", "%s57", "%s58", "%s59", "%s60", "%s61", "%s62", "%s63", \
 }

#define ADDITIONAL_REGISTER_NAMES       \
{                      \
    { "%s8",  8  }     \
  , { "%s9",  9  }     \
  , { "%s10", 10 }     \
  , { "%s11", 11 }     \
  , { "%s14", 14 }     \
  , { "%s15", 15 }     \
  , { "%s16", 16 }     \
}


/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.*/
#define REG_ALLOC_ORDER         \
  {                                        \
   34, 35, 36, 37, 38, 39,                 \
   40, 41, 42, 43, 44, 45, 46, 47, 48, 49, \
   50, 51, 52, 53, 54, 55, 56, 57, 58, 59, \
   60, 61, 62, 63,                         \
   10,                                     \
    7,  6,  5,  4,  3,  2,  1,  0,         \
   18, 19, 20, 21, 22, 23, 24, 25, 26, 27, \
   28, 29, 30, 31, 32, 33,                 \
   13, 16, 15, 14,                         \
   17, 9,  11,  8, 12 }


/********************************************* later*/
/* values that can go in particular registers. */
/* q-doubles will be the only thing that will take 2 registers. */

#define HARD_REGNO_NREGS(regno,mode) \
  ((GET_MODE_SIZE(mode) + UNITS_PER_WORD - 1)/UNITS_PER_WORD)

#define EPILOGUE_USES(REGNO)    ve_epilogue_uses (REGNO)

/*
 * Allocate registers appropriate to data types. doubles 
 * require even/odd pairs of long double registers.  */

#define HARD_REGNO_MODE_OK(regno,mode) ve_hard_regno_mode_ok(regno,mode)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

/* How to renumber registers for dbx and gdb. */
#define DBX_REGISTER_NUMBER(REGNO) ve_dbx_register_number (REGNO)

/* We use the identical numbers for the DWARF 2 CFA column numbers */
#define DWARF_FRAME_REGNUM(REG) DBX_REGISTER_NUMBER (REG)
#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (VE_RETURN_REGNUM)


/* Return a class of registers that cannot change FROM mode to TO mode.  */
#define CANNOT_CHANGE_MODE_CLASS(FROM, TO, CLASS) \
  ve_cannot_change_mode_class (FROM, TO, CLASS)


#define MODES_TIEABLE_P(mode1, mode2) ve_modes_tieable_p(mode1,mode2) 

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

enum reg_class
{
  NO_REGS,                      /* no registers in set */
  QS_REGS,                      /* scalar even number registers */
  S_REGS,                       /* scalar registers */
  ALL_REGS,                     /* all registers */
  LIM_REG_CLASSES               /* max value + 1 */
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define GENERAL_REGS S_REGS
#define GP_REG_P(REGNO) (FIRST_S_REG <= (int)(REGNO) && (int)(REGNO) <= LAST_S_REG)

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES                                                 \
{                                                                       \
  "NO_REGS",                                                            \
  "QS_REGS",                                                            \
  "S_REGS",                                                             \
  "ALL_REGS"                                                            \
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS                                               \
{                                                                        \
  { 0x00000000,0x00000000 },   /* no registers */  \
  { 0x55555555,0x55555555 },   /* qs registers */   \
  { 0xffffffff,0xffffffff },   /* s  registers */ \
  { 0xffffffff,0xffffffff }    /* all registers */ \
}

#define REGNO_REG_CLASS(REGNO) ((REGNO) < FIRST_PSEUDO_REGISTER \
                      ? ((REGNO % 2) ? QS_REGS : S_REGS) : NO_REGS)   

/* The class value for index registers, and the one for base regs.  */

#define BASE_REG_CLASS S_REGS
#define INDEX_REG_CLASS S_REGS

#define VE_FIRST_ARG_REGNUM 0
#define VE_MAX_ARGS_IN_REGS 8


/* If we use the normal load/store ops ,
   it will always sign-extend sub-word types. */
#define LOAD_EXTEND_OP(mode) SIGN_EXTEND


/*
 * Memory address stuff.
 */


/* Addressing modes, and classification of registers for them.  */

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects them all.
   The symbol REG_OK_STRICT causes the latter definition to be used.

  Most source files want to accept pseudo regs in the hope that
  they will get allocated to the class that the insn wants them to be in.
  Some source files that are used after register allocation
  need to be strict.  */

#ifndef REG_OK_STRICT
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  ve_regno_mode_ok_for_base_p (REGNO (X), MODE, 0)
#else
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  ve_regno_mode_ok_for_base_p (REGNO (X), MODE, 1)
#endif


#define REGNO_IN_RANGE(REGNO, MIN, MAX)         \
  (IN_RANGE ((REGNO), (MIN), (MAX))             \
   || (reg_renumber != NULL                     \
       && reg_renumber[(REGNO)] >= (MIN)        \
       && reg_renumber[(REGNO)] <= (MAX)))

#define REGNO_OK_FOR_INDEX_P(REGNO)		\
		REGNO_IN_RANGE(REGNO,FIRST_S_REG,LAST_S_REG)

#define REGNO_OK_FOR_BASE_P(REGNO)		\
		REGNO_IN_RANGE(REGNO,FIRST_S_REG,LAST_S_REG)

/* Non strict versions, pseudos are ok.  */
#define REG_OK_FOR_INDEX_NONSTRICT_P(X)	1
#define REG_OK_FOR_BASE_NONSTRICT_P(X)	1

/* Strict versions, hard registers only */
#define REG_OK_FOR_INDEX_STRICT_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_STRICT_P(X)  REGNO_OK_FOR_BASE_P (REGNO (X))

#ifndef REG_OK_STRICT
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_NONSTRICT_P (X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_NONSTRICT_P (X)

#else
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_STRICT_P (X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_STRICT_P (X)
#endif


/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and jump to WIN.  This
   macro is used in only one place: `find_reloads_address' in reload.c.  */


/* Check for constness inline but use mips_legitimate_address_p
   to check whether a constant really is an address.  */


/* This handles the magic '..CURRENT_FUNCTION' symbol, which means
 *    'the start of the function that this code is output in'.  */


/*
 * VE has 32 bit immediates. 
 */
#define SMALL_INT(X)  \
  ((HOST_WIDE_INT)INTVAL(X) >= -2147483647LL-1 && (HOST_WIDE_INT)INTVAL(X) <= 2147483647)
#define SMALL_OPERAND(VALUE) \
  ((unsigned HOST_WIDE_INT) (VALUE) + 0x8000 < 0x10000)

#define CLASS_UNITS(mode, size)                                         \
  ((GET_MODE_SIZE (mode) + (size) - 1) / (size))

#define CLASS_MAX_NREGS(CLASS, MODE)                                    \
    (CLASS_UNITS (MODE, UNITS_PER_WORD))

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
/* #define STACK_GROWS_DOWNWARD */

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */

#define FRAME_GROWS_DOWNWARD 1

/* Register save area : fp(s9),lr(s10),and s14-s33 (22 * 8byte) */ 
#define VE_RSA_SIZE 176

/* stack allocated variables have such offset that reserve outgoing pramlist */ 
#define STARTING_FRAME_OFFSET 0

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable current_function_outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* reserve stack space for all argument even when their values are 
passed in registers.  */
#define REG_PARM_STACK_SPACE(FNDECL)    UNITS_PER_WORD

/* Define this if it is the responsibility of the caller to
   allocate the area reserved for arguments passed in registers.
   If `ACCUMULATE_OUTGOING_ARGS' is also defined, the only effect
   of this macro is to determine whether the space is included in
   `crtl->outgoing_args_size'.  */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1
#define STACK_PARMS_IN_REG_PARM_AREA

#define STACK_POINTER_OFFSET (VE_RSA_SIZE)

/* Eliminating the Frame Pointer and the Arg Pointer.  */
#define ELIMINABLE_REGS                                 \
{                                                       \
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},         \
}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = ve_initial_elimination_offset (FROM, TO)


/* Lifted from mips.h.
 * If defined, a C expression that gives the alignment boundary, in
 * bits, of an argument with the specified mode and type.  If it is
 * not defined,  `PARM_BOUNDARY' is used for all arguments.  
 */


/* incomming args are accessed from this offset with %s(ARG_POINTER_REGNUM) */
#define FIRST_PARM_OFFSET(FNDECL) (VE_RSA_SIZE)


/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */


/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 		9 

/* Register for nested function (trampoline) */
#define STATIC_CHAIN_REGNUM             63

#define RETURN_ADDR_RTX(COUNT, FRAME)           \
 (ve_return_addr_rtx (COUNT, FRAME))

/* Before the prologue, RA lives in VE_RETURN_REGNUM  */
#define INCOMING_RETURN_ADDR_RTX  (gen_rtx_REG(Pmode,VE_RETURN_REGNUM))

/* Before the prologue, the top of the frame is at 0(%sp).  */
#define INCOMING_FRAME_SP_OFFSET 0

/* Place to put static chain when calling a function that requires it.  */


/* Place where static chain is found upon entry to routine.  */


#define FUNCTION_VALUE(VALTYPE, FUNC)                                   \
 ve_function_value(VALTYPE,FUNC)

/* return in s0, s1, s2, or s3. */
#define LIBCALL_VALUE(MODE)     \
 ve_libcall_value(MODE)

struct ve_args
{
  int arg_words;   /* Total words the argumets take */
  int indirect;    /* Function call is indirect */
  int stdarg;      /* Stdarg */
  int prototype;   /* Function has a prototype */
};  

/*typedef int CUMULATIVE_ARGS;*/
#define CUMULATIVE_ARGS struct ve_args

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  ve_init_cumulative_args(&CUM, FNTYPE, LIBNAME, FNDECL);

/*
 * First few (VE_MAX_ARGS_IN_REGS) args are passed in  registers.
 */

#if VE_FIRST_ARG_REGNUM == 0
#define FUNCTION_ARG_REGNO_P(regno) \
((regno) < VE_FIRST_ARG_REGNUM + VE_MAX_ARGS_IN_REGS)
#else
#define FUNCTION_ARG_REGNO_P(regno) \
((regno) >= VE_FIRST_ARG_REGNUM && \
 (regno) < VE_FIRST_ARG_REGNUM + VE_MAX_ARGS_IN_REGS)
#endif

/*
 * Return value is in s0, s1, s2, or s3.
 */
#define FUNCTION_VALUE_REGNO_P(regno) \
((regno) == 0 ||(regno) == 1 ||(regno) == 2 ||(regno) ==3)

/* Initialize data used by insn expanders.  This is called from insn_emit,
   once for every function before code is generated.  */
#define INIT_EXPANDERS  ve_init_expanders ()

/*
 * Trampoline stuff, stolen from mips.h.
 * This will need serious work.
 *
 */

/* A C expression for the size in bytes of the trampoline, as an
   integer.  */

#define TRAMPOLINE_SIZE 40
/* Alignment required for trampolines, in bits.

   If you don't define this macro, the value of `BIGGEST_ALIGNMENT'
   is used for aligning trampolines.  */

#define TRAMPOLINE_ALIGNMENT 64 

#undef TARGET_ELF
#define TARGET_ELF 1

/* Tell collect that the object format is ELF.  */
#define OBJECT_FORMAT_ELF

/* Do not use TM clone registry as it currently doesn't work. */
#define USE_TM_CLONE_REGISTRY 0

/* Definitions for debugging.  */

/* generate embedded stabs */

/* Generate DWARF2 debugging information and make it the default.  */
#define DWARF2_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Use a more compact format for line information.  */
#define DWARF2_ASM_LINE_DEBUG_INFO 1

#define DWARF2_UNWIND_INFO         0
#define TARGET_DEBUG_UNWIND_INFO  ve_debug_unwind_info

#undef TARGET_GAS
#define TARGET_GAS 1

/* gas supports weak label */
#define SUPPORTS_WEAK 1

/* This is how to tell assembler that a symbol is weak  */
#undef ASM_WEAKEN_LABEL
#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
       fputc ('\n', FILE); } while (0)

/* Support for C++ templates.  */
#undef MAKE_DECL_ONE_ONLY
#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

/* Biggest alignment supported by the object file format of this
   machine.  Use this macro to limit the alignment which can be
   specified using the `__attribute__ ((aligned (N)))' construct.  If
   not defined, the default value is `BIGGEST_ALIGNMENT'.  */
#define MAX_OFILE_ALIGNMENT (32768 * 8)


#define TARGET_THREAD_SSP_OFFSET (0x10)

/* Define this macro if it is as good or better to call a constant
 *    function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1
