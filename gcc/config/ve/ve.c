/* Subroutines used for VE code generation.
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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "insn-attr.h"
#include "recog.h"
#include "output.h"
#include "stringpool.h"
#include "function.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "expr.h"
#include "flags.h"
#include "reload.h"
#include "tm_p.h"
#include "diagnostic-core.h"
#include "optabs.h"
#include "libfuncs.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"
#include "common/common-target.h"
#include "targhooks.h"
#include "langhooks.h"
#include "gstab.h"
#include "hashtab.h"
#include "debug.h"
#include "sched-int.h"
#include "bitmap.h"
#include "diagnostic.h"
#include "real.h"
#include "insn-flags.h"
#include "toplev.h"
#include "predict.h"
#include "basic-block.h"
#include "df.h"
#include "hash-map.h"
#include "is-a.h"
#include "plugin-api.h"
#include "ipa-ref.h"
#include "cgraph.h"
#include "builtins.h"
#include <ctype.h>

/* s(ARG_POINTER_REGNUM) is saved if number of args >= 9 */
#define REG_NEEDS_SAVE(i) ( \
   ( i==VE_GOT_REGNUM || i==VE_PLT_REGNUM ) || \
   df_regs_ever_live_p(i) \
   && ( !(call_used_regs[i]) \
       || (i==VE_LINKAGE_REGNUM && ve_use_linkage)))
/*   || (i==10 && !crtl->is_leaf)) */  
/* FIXME s10 is saved 
  if function is (!leaf [!crtl->is_leaf]
  or __builtin_return_address used ) */
/* no register is always saved */
#define REG_ALWAYS_SAVE_BEG 17
#define REG_ALWAYS_SAVE_END 17
#define VE_MAX_SAVE_REGS 20

#define VE_BUF_SIZE 130

void abort_with_insn (rtx, const char *);
void oops_message(char *);
bool ve_symbolic_constant_p (rtx);
static void ve_split_const (rtx, rtx *, HOST_WIDE_INT *);
rtx ve_legitimize_address (rtx, rtx,machine_mode);
static rtx ve_struct_value_rtx (tree, int);
static bool ve_return_in_memory (const_tree, const_tree);
static bool ve_pass_by_reference (cumulative_args_t cum,
                machine_mode mode, const_tree type, bool named);
static int ve_const_insns (rtx x);
static bool ve_reg_ok_for_legitimate(rtx x, bool strict);
static rtx ve_expand_mem_scratch(rtx x, int *changed);
static void ve_addr_cut_mem_internal(rtx x); 
static void ve_output_addr_const_offset(FILE *file, rtx addr, 
      HOST_WIDE_INT offset);
static bool ve_can_eliminate (const int from, const int to);
static rtx ve_function_arg (cumulative_args_t,
                               machine_mode, const_tree, bool);
static rtx ve_function_arg_1 (cumulative_args_t,
                                 machine_mode, const_tree, bool, bool);
static rtx ve_function_incoming_arg (cumulative_args_t,
                                        machine_mode, const_tree, bool);
static unsigned int ve_function_arg_boundary (machine_mode,
                                                  const_tree);
static void ve_asm_output_ldst_unified_sub(const char *, int, 
                  rtx, rtx ,rtx, rtx, HOST_WIDE_INT, rtx);
static void ve_extract_base_offset_sub(rtx, rtx *,
                         rtx *, rtx *, HOST_WIDE_INT *);
static void ve_extract_base_offset(rtx, rtx *, 
                         rtx *, rtx *, HOST_WIDE_INT *);

/* Table of valid machine attributes.  */

static const struct attribute_spec ve_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
#ifdef SUBTARGET_ATTRIBUTE_TABLE
  SUBTARGET_ATTRIBUTE_TABLE,
#endif
  { NULL,        0, 0, false, false, false, NULL, false }
};

/* Initialize the GCC target structure.  */
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE ve_attribute_table

/* Abort after printing out a specific insn.  */

void
abort_with_insn (rtx insn, const char *reason)
{
  error (reason);
  debug_rtx (insn);
  abort ();
}

void
oops_message(char *str)
{
  fputs(str,stderr);
}

static void
ve_option_override (void)
{
  ve_const_indirect = TARGET_CONST_INDIRECT ? TRUE : FALSE;
  ve_symbol_indirect = TARGET_SYMBOL_INDIRECT ? TRUE : FALSE;

  if (flag_pie)
    {
      error("-fpie and -fPIE not supported on this target");
      flag_pie = 0;
    }
}

/* Do anything needed before RTL is emitted for each function.  */
static  int ve_use_linkage;
void
ve_init_expanders (void)
{
  if (ve_const_indirect || ve_symbol_indirect)
    ve_use_linkage = true;
  else
    ve_use_linkage = false;
}

/* Construct the SYMBOL_REF for GOT references.  */

static GTY(()) rtx ve_got_symbol = NULL_RTX;

static rtx
ve_got_sym (void)
{
  if (ve_got_symbol == NULL_RTX)
    {
      ve_got_symbol = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
      SYMBOL_REF_FLAGS (ve_got_symbol) |= SYMBOL_FLAG_EXTERNAL;
    }
  
  return ve_got_symbol;
}

/* Return true if X is a thread-local symbol.  */

static bool
ve_tls_symbol_p (rtx x)
{
  return GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x) != 0;
}

static rtx gen_tls_tga;

static rtx
gen_tls_get_addr (void)
{
  if (!gen_tls_tga)
    gen_tls_tga = init_one_libfunc ("__tls_get_addr");
  return gen_tls_tga;
}

int ve_hard_regno_mode_ok(unsigned int regno, machine_mode mode)
{
  /* modes that require multiple registers we choose to 
     always put in an even/odd pair. This is required on
     the FP side but not on the integer. We do it there anyway
     just to make things easier.  */

  if(GET_MODE_SIZE(mode) > UNITS_PER_WORD &&
     ((regno&1) != 0)) return 0;
  
  if(regno >= FIRST_S_REG && regno <= LAST_S_REG) 
    if (mode == QImode || mode == HImode || mode == SImode || mode == DImode 
	|| mode == CQImode || mode == CHImode || mode == CSImode || mode == CDImode
	|| mode == SFmode || mode == DFmode || mode == SCmode || mode == DCmode
	|| mode == TFmode || mode == TCmode || mode == TImode || mode == CTImode) 
      return 1;
  
  return 0;
}

/* Map internal gcc register numbers to DWARF2 register numbers.  */
unsigned int
ve_dbx_register_number (unsigned int regno)
{
  if (GP_REG_P(regno))
    return regno;
  return INVALID_REGNUM;
}

static bool
ve_scalar_mode_supported_p (machine_mode mode)
{
  if (mode == QImode || mode == HImode || mode == SImode || mode == DImode
      || mode == CQImode || mode == CHImode || mode == CSImode || mode == CDImode
      || mode == SFmode || mode == DFmode || mode == SCmode || mode == DCmode
      || mode == TFmode || mode == TCmode || mode == TImode || mode == CTImode)
    return true;
  return false;
}

static bool
ve_vector_mode_supported_p (machine_mode mode ATTRIBUTE_UNUSED)
{
  return false;
}

/* Return true if the registers in CLASS cannot represent the change from
   modes FROM to TO.  */

bool
ve_cannot_change_mode_class (machine_mode from, machine_mode to,
                             enum reg_class rclass ATTRIBUTE_UNUSED)
{
  if (from == to)
    return false;

  /* Reject changes to/from complex modes.  */
  if (COMPLEX_MODE_P (from) || COMPLEX_MODE_P (to))
    return true;
  
  if (GET_MODE_CLASS (from) == MODE_FLOAT || 
      GET_MODE_CLASS (to) == MODE_FLOAT)
    return true;

  /* We cannot use double word registers for subreg, 
     since aurora double word regs (especially TFmode) are 
     big-endian register numbering */
  if (GET_MODE_SIZE (to) > UNITS_PER_WORD ||
      GET_MODE_SIZE (from) > UNITS_PER_WORD)
    return true;
  
  if (GET_MODE_SIZE (from) == GET_MODE_SIZE (to))
    return false;
  
  return false;
}

/* Return true if MODE1 is accessible in a register that can hold MODE2
   without copying.  That is, all register classes that can hold MODE2
   can also hold MODE1.  */
bool
ve_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if (mode1 == mode2)
    return true;
  
  if (mode1 == TFmode || mode2 == TFmode)
    return false;
  
  if (mode1 == TImode || mode2 == TImode)
    return false;

  return true;
}
/* Implement TARGET_PRINT_OPERAND_PUNCT_VALID_P.  */

static bool
ve_print_operand_punct_valid_p (unsigned char code)
{
  return (code == '+' || code == '-');
}

void ve_print_operand(FILE *stream, rtx x, int letter)
{
  enum rtx_code code;
  
  switch (letter)
    {
    case '%':
      putc('%',stream);
      return;
    case '+': case '-':
      {
	rtx x;
	if (!optimize)
	  return;
	x = find_reg_note (current_output_insn, REG_BR_PROB, 0);
	if (x)
	  {
	    int pred_val = XINT (x, 0);
	    /* if 0.45 <= p <= 0.55 , no hint is generated */
	    if (pred_val < REG_BR_PROB_BASE * 45 / 100
		|| pred_val > REG_BR_PROB_BASE * 55 / 100)
	      {
		bool taken = pred_val > REG_BR_PROB_BASE / 2;
		bool cputaken
		  = final_forward_branch_p (current_output_insn) == 0;
		
		/* Emit hints in the case default branch prediction
		   heuristics would fail, or p is small or large enough  */
		if (taken != cputaken
		    || pred_val <= REG_BR_PROB_BASE * (100 - ve_branch_prob) / 100
		    || pred_val >= REG_BR_PROB_BASE * ve_branch_prob/ 100)
		  {
		    if ((taken && letter == '+') || (!taken && letter == '-') )
		      fputs (".t",stream);
		    else
		      fputs (".nt",stream);
		  }
	      }
	  }
      }
      return;
    }
  
  if(!x)
    error("PRINT_OPERAND null pointer");
  
  code = GET_CODE(x);
  
  switch (letter)
    {
    case 'C': /* conditional */
      switch(code)
        {
        case EQ:        fputs(VE_COND_EQ,stream); break;
        case NE:        fputs(VE_COND_NE,stream); break;
        case GT:        fputs(VE_COND_GT,stream); break;
        case GE:        fputs(VE_COND_GE,stream); break;
        case LT:        fputs(VE_COND_LT,stream); break;
        case LE:        fputs(VE_COND_LE,stream); break;
        case GTU:       fputs(VE_COND_GTU,stream); break;
        case GEU:       fputs(VE_COND_GEU,stream); break;
        case LTU:       fputs(VE_COND_LTU,stream); break;
        case LEU:       fputs(VE_COND_LEU,stream); break;
        default:
          abort_with_insn (x, "PRINT_OPERAND, illegal insn for %%C");
        }
      break;

    case'F':  /* floating conditional */
      switch(code)
        {
        case EQ:        fputs(VE_COND_EQ,stream); break;
        case NE:        fputs(VE_COND_FNE,stream); break;
        case GT:        fputs(VE_COND_GT,stream); break;
        case GE:        fputs(VE_COND_GE,stream); break;
        case LT:        fputs(VE_COND_LT,stream); break;
        case LE:        fputs(VE_COND_LE,stream); break;
        case GTU:       fputs(VE_COND_GTU,stream); break;
        case GEU:       fputs(VE_COND_GEU,stream); break;
        case LTU:       fputs(VE_COND_LTU,stream); break;
        case LEU:       fputs(VE_COND_LEU,stream); break;
        case UNEQ:      fputs(VE_COND_UNEQ,stream); break;
        case LTGT:      fputs(VE_COND_LTGT,stream); break;
        case UNGT:      fputs(VE_COND_UNGT,stream); break;
        case UNGE:      fputs(VE_COND_UNGE,stream); break;
        case UNLT:      fputs(VE_COND_UNLT,stream); break;
        case UNLE:      fputs(VE_COND_UNLE,stream); break;
        case UNORDERED: fputs(VE_COND_UNORDERED,stream); break;
        case ORDERED:   fputs(VE_COND_ORDERED,stream); break;
        default:
          abort_with_insn (x, "PRINT_OPERAND, illegal insn for %%C");
        }
      break;
    
    case 'N': /* negate conditional */
      switch(code)
        {
        case EQ:        fputs(VE_COND_NE,stream); break;
        case NE:        fputs(VE_COND_EQ,stream); break;
        case GT:        fputs(VE_COND_LE,stream); break;
        case GE:        fputs(VE_COND_LT,stream); break;
        case LT:        fputs(VE_COND_GE,stream); break;
        case LE:        fputs(VE_COND_GT,stream); break;
        case GTU:       fputs(VE_COND_LEU,stream); break;
        case GEU:       fputs(VE_COND_LTU,stream); break;
        case LTU:       fputs(VE_COND_GEU,stream); break;
        case LEU:       fputs(VE_COND_GTU,stream); break;
        default:
          abort_with_insn (x, "PRINT_OPERAND, illegal insn for %%N");
        }
      break;

    case 'E': /* floating negate conditional */
      switch(code)
        {
        case EQ:        fputs(VE_COND_FNE,stream); break;
        case NE:        fputs(VE_COND_EQ,stream); break;
        case GT:        fputs(VE_COND_UNLE,stream); break;
        case GE:        fputs(VE_COND_UNLT,stream); break;
        case LT:        fputs(VE_COND_UNGE,stream); break;
        case LE:        fputs(VE_COND_UNGT,stream); break;
        case GTU:       fputs(VE_COND_UNLE,stream); break;
        case GEU:       fputs(VE_COND_UNLT,stream); break;
        case LTU:       fputs(VE_COND_UNGE,stream); break;
        case LEU:       fputs(VE_COND_UNGT,stream); break;
        case UNEQ:      fputs(VE_COND_LTGT,stream); break;
        case LTGT:      fputs(VE_COND_UNEQ,stream); break;
        case UNGT:      fputs(VE_COND_LE,stream); break;
        case UNGE:      fputs(VE_COND_LT,stream); break;
        case UNLT:      fputs(VE_COND_GE,stream); break;
        case UNLE:      fputs(VE_COND_GT,stream); break;
        case UNORDERED: fputs(VE_COND_ORDERED,stream); break;
        case ORDERED:   fputs(VE_COND_UNORDERED,stream); break;
        default:
          abort_with_insn (x, "PRINT_OPERAND, illegal insn for %%N");
        }
      break;

    case 'R': /* reverse (swap operand) conditional */
      switch(code)
        {
        case EQ:        fputs(VE_COND_EQ,stream); break;
        case NE:        fputs(VE_COND_FNE,stream); break;
        case GT:        fputs(VE_COND_LT,stream); break;
        case GE:        fputs(VE_COND_LE,stream); break;
        case LT:        fputs(VE_COND_GT,stream); break;
        case LE:        fputs(VE_COND_GE,stream); break;
        case GTU:       fputs(VE_COND_LTU,stream); break;
        case GEU:       fputs(VE_COND_LEU,stream); break;
        case LTU:       fputs(VE_COND_GTU,stream); break;
        case LEU:       fputs(VE_COND_GEU,stream); break;
        case UNEQ:      fputs(VE_COND_UNEQ,stream); break;
        case LTGT:      fputs(VE_COND_LTGT,stream); break;
        case UNGT:      fputs(VE_COND_UNLT,stream); break;
        case UNGE:      fputs(VE_COND_UNLE,stream); break;
        case UNLT:      fputs(VE_COND_UNGT,stream); break;
        case UNLE:      fputs(VE_COND_UNGE,stream); break;
        case UNORDERED: fputs(VE_COND_UNORDERED,stream); break;
        case ORDERED:   fputs(VE_COND_ORDERED,stream); break;
        default:
          abort_with_insn (x, "PRINT_OPERAND, illegal insn for %%R");
        }
      break;
      
    case 'T': /* reverse (swap operands) and negate conditional */
      switch(code)
        {
        case EQ:        fputs(VE_COND_NE,stream); break;
        case NE:        fputs(VE_COND_EQ,stream); break;
        case GT:        fputs(VE_COND_GE,stream); break;
        case GE:        fputs(VE_COND_GT,stream); break;
        case LT:        fputs(VE_COND_LE,stream); break;
        case LE:        fputs(VE_COND_LT,stream); break;
        case GTU:       fputs(VE_COND_GEU,stream); break;
        case GEU:       fputs(VE_COND_GTU,stream); break;
        case LTU:       fputs(VE_COND_LEU,stream); break;
        case LEU:       fputs(VE_COND_LTU,stream); break;
        default:
          abort_with_insn (x, "PRINT_OPERAND, illegal insn for %%T");
        }
      break;
    
    case 'J': /* remove eq condition*/
      switch(code)
        {
        /*case EQ:        fputs(VE_COND_NE,stream); break;*/
        /*case NE:        fputs(VE_COND_NE,stream); break;*/
        case GT:        fputs(VE_COND_GT,stream); break;
        case GE:        fputs(VE_COND_GT,stream); break;
        case LT:        fputs(VE_COND_LT,stream); break;
        case LE:        fputs(VE_COND_LT,stream); break;
        case GTU:       fputs(VE_COND_GTU,stream); break;
        case GEU:       fputs(VE_COND_GTU,stream); break;
        case LTU:       fputs(VE_COND_LTU,stream); break;
        case LEU:       fputs(VE_COND_LTU,stream); break;
        default:
          abort_with_insn (x, "PRINT_OPERAND, illegal insn for %%T");
        }
      break;
    
    case 'M': /* special (n)1 or (n)0 notation */
      {
        long long iv = INTVAL(x);
        int m, n;
        if (iv == 0) 
          fputs("(0)1",stream);
        else if (iv == -1) 
          fputs("(0)0",stream);
        else {
          if ((iv & (iv +1)) == 0) {
	    n = 0;
	  } 
          else {
	    n = 1;
	  }
          for (m=64; (iv&1) == 1-n && m >=0 ; iv  >>= 1) m--;  
          fprintf(stream,"(%d)%d",m,n);
        }
      }
      break;
      
    case 'O': case'P': 
      /* special (n)1 or (n)0 notation 32bit*/
      {
        long long iv = INTVAL(x);
        int m, n;
        if (iv & 0x80000000) iv |= 0xffffffff00000000;
        if (iv == 0) 
          fputs("(0)1",stream);
        else if (iv == -1) 
          fputs("(0)0",stream);
        else {
          if ((iv & (iv +1)) == 0) {
	    n = 0;
	  } 
          else {
	    n = 1;
	  }
          for (m=64; (iv&1) == 1-n && m >=0 ; iv  >>= 1) m--;  
          fprintf(stream,"(%d)%d",m,n);
        }
      }
      break;
    
    case 'S': /* print SYMBOL_REF */
      assemble_name(stream,XSTR(x,0));
      break;


    case 'U': /* Unsigned int */
      {
        unsigned long iv = (unsigned)(INTVAL(x));
        fprintf(stream,"%ld",iv);
      }
      break;
      
    case 'G': /* siGned int */
      {
        long iv = INTVAL(x);
        fprintf(stream,"%ld",iv);
      }
      break;

    case 'L': /* int low 32bit */
      {
	long long iv = INTVAL(x);
	int l = (int)(iv & 0xffffffff);
	fprintf(stream,"%d",l );
      }
      break;

    case 'H': /* int high 32bit + 1(if low 32bit <0) */
      {
	long long iv = INTVAL(x);
	int l = (int)(iv & 0xffffffff);
	fprintf(stream,"%d",(int)(iv >> 32)+(l>=0?0:1));
      }
      break;

    case 'V': /* lower 5 bit for shift SI operation */
      {
        unsigned long iv = (unsigned) (INTVAL(x));
        fprintf(stream,"%d",(int)(iv & 0x1f));
      }
      break;

    case 'Z': /* x >=0 ? '0' : '-1' */
      {
        long iv = INTVAL(x);
        if (iv >=0)
	  fputs("0",stream);
        else
	  fputs("-1",stream);
      }
      break;


    default:
      switch (code) {
      case REG:
	{
	  int regnum = REGNO(x);
	  if (letter=='Q') 
	    {
	      gcc_assert(regnum %2 == 0); 
	      regnum++;
	    }
	  fputs(reg_names[regnum],stream);
	}
	break;
	
      case CONST_INT:
	fprintf(stream,HOST_WIDE_INT_PRINT_DEC,INTVAL(x));
	break;
	
      case MEM:
	if (letter == 'A') 
	  output_address(GET_MODE(x), 
			 plus_constant(Pmode,XEXP(x,0),UNITS_PER_WORD));
	else
	  output_address(GET_MODE(x), XEXP(x, 0));
	
	break;
	
      default:
	output_addr_const(stream,x);
      }
    }
}

void
ve_print_operand_address (FILE *file, machine_mode /*mode*/, rtx addr)
{
  int spill = 0;
  HOST_WIDE_INT offset = 0;
  
  if (!addr)
    fatal_error (input_location,
		 "internal error:PRINT_OPERAND_ADDRESS, null pointer");
  
  if (GET_CODE(addr) == PLUS && GET_CODE(XEXP(addr,1)) == CONST_INT)  
    {
      offset = INTVAL(XEXP(addr,1));
      addr = XEXP(addr,0);
    }
  switch (GET_CODE (addr))
    {
    default:
      abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #1");
      break;

    case REG:
      fprintf (file, HOST_WIDE_INT_PRINT_DEC
	       "(,%s)", offset, reg_names [REGNO (addr)]);
      break;

    case PLUS:
      {
	rtx x1 = XEXP(addr, 0);
	rtx x2 = XEXP(addr, 1);
	
   /* try swapping */
	if (! REG_P(x1) && REG_P(x2))
	  {
	    rtx tmp = x1; x1 = x2; x2 = tmp;
	  }
	if (CONSTANT_P (x1) && CONSTANT_P (x2))
	  {
	    if (GET_CODE(x1) == CONST_INT) 
	      {
		if (GET_CODE(x2) == CONST_INT) 
		  {
		    offset += INTVAL(x1) + INTVAL(x2);
		    fprintf (file, HOST_WIDE_INT_PRINT_DEC, offset);
		  }
		else 
		  {
		    offset += INTVAL(x1);
		    output_addr_const (file, x2);
		    if (offset != 0)
		      fprintf (file, "+" HOST_WIDE_INT_PRINT_DEC, offset);
		  }
	      }
	    else if (GET_CODE(x2) == CONST_INT)
	      {
		offset += INTVAL(x2);
		output_addr_const (file, x1);
		if (offset !=0)
		  fprintf (file, "+" HOST_WIDE_INT_PRINT_DEC, offset);
	      }
	    else
	      {
		output_addr_const (file, addr);
		if (offset !=0)
		  fprintf (file, "+" HOST_WIDE_INT_PRINT_DEC, offset);
	      }
	    break;
	  }
	
	if (REG_P(x1))
	  {
	    if (REG_P(x2))
	      {
		fprintf(file, HOST_WIDE_INT_PRINT_DEC "(%s,%s)",
			offset, reg_names[REGNO(x1)],
			reg_names[REGNO(x2)]);
		break;
	      }
	    
	    if (CONSTANT_P(x2))  
	      {
		ve_output_addr_const_offset(file,x2,offset);
		if (GET_CODE(x2) == LABEL_REF) 
		  {
		    fprintf (file, "(%s,%s)",
			     reg_names [REGNO (x1)],
			     reg_names [VE_LINKAGE_REGNUM]);
		    ve_use_linkage = true;
		  }
		else
		  fprintf(file, "(,%s)", reg_names[REGNO(x1)]);
		break;
	      }
	    
	    if (GET_CODE(x2) == PLUS)
	      {
		rtx x3 = XEXP(x2,0);
		rtx x4 = XEXP(x2,1);
		
		if (! REG_P(x3))
		  {
		    rtx tmp = x3; x3 = x4; x4 = tmp;
		  }
		if (REG_P(x3) && CONSTANT_P(x4)) 
		  {
		    ve_output_addr_const_offset(file,x4,offset);
		    fprintf(file, "(%s,%s)",
			    reg_names[REGNO(x1)], reg_names[REGNO(x3)]);
		    break;
		  }
		else
		  abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #1");
	      }
	  } /* end REG_P(x1) */
	
	if (GET_CODE(x1) != PLUS && GET_CODE(x2) == PLUS)
	  {
	    rtx tmp = x1; x1 = x2; x2 = tmp;
	  }
	if (GET_CODE(x1) == PLUS)
	  {
	    rtx x3 = XEXP(x1,0);
	    rtx x4 = XEXP(x1,1);
	    
	    if (REG_P(x3) && REG_P(x4) && CONSTANT_P(x2)) 
	      {
		ve_output_addr_const_offset(file, x2, offset);
		fprintf(file, "(%s,%s)",
			reg_names[REGNO(x3)], reg_names[REGNO(x4)]);
		break;
	      }
	    else
	      abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #2");
	  }
	abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #3");
	break;
      }

    case SYMBOL_REF:
      ve_output_addr_const_offset (file, addr, offset);
      if (!CONSTANT_POOL_ADDRESS_P(addr)) break;
      addr = get_pool_constant(addr);
      switch (GET_CODE(addr)) {
      case SYMBOL_REF:
	fprintf(file,"\t# ");
	output_addr_const(file,addr);
	break;
      case CONST_DOUBLE:
	{
          char string[30];
          real_to_decimal(string, CONST_DOUBLE_REAL_VALUE(addr),
			  sizeof(string),0,1);

          fprintf(file,"\t# %s",string);
          break;
	}
      default:
	break;
      }
      break;
    case LABEL_REF:
    case CONST_INT:
    case CONST:
      ve_output_addr_const_offset (file, addr, offset);
      break;
    }
}

static void
ve_output_addr_const_offset(FILE *file, rtx addr, HOST_WIDE_INT offset)
{
  gcc_assert(CONSTANT_P(addr));
  if (GET_CODE(addr) == CONST_INT)
    {
      offset += INTVAL(addr);
      fprintf(file, HOST_WIDE_INT_PRINT_DEC, offset);
    }
  else
    {
      output_addr_const (file, addr);
      if (offset !=0) 
	fprintf(file, "+" HOST_WIDE_INT_PRINT_DEC, offset);
    }
}

static void
ve_extract_base_offset_sub(rtx addr, rtx *sym, rtx *reg1, rtx *reg2,
                             HOST_WIDE_INT *offset)
{
  switch (GET_CODE (addr))
    {
    case CONST:
      ve_extract_base_offset_sub(XEXP(addr,0),sym,reg1,reg2,offset);
      break;
      
    case REG:
      if (*reg1 == NULL_RTX)
        *reg1 = addr;
      else if (*reg2 == NULL_RTX)
        *reg2 = addr;
      else
        abort_with_insn (addr, "EXTRACT_BASE_OFFSET, invalid insn");
      break;
      
    case SYMBOL_REF:
    case LABEL_REF:
      if (*sym == NULL_RTX)
        *sym = addr;
      else
        abort_with_insn (addr, "EXTRACT_BASE_OFFSET, invalid insn");
      break;
      
    case CONST_INT:
      *offset += INTVAL(addr);
      break;
      
    case PLUS:
      ve_extract_base_offset_sub(XEXP(addr,0),sym,reg1,reg2,offset);
      ve_extract_base_offset_sub(XEXP(addr,1),sym,reg1,reg2,offset);
      break;
      
    case MEM:
      if (*reg1 == NULL_RTX)
        *reg1 = gen_rtx_REG(GET_MODE(addr),VE_SCRATCH_REGNUM);
      else if (*reg2 == NULL_RTX)
        *reg2 = gen_rtx_REG(GET_MODE(addr),VE_SCRATCH_REGNUM);
      else
        abort_with_insn (addr, "EXTRACT_BASE_OFFSET, invalid insn");
      break;
      
    default:
      abort_with_insn (addr, "EXTRACT_BASE_OFFSET, invalid insn");
    }
}

static void 
ve_extract_base_offset(rtx addr, rtx *sym, rtx *reg1, rtx *reg2,
                             HOST_WIDE_INT *offset)
{
  gcc_assert (MEM_P (addr));
  addr = XEXP (addr, 0);

  *sym = NULL_RTX;
  *reg1 = NULL_RTX;
  *reg2 = NULL_RTX;
  *offset = 0;
  ve_extract_base_offset_sub(addr,sym,reg1,reg2,offset);
}

int
ve_regno_mode_ok_for_base_p (int regno, 
			     machine_mode mode ATTRIBUTE_UNUSED, bool strict_p)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict_p)
        return true;
      regno = reg_renumber[regno];
    }
  
  if (regno == ARG_POINTER_REGNUM || regno == STACK_POINTER_REGNUM)
    return true;
  
  return GP_REG_P(regno);
}


/* This function is used to implement GO_IF_LEGITIMATE_ADDRESS.  It
   returns a nonzero value if X is a legitimate address for a memory
   operand of the indicated MODE.  STRICT is nonzero if this function
   is called during reload.  */

static bool
ve_reg_ok_for_legitimate(rtx x, bool strict)
{
  return strict? REG_OK_FOR_BASE_STRICT_P(x):REG_OK_FOR_BASE_NONSTRICT_P(x);
}

bool
ve_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED ,
                     rtx x, bool strict)
{
  /* We only accept:
     (mem reg)
     (mem (plus reg reg))
     (mem (plus reg SIconst))
     (mem (plus reg (plus reg SIconst)))
     (mem (plus (plus reg reg) SIconst))
     (mem symbol:local) 
  */

  if (REG_P(x))  
    return ve_reg_ok_for_legitimate(x,strict);
	
  if (GET_CODE(x) == MEM) return false;

  if (GET_CODE(x) == PLUS) 
    {
      rtx x1 = XEXP(x, 0);
      rtx x2 = XEXP(x, 1);
      
      if (GET_CODE(x1) == MEM) return false;
      if (GET_CODE(x2) == MEM) return false;

      /* try swapping */
      if (! REG_P(x1) && REG_P(x2)) 
	{    
	  rtx tmp = x1; x1 = x2; x2 = tmp;
	} 
      
      if (REG_P(x1))
	{
	  if (!ve_reg_ok_for_legitimate(x1,strict)) return false;
	  
	  if (REG_P(x2)) 
	    return ve_reg_ok_for_legitimate(x2,strict);
	  
	  if (GET_CODE(x2) == CONST_INT)
	    return true;
	  
	  if (GET_CODE(x2) == PLUS) 
	    {
	      rtx x3 = XEXP(x2,0);
	      rtx x4 = XEXP(x2,1);         

	      if (GET_CODE(x3) == MEM) return false;
	      if (GET_CODE(x4) == MEM) return false;

	      /* try swapping */
	      if (! REG_P(x3)) 
		{    
		  rtx tmp = x3; x3 = x4; x4 = tmp;
		}
	      if (!REG_P(x3)) return false;
	      if (!ve_reg_ok_for_legitimate(x3,strict)) return false;
	      if (GET_CODE(x4) == CONST_INT) return true;
	      return false;
	    }
	}

      if (GET_CODE(x1) != PLUS && GET_CODE(x2) == PLUS) 
	{    
	  rtx tmp = x1; x1 = x2; x2 = tmp;
	} 
      if (GET_CODE(x1) == PLUS)
	{
	  rtx x3 = XEXP(x1,0); 
	  rtx x4 = XEXP(x1,1); 

	  if (GET_CODE(x3) == MEM) return false;
	  if (GET_CODE(x4) == MEM) return false;
	  if (!REG_P(x3)) return false;
	  if (!ve_reg_ok_for_legitimate(x3,strict)) return false;
	  if (!REG_P(x4)) return false;
	  if (!ve_reg_ok_for_legitimate(x4,strict)) return false;
	  if (GET_CODE(x2) == CONST_INT) return true;
	  return false;
	}
    }
  
  /* pic investigation */
  if (GET_CODE(x) == SYMBOL_REF 
      && CONSTANT_POOL_ADDRESS_P(x)) 
    return true;
  
  return false;
}

/* return num-th appearence of mem using depth first search */

int gnum;
rtx grtx;

rtx
ve_addr_cut_mem(rtx x, int num)
{
  gnum = num;
  grtx = NULL_RTX;
  gcc_assert(GET_CODE(x) == MEM);
  ve_addr_cut_mem_internal(XEXP(x,0));
  return grtx;
}

static void
ve_addr_cut_mem_internal(rtx x) 
{
  if (gnum <= 0) return;
  
  if (REG_P(x)) 
    return;
  
  if (GET_CODE(x) == MEM)
    {
      ve_addr_cut_mem_internal(XEXP(x,0)); 
      if (--gnum == 0) grtx = x;
       return;
    }

  if (GET_CODE(x) == PLUS) 
    {
      ve_addr_cut_mem_internal(XEXP(x,0));
      if (gnum <= 0) return;
      ve_addr_cut_mem_internal(XEXP(x,1));
      return;
    }

  return ;
}

/* Determine if it's legal to put X into the constant pool.  This
   is not possible if X contains the address of a symbol that is
   not constant (TLS) or not known at final link time (PIC).  */

static bool
ve_cannot_force_const_mem (machine_mode mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
      /* Accept all non-symbolic constants.  */
      return flag_pic || !ve_const_indirect;
      
    case LABEL_REF:
      /* Labels are OK iff we are non-PIC.  */
      return flag_pic || !ve_symbol_indirect ;
      
    case SYMBOL_REF:
      /* 'Naked' TLS symbol references are never OK,
         non-TLS symbols are OK iff we are non-PIC.  */
      if (SYMBOL_REF_TLS_MODEL (x))
        return true;
      else
        return flag_pic || !ve_symbol_indirect;

    case CONST:
    case HIGH:
      return ve_cannot_force_const_mem (mode, XEXP (x, 0));
    case PLUS:
    case MINUS:
      return ve_cannot_force_const_mem (mode, XEXP (x, 0))
         || ve_cannot_force_const_mem (mode, XEXP (x, 1));
    case LO_SUM:
      return ve_cannot_force_const_mem (mode, XEXP (x, 1));
    case REG:
      return false;
    case UNSPEC:
      return true;
    default:
      debug_rtx(x);
      gcc_unreachable ();
    }
}

/* Return true if X is a symbolic constant that can be calculated in
   the same way as a bare symbol.  If it is, store the type of the
   symbol in *SYMBOL_TYPE.  */

bool
ve_symbolic_constant_p (rtx x)
{
  HOST_WIDE_INT offset;

  ve_split_const (x, &x, &offset);

  if (GET_CODE(x) == SYMBOL_REF && 
      CONSTANT_POOL_ADDRESS_P(x)) return true;
  if (GET_CODE(x) == LABEL_REF) return true;
  return false;
}

/* Split X into a base and a constant offset, storing them in *BASE
   and *OFFSET respectively.  */

static void
ve_split_const (rtx x, rtx *base, HOST_WIDE_INT *offset)
{
  *offset = 0;

  if (GET_CODE (x) == CONST)
    {
      x = XEXP (x, 0);
      if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == CONST_INT)
        {
          *offset += INTVAL (XEXP (x, 1));
          x = XEXP (x, 0);
        }
    }
  *base = x;
}

/* This function is used to implement LEGITIMIZE_ADDRESS.  If *XLOC can
   be legitimized in a way that the generic machinery might not expect,
   put the new address in *XLOC and return true.  MODE is the mode of
   the memory being accessed.  */

rtx
ve_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
		machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (x) == PLUS) 
    {
      rtx x1 = XEXP(x,0);
      rtx x2 = XEXP(x,1);
      if (GET_CODE(x1) == CONST_INT)
	{
	  rtx temp;
	  temp = x1; x1 = x2; x2 = temp;
	}
      if (GET_CODE(x1) == SYMBOL_REF 
	  && !CONSTANT_POOL_ADDRESS_P(x1)
	  && GET_CODE(x2) == CONST_INT)
	{ 
	  rtx tmp,tmpreg;
	  tmpreg = ve_indirect_addr(x1,false,false);
	  tmp = gen_rtx_PLUS(Pmode,tmpreg,x2);
	  return(tmp);
	}
    }

  if (GET_CODE (x) == SYMBOL_REF &&
      !CONSTANT_POOL_ADDRESS_P(x)) 
    {
      rtx tmpreg;
      tmpreg = ve_indirect_addr(x,false,false);
      return(tmpreg);
    }
  
  return x;
}


/* Likewise for constant X.  */

static bool
ve_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  if (ve_tls_symbol_p (x))
    return false;
  
  return ve_const_insns (x) > 0;
}


static int
ve_const_insns (rtx x)
{
#define VE_IMMEDIATE_INSTR 2
  switch (GET_CODE (x))
    {
    case CONST_INT:
      if (ve_const_si_p(INTVAL(x))) 
        return 1;
      else
	{
/* accept const > 32bit */
	  if (flag_pic || !ve_const_indirect) 
	    return VE_IMMEDIATE_INSTR;
	  return 0;
	}

    case CONST_DOUBLE:
    case CONST_VECTOR:
      if (x == CONST0_RTX (GET_MODE(x)))
        return 1;
      if (GET_MODE_SIZE(GET_MODE(x)) < UNITS_PER_WORD)
        return 1;
      if (flag_pic || !ve_const_indirect)
        return VE_IMMEDIATE_INSTR;
      return 0;
    case CONST:
      /* See if we can refer to X directly.  */
      if (ve_symbolic_constant_p (x))
      /*  return ve_symbol_insns (symbol_type); */
	return VE_IMMEDIATE_INSTR;

      return 0;

    case SYMBOL_REF:
/* let optimizer do not revert indirect address */
      if (!CONSTANT_POOL_ADDRESS_P(x)) return 0;
      return VE_IMMEDIATE_INSTR;
    case LABEL_REF:
      return VE_IMMEDIATE_INSTR;

    default:
      return 0;
    }
}

/* Worker function for TARGET_REGISTER_MOVE_COST.  */

static int
ve_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
		       reg_class_t from ATTRIBUTE_UNUSED,
		       reg_class_t to ATTRIBUTE_UNUSED)
{
  return 1;
}

/* Worker function for TARGET_MEMORY_MOVE_COST.  */

static int
ve_memory_move_cost (machine_mode mode,
		     reg_class_t rclass ATTRIBUTE_UNUSED,
		     bool in ATTRIBUTE_UNUSED)
{
  return (GET_MODE_SIZE(mode)+UNITS_PER_WORD-1)/ UNITS_PER_WORD *10;
}

static bool
ve_rtx_costs (rtx x, machine_mode mode ATTRIBUTE_UNUSED,
              int outer_code ATTRIBUTE_UNUSED,
              int opno ATTRIBUTE_UNUSED,
              int *total, bool speed ATTRIBUTE_UNUSED)
{
  enum rtx_code code = GET_CODE(x);
  switch (code)
    {
    case CONST_INT:
      /* Make 6-bit integers really cheap.  */
      if (IN_RANGE (INTVAL (x), -64, 63))
        {
          *total = 0;
          return true;
        }
      /* Fall through.  */

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
      *total = COSTS_N_INSNS (2);
      return true;

    case PLUS:
    case MINUS:
    case AND:
    case IOR:
    case XOR:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case NOT:
    case NEG:
    case COMPARE:
      if (GET_MODE (x) == SImode || GET_MODE(x) == DImode)
        *total = COSTS_N_INSNS (1);
      else
        *total = COSTS_N_INSNS (3);
      return true;

    case MULT:
      if (GET_MODE (x) == SImode || GET_MODE(x) == DImode)
        *total = COSTS_N_INSNS (2);
      else
        *total = COSTS_N_INSNS (6);  
      return true;
    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
        *total = COSTS_N_INSNS (18);
      return true;

    case MEM:
        *total = COSTS_N_INSNS (10);
        return true;
    default:
      return false;
    }
}

static void
ve_trampoline_template (FILE * file)
{
  /* Output assembler code for a block containing the constant
     part of a trampoline, leaving space for the variable parts.

     On the VE, (where s63 is the static chain regnum) the trampoline
     looks like:

           ld    %s63,Lstatic_chain(,%s12)
           ld    %s12,Lfunction_address(,%s12)
           b.l.t (,%s12)
     Lstatic_chain:
           .quad static_chain
     Lfunction_address:
           .quad function_address


     On the SX, (where s121 is the static chain regnum) the trampoline
     looks like:

           lds   $s121,Lstatic_chain(,$s120)
           lds   $s120,Lfunction_address(,$s120)
           be>   0,(,$s120)
      Lstatic_chain:
           llong static_chain
     Lfunction_address:
           llong function address

  */
  asm_fprintf (file, "\tld\t%s,24(,%s)\n", reg_names[STATIC_CHAIN_REGNUM],
	       reg_names[VE_SCRATCH_REGNUM]);
  asm_fprintf (file, "\tld\t%s,32(,%s)\n", reg_names[VE_SCRATCH_REGNUM],
	       reg_names[VE_SCRATCH_REGNUM]);
  asm_fprintf (file, "\tb.l.t\t(,%s)\n",reg_names[VE_SCRATCH_REGNUM]);
  asm_fprintf (file, ASM_LONGLONG "0\n");
  asm_fprintf (file, ASM_LONGLONG "0\n");
}

static void
ve_trampoline_init (rtx tramp, tree fndecl, rtx chain)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);

  emit_block_move (tramp, assemble_trampoline_template (),
                   GEN_INT (24), BLOCK_OP_NORMAL);

  emit_move_insn (adjust_address (tramp, DImode, 24), chain);
  emit_move_insn (adjust_address (tramp, DImode, 32), fnaddr);
  emit_insn (gen_flush_icache ());

}

static long l32 (long l)
{
  return l & 0xffffffff;
}

/* Generate movesf constant */
const char *
ve_movsf_reg_immediate (rtx *operands)
{
  REAL_VALUE_TYPE d;
  long l;
  char buf[VE_BUF_SIZE];
  char string[30];

  d = *CONST_DOUBLE_REAL_VALUE(operands[1]);
  REAL_VALUE_TO_TARGET_SINGLE(d,l);  
  real_to_decimal(string, &d, sizeof(string),0,1);

  snprintf(buf,VE_BUF_SIZE,"lea.sl\t%%0,0x%08lx\t# %s",
           l32(l),string);
  output_asm_insn(buf,operands);
  return "";
}

/* Generate movedf constant */
const char *
ve_movdf_reg_immediate (rtx *operands)
{
  rtx xop[3];
  REAL_VALUE_TYPE d;
  long l[2];
  int ll;
  char buf[VE_BUF_SIZE];
  char string[30];

  d = *CONST_DOUBLE_REAL_VALUE(operands[1]);
  REAL_VALUE_TO_TARGET_DOUBLE(d,l);  
  real_to_decimal(string, &d, sizeof(string),0,1);

  xop[0] = operands[0];
  xop[1] = operands[1];
  xop[2] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);

  ll = l32(l[WORDS_BIG_ENDIAN?1:0]);
  if (ll == 0)
    {
      snprintf(buf,VE_BUF_SIZE,"lea.sl\t%%0,0x%08lx\t# %s",
	       l32(l[WORDS_BIG_ENDIAN?0:1]),string);
      output_asm_insn(buf,xop);
    }
  else
    {
      snprintf(buf,VE_BUF_SIZE,"lea\t%%2,0x%08lx",
	       l32(l[WORDS_BIG_ENDIAN?1:0]));
      output_asm_insn(buf,xop);
      snprintf(buf,VE_BUF_SIZE,"lea.sl\t%%0,0x%08lx(%%2)\t# %s",
	       l32(l[WORDS_BIG_ENDIAN?0:1]+(ll>=0?0:1)),string);
      output_asm_insn(buf,xop);
    }
  return "";
}  

/* Generate movetf constant */
const char *
ve_movtf_reg_immediate (rtx *operands)
{
  rtx xop[3];
  REAL_VALUE_TYPE d;
  long l[4];
  int ll;
  char buf[VE_BUF_SIZE];
  char string[30];

  d = *CONST_DOUBLE_REAL_VALUE(operands[1]);
  REAL_VALUE_TO_TARGET_LONG_DOUBLE(d,l);  
  real_to_decimal(string, &d, sizeof(string),0,1);

  xop[0] = operands[0];
  xop[1] = operands[1];
  xop[2] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
  
  ll = l32(l[WORDS_BIG_ENDIAN?1:2]);
  if (ll == 0) 
    {
      snprintf(buf,VE_BUF_SIZE,"lea.sl\t%%0,0x%08lx",
	       l32(l[WORDS_BIG_ENDIAN?0:3]));
      output_asm_insn(buf,xop);
    }
  else
    {
      snprintf(buf,VE_BUF_SIZE,"lea\t%%2,0x%08lx",
	       l32(l[WORDS_BIG_ENDIAN?1:2]));
      output_asm_insn(buf,xop);
      snprintf(buf,VE_BUF_SIZE,"lea.sl\t%%0,0x%08lx(%%2)",
	       l32(l[WORDS_BIG_ENDIAN?0:3]+(ll>=0?0:1)));
      output_asm_insn(buf,xop);
    }
  
  ll = l32(l[WORDS_BIG_ENDIAN?3:0]);
  if (ll == 0)
    {
      snprintf(buf,VE_BUF_SIZE,"lea.sl\t%%Q0,0x%08lx\t# %s",
	       l32(l[WORDS_BIG_ENDIAN?2:1]),string);
      output_asm_insn(buf,xop);
    }
  else
    {
      snprintf(buf,VE_BUF_SIZE,"lea\t%%2,0x%08lx",
	       l32(l[WORDS_BIG_ENDIAN?3:0]));
      output_asm_insn(buf,xop);
      snprintf(buf,VE_BUF_SIZE,"lea.sl\t%%Q0,0x%08lx(%%2)\t# %s",
	       l32(l[WORDS_BIG_ENDIAN?2:1]+(ll>=0?0:1)),string);
      output_asm_insn(buf,xop);
    }
  return "";
}  

const char*
ve_asm_output_ldst_unified(const char *ldst, int direction, rtx* operands,
                           rtx operand_aid) 
{
  int pos=1;
  rtx operand_mem;
  rtx operand_reg;
  rtx xop[2];
  rtx sym;
  rtx base1;
  rtx base2;
  HOST_WIDE_INT offset;
  switch (direction)
    {
    case VE_DIR_LOAD: case VE_DIR_QLOAD:
      operand_reg = operands[0];
      operand_mem = operands[1];
      break;
    case VE_DIR_STORE: case VE_DIR_QSTORE:
      operand_mem = operands[0];
      operand_reg = operands[1];
      break;
    case VE_DIR_PFCH:
      operand_mem = operands[0];
      operand_reg = NULL_RTX;
      break;
    case VE_DIR_ATOMIC01:
      operand_reg = operands[0];
      operand_mem = operands[1];
      break;
    case VE_DIR_ATOMIC20:
      operand_reg = operands[2];
      operand_mem = operands[0];
      break;
    default:
      gcc_unreachable();
    }

  ve_extract_base_offset(operand_mem,&sym,&base1,&base2,&offset);
  switch (direction)
    {
    case VE_DIR_QLOAD:
      ve_asm_output_ldst_unified_sub(ldst,VE_DIR_QLOAD1,
				     operand_reg,sym,base1,base2,offset,operand_aid) ;
      ve_asm_output_ldst_unified_sub(ldst,VE_DIR_QLOAD2,
				     operand_reg,sym,base1,base2,offset,operand_aid) ;
      break;
    case VE_DIR_QSTORE:
      ve_asm_output_ldst_unified_sub(ldst,VE_DIR_QSTORE1,
				     operand_reg,sym,base1,base2,offset,operand_aid) ;
      ve_asm_output_ldst_unified_sub(ldst,VE_DIR_QSTORE2,
				     operand_reg,sym,base1,base2,offset,operand_aid) ;
      break;
    default:
      ve_asm_output_ldst_unified_sub(ldst,direction,
				     operand_reg,sym,base1,base2,offset,operand_aid) ;
      break;
  }
  return ""; 
}
  
static void
ve_asm_output_ldst_unified_sub(const char *ldst, int direction, 
			       rtx operand_reg, rtx sym,
			       rtx base1, rtx base2, HOST_WIDE_INT offset,
			       rtx operand_aid)
{
  rtx xop[6];
  char buf[VE_BUF_SIZE];
  const char *form0;
  const char *form31;
  const char *form4;
  switch (direction)
    {
    case VE_DIR_QLOAD1:
      form0 = "%Q0,";
      direction = VE_DIR_LOAD;
      break;
    case VE_DIR_QLOAD2:
      form0 = "%0,";
      offset += UNITS_PER_WORD;
      direction = VE_DIR_LOAD;
      break;
    case VE_DIR_QSTORE1:
      form0 = "%Q0,";
      direction = VE_DIR_STORE;
      break;
    case VE_DIR_QSTORE2:
      form0 = "%0,";
      offset += UNITS_PER_WORD;
      direction = VE_DIR_STORE;
      break;
    case VE_DIR_PFCH:
      form0 = "";
      break;
    default:
      form0 = "%0,";
      break;
    }
  switch (direction)
    {
    case VE_DIR_ATOMIC01: case VE_DIR_ATOMIC20:
      if (base1 == NULL_RTX) 
        {
          base1 = base2; 
          base2 = NULL_RTX;
        }
      if (base1 != NULL_RTX && base2 != NULL_RTX) 
        {
          xop[0] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
          xop[1] = base1;
          xop[2] = base2;
          output_asm_insn("addu.l\t%0,%1,%2",xop);
          base1 = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM); 
          base2 = NULL_RTX;
        }
      if(base1 == NULL_RTX)
	form31 = "";
      else
	form31 = "(%1)";
      form4 = ",%5";
      break;

    default:
      if (base2 == NULL_RTX) 
        {
          if(base1 == NULL_RTX)
            form31 = "";
          else
            form31 = "(,%1)";
        }
      else
        {
          if(base1 == NULL_RTX)
            form31 = "(,%2)";
          else
            form31 = "(%2,%1)";
        }
      form4 = "";
    }
  
  if (!ve_const_si_p(offset))
    {
      /* over 2GB offset */
      gcc_assert(sym == NULL_RTX);
      xop[0] = operand_reg;
      xop[1] = base1;
      xop[2] = base2;
      xop[3] = GEN_INT(offset);
      xop[4] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
      xop[5] = operand_aid;

      snprintf(buf,VE_BUF_SIZE,"lea.sl\t%%4,%%H3%s",form31);
      output_asm_insn(buf,xop);
    
      snprintf(buf,VE_BUF_SIZE,"%s\t%s%%L3(,%%4)%s",ldst,form0,form4);
      output_asm_insn(buf,xop);
    }
  else
    {
      /* under 2GB offset, SYMBOL_REF or LABEL_REF */
      xop[0] = operand_reg;
      xop[1] = base1;
      xop[2] = base2;
      xop[3] = GEN_INT(offset);
      xop[4] = sym;
      xop[5] = operand_aid;
      if (sym == NULL_RTX) 
        {
          snprintf(buf,VE_BUF_SIZE,"%s\t%s%%3%s%s",ldst,form0,form31,form4);
        }
      else /* sym != NULL_RTX */
        {
          const char *form3 = (offset > 0)? "+%3" 
	    : (offset ==0)? "" 
	    : "%3";
          snprintf(buf,VE_BUF_SIZE,"%s\t%s%%4%s%s%s",
		   ldst,form0,form3,form31,form4);
        }
      
      output_asm_insn(buf,xop);
    }
  return;
}

/* Woker function for TARGET_PROMOTE_FUNCTION_MODE.  */
machine_mode
ve_promote_function_mode (const_tree type,
			  machine_mode mode,
			  int *punsignedp,
			  const_tree fntype,
			  int for_return)
{
  /* for function return value, apply the same rules given 
     by PROMOTE_MODE */
  if (for_return !=0)
    return default_promote_function_mode_always_promote(type,mode,
							punsignedp,fntype,for_return);

  /* Promotion of modes smaller than DI mode to 
     DImode only for argment */ 
   
  if (GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) < 8)
    {
      return DImode;
    }

  return mode;
}


const char *
ve_output_call_instr(rtx *operands)
{
  rtx xop[3];
  xop[0] = operands[0];
  xop[1] = gen_rtx_REG(DImode, VE_SCRATCH_REGNUM);
  xop[2] = gen_rtx_REG(DImode, VE_RETURN_REGNUM);

  if (REGNO(operands[0]) != VE_SCRATCH_REGNUM) {
    output_asm_insn("or\t%1,0,%0",xop);
  }

  output_asm_insn("bsic\t%2,(%1)",xop);

  return "";
}

const char *
ve_output_call_instr_value(rtx *operands)
{
  ve_output_call_instr(&operands[1]);

  return "";
}

rtx
ve_function_value (const_tree valtype, const_tree func ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  mode = TYPE_MODE(valtype);
  if(TREE_CODE (valtype) == VECTOR_TYPE)
    return gen_rtx_REG(mode,0);
  else
    return ve_libcall_value (mode);
}

rtx
ve_libcall_value (machine_mode mode)
{

  if (GET_MODE_CLASS(mode) == MODE_COMPLEX_FLOAT)
    {
      machine_mode inner = GET_MODE_INNER(mode);
      if (inner == TFmode || inner == TImode)
	return gen_rtx_PARALLEL
	  (mode,
	   gen_rtvec (2,
		      gen_rtx_EXPR_LIST (VOIDmode,
					 gen_rtx_REG(inner,0),
					 GEN_INT(0)),
		      gen_rtx_EXPR_LIST (VOIDmode,
					 gen_rtx_REG(inner,2),
					 GEN_INT(GET_MODE_SIZE(inner)))
		      ));
      else
	return gen_rtx_PARALLEL
	  (mode,
	   gen_rtvec (2,
		      gen_rtx_EXPR_LIST (VOIDmode,
					 gen_rtx_REG (inner,0),
					 GEN_INT(0)),
		      gen_rtx_EXPR_LIST (VOIDmode,
					 gen_rtx_REG (inner,1),
					 GEN_INT(GET_MODE_SIZE(inner)))));
    }
  if (GET_MODE_CLASS(mode) == MODE_COMPLEX_INT)
    {
      machine_mode inner = GET_MODE_INNER(mode);
      return gen_rtx_PARALLEL
	(mode,
	 gen_rtvec (2,
		    gen_rtx_EXPR_LIST (VOIDmode,
				       gen_rtx_REG(inner,0),
				       GEN_INT(0)),
		    gen_rtx_EXPR_LIST (VOIDmode,
				       gen_rtx_REG(inner,1),
				       GEN_INT(GET_MODE_SIZE(inner)))));
    }
  if (mode == TFmode) 
    return gen_rtx_REG(mode,0);
  if (mode == DFmode || mode == SFmode)
    return gen_rtx_REG(mode,0) ;
  
  if (mode == TImode) 
    return gen_rtx_REG(TImode,0);
  if (mode == DImode)
    return gen_rtx_REG(DImode,0);

   /* mode == QImode || mode == HImode || mode == SImode */
  return gen_rtx_REG(DImode,0);
}

static void
ve_elf_asm_constructor (rtx symbol, int priority)
{
  default_elf_init_array_asm_out_constructor (symbol,priority);
}

static void
ve_elf_asm_destructor (rtx symbol, int priority)
{
  default_elf_fini_array_asm_out_destructor (symbol,priority);
}

/* Worker function for TARGET_STRUCT_VALUE_RTX.  */

static rtx
ve_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
                     int incoming ATTRIBUTE_UNUSED)
{
/* pass the address of the struct that is the function value as if it
   were an extra parameter */
  return NULL_RTX;
}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */
static bool
ve_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  return (TYPE_MODE (type) == BLKmode);
}

void 
ve_asm_output_function_prefix(FILE *file ATTRIBUTE_UNUSED,
                    const char *fnname ATTRIBUTE_UNUSED) 
{ 
}

void
ve_asm_output_ident (const char *ident_str)
{
  const char *ident_asm_op = "\t.ident\t";

  /* If we are still in the front end, do not write out the string
     to asm_out_file.  Instead, add a fake top-level asm statement.
     This allows the front ends to use this hook without actually
     writing to asm_out_file, to handle #ident or Pragma Ident.  */
  if (symtab->state == PARSING)
    {
      char *buf = ACONCAT ((ident_asm_op, "\"", ident_str, "\"\n", NULL));
      symtab->finalize_toplevel_asm (build_string (strlen (buf), buf));
    }
  else
    fprintf (asm_out_file, "%s\"%s\"\n", ident_asm_op, ident_str);
}

void
ve_asm_output_function_label (FILE *file, const char *fnname,
                                tree decl ATTRIBUTE_UNUSED)
{
/* making of linkage section */
  fputs("\t.type\t",file);
  assemble_name(file,fnname);
  fputs(", @function\n",file);
  if (ve_use_linkage) 
    { 
      fputs("\t.using\t",file);
      assemble_name(file,fnname);
      fprintf(file,",%s\n",reg_names[VE_LINKAGE_REGNUM]);
    }
  ASM_OUTPUT_LABEL (file, fnname);
}

static void
ve_output_function_prologue (FILE *file ATTRIBUTE_UNUSED,
                                 HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
}

void
ve_expand_prologue (void)
{
  HOST_WIDE_INT size = get_frame_size ();
  unsigned long long local_space;
  unsigned long long total_space;
  int i;
  int n_regs_to_save;
  long long stack_extension_size;
  const char *fnname ;
  rtx insn;

  /* Get the function name the same way that toplev.c does before calling
     assemble_start_function.  This is needed so that the name used here
     exactly matches the name used in ASM_DECLARE_FUNCTION_NAME.  */
  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);

  n_regs_to_save = 0 ;

  for(i = FIRST_S_REG ; i <= LAST_S_REG ; i++)
    {
      if(REG_NEEDS_SAVE(i)) 
	{
	  n_regs_to_save++;
	}
    }
  if (n_regs_to_save > VE_MAX_SAVE_REGS) 
    fatal_error(input_location,
		"internal error:saving registers exceeds limit");
  fputs("\t# Function '",asm_out_file);
  assemble_name(asm_out_file,fnname);
  fprintf(asm_out_file,"' local:"
          HOST_WIDE_INT_PRINT_DEC
          " bytes, param:%d bytes, save regs:%d.\n",
          size,crtl->outgoing_args_size,n_regs_to_save+2);
                     /* +2 = frame pointer and return address */

  /* add max function paramlist size*/
  local_space = size + crtl->outgoing_args_size;
  total_space = local_space + VE_RSA_SIZE; 

  /* align to 16 bytes*/
  total_space = (total_space + 15)&(~15LL);
  local_space = total_space - VE_RSA_SIZE;

  /* set stack usage info */
  if (flag_stack_usage_info)
    current_function_static_stack_size = total_space;


  /* save frame pointer */
  insn = emit_move_insn (
			 gen_rtx_MEM(Pmode,
				     plus_constant(Pmode,stack_pointer_rtx, 0)),
			 frame_pointer_rtx);
  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_CFA_DEF_CFA, 
		plus_constant(Pmode, stack_pointer_rtx,0));
  add_reg_note (insn, REG_CFA_OFFSET, 
		gen_rtx_SET(
			    gen_rtx_MEM(Pmode,
					plus_constant(Pmode,stack_pointer_rtx, 0)),
			    frame_pointer_rtx));

  /* save return address registerr */
  emit_move_insn (
		  gen_rtx_MEM(Pmode,
			      plus_constant(Pmode,stack_pointer_rtx,
					    UNITS_PER_WORD)),
		  gen_rtx_REG(Pmode,VE_RETURN_REGNUM));

  /* save registers (1) special registers */
  for(i = FIRST_S_REG ; i < VE_SMALLEST_CALLEE_SAVED_REGNUM ; i++)
    {
      if(REG_NEEDS_SAVE(i)) 
	{
	  emit_move_insn (
			  gen_rtx_MEM(Pmode,
				      plus_constant(Pmode,stack_pointer_rtx,
						    (i-14)*8+16)),
			  gen_rtx_REG(Pmode,i));
	}
    }

  /* set Global Offset Table register */
  if (flag_pic)
    {
      if (df_regs_ever_live_p(VE_GOT_REGNUM))
	{
	  rtx p,pltreg,gotreg,x;
	  x = ve_got_sym();
	  pltreg = gen_rtx_REG(Pmode,VE_PLT_REGNUM);
	  gotreg = gen_rtx_REG(Pmode,VE_GOT_REGNUM);
          emit_insn (gen_ve_move_pic_pc2(gotreg,x,pltreg));
	}
    }

  /* set frame pointer */
  insn = emit_move_insn (
			 frame_pointer_rtx,
			 stack_pointer_rtx);
  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_CFA_DEF_CFA,frame_pointer_rtx);

  /* save registers (2) callee saved registers */
  for(i = VE_SMALLEST_CALLEE_SAVED_REGNUM ; i <= LAST_S_REG ; i++)
    {
      if(REG_NEEDS_SAVE(i)) 
	{
	  emit_move_insn (
			  gen_rtx_MEM(Pmode,
				      plus_constant(Pmode,frame_pointer_rtx,
						    (i-14)*8+16)),
			  gen_rtx_REG(Pmode,i));
	}
    }


  /* set text section base register */
  if (ve_use_linkage) 
    emit_move_insn (
		    gen_rtx_REG(Pmode,VE_LINKAGE_REGNUM),
		    gen_rtx_REG(Pmode,VE_SCRATCH_REGNUM));

  /* extend stack area */
  /* N.B. VE allows over 2GByte local stack size. Is there any limit ? */
  stack_extension_size = (HOST_WIDE_INT)(-total_space);

  emit_insn (gen_rtx_SET (
			  stack_pointer_rtx,
			  gen_rtx_UNSPEC_VOLATILE(Pmode,
						  gen_rtvec(3,
							    GEN_INT(stack_extension_size),
							    gen_rtx_REG(Pmode,VE_BEGIN_TEMP_REGNUM),
							    gen_rtx_REG(Pmode,VE_BEGIN_TEMP_REGNUM+1)),
						  UNSPEC_STACK_PROLOGUE)
			  ));

  if (MAIN_NAME_P (DECL_NAME (current_function_decl)))
    {
      emit_insn(gen_init_program_mode());
    }

  emit_insn (gen_prologue_end ());
  emit_insn (gen_blockage ());
}

static bool
ve_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == STACK_POINTER_REGNUM ? ! frame_pointer_needed : true);
}

/* Initial offset from frame_pointer to stack pointer after prologue */
HOST_WIDE_INT
ve_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT offset;
  gcc_assert(from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM);
  offset = get_frame_size() + crtl->outgoing_args_size + VE_RSA_SIZE;
  offset = (offset + 15) & (~15LL);
  return offset;
}


/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
   This is null for libcalls where that information may
   not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
   the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
   (otherwise it is an extra parameter matching an ellipsis).  */

/* On the VE first VE_MAX_ARGS_IN_REGS are passed in register
   any more args are passed in stack.  */

static rtx
ve_function_arg_1 (cumulative_args_t cum_v, machine_mode mode,
		   const_tree type, bool named ATTRIBUTE_UNUSED, bool incoming_p)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int arg_number = cum->arg_words/UNITS_PER_WORD;  
  int arg_number_align;
  int size, numargs;
  rtx loc[VE_MAX_ARGS_IN_REGS+1]; 
  int i;

  /* Last-argument marker.  */
  if (mode == VOIDmode) 
    return NULL_RTX;

  size = (mode != BLKmode)? GET_MODE_SIZE(mode)
    : int_size_in_bytes(type);

  arg_number_align = arg_number; 
  if (mode == TFmode || mode == TCmode || mode == TImode || mode == CTImode) 
    arg_number_align = arg_number_align + (arg_number_align & 1);

  numargs = (size+UNITS_PER_WORD-1)/UNITS_PER_WORD;
  if (arg_number_align + numargs > VE_MAX_ARGS_IN_REGS)  
    return NULL_RTX;
  
  if (mode == TFmode || mode == TCmode || mode == TImode || mode == CTImode) 
    if (arg_number_align > arg_number) 
      cum->arg_words  += UNITS_PER_WORD;

  if (incoming_p 
    || ve_argmem == ARGMEM_OPT
      || ((ve_argmem == ARGMEM_SAFE 
	   && cum->prototype) && !cum->stdarg))
    {
      if (cum->stdarg) 
	{
	  return NULL_RTX;
	} 
      else
	{
	  return gen_rtx_REG (mode,VE_FIRST_ARG_REGNUM + arg_number_align);
	}
    }
   
  /* otherwise set arg to both memory and register 
    if (ve_argmem == ARGMEM_FORCE 
     || ve_argmem == ARGMEM_SAFE 
         && (!cum->prototype || cum->indirect || cum->stdarg))
  */
  switch (mode)
    {
    case BLKmode:
      /* set 0 on the first vector means set args also on stack */
      loc[0] = gen_rtx_EXPR_LIST(VOIDmode, NULL_RTX, GEN_INT(0));
      for (i=0; i < numargs; i++) 
	{
	  loc[i+1] = gen_rtx_EXPR_LIST(VOIDmode,
				       gen_rtx_REG(DImode, 
				   	 VE_FIRST_ARG_REGNUM + arg_number_align + i),
				       GEN_INT(i*UNITS_PER_WORD));
	}
      return gen_rtx_PARALLEL(mode, gen_rtvec_v(numargs+1,loc));

    case TFmode: case TCmode: case TImode: case CTImode:
      /* set 0 on the first vector means set args also on stack */
      /* swap odd-even regster since register number is big endian */
      /* 0 1 2 3 -> 1 0 3 2 */
      loc[0] = gen_rtx_EXPR_LIST(VOIDmode, NULL_RTX, GEN_INT(0));
      for (i=numargs-1; i>=0; i--)
	{
	  loc[i+1] = gen_rtx_EXPR_LIST(VOIDmode,
				       gen_rtx_REG(DImode, 
     					  VE_FIRST_ARG_REGNUM + arg_number_align + i),
				       GEN_INT((i^1)*UNITS_PER_WORD));
	}
      return gen_rtx_PARALLEL(mode, gen_rtvec_v(numargs+1,loc));
      
    /* mode != BLKmode, TFmode, TCmode */
    /* set 0 on the first vector means set args also on stack */
    /* See ** below. This is the mode before promotion.  */
    /* If we use "mode" instead of TYPE_MODE(type), wrong conversion */
    /* ZERO_EXTEND will be generated if the argument is promoted */
    default:
      return gen_rtx_PARALLEL(mode, gen_rtvec(2,
			       gen_rtx_EXPR_LIST(VOIDmode,
						 NULL_RTX,
						 const0_rtx),
			       gen_rtx_EXPR_LIST(VOIDmode,
			       gen_rtx_REG (type?TYPE_MODE(type):mode,//<-- ** risky 
					    VE_FIRST_ARG_REGNUM + arg_number_align),
						 const0_rtx)
					      ));
    }
}

/* Handle the TARGET_FUNCTION_ARG target hook.  */

static rtx
ve_function_arg (cumulative_args_t cum, machine_mode mode,
                    const_tree type, bool named)
{
  return ve_function_arg_1 (cum, mode, type, named, false);
}
  
/* Handle the TARGET_FUNCTION_INCOMING_ARG target hook.  */
  
static rtx
ve_function_incoming_arg (cumulative_args_t cum, machine_mode mode,
                             const_tree type, bool named)
{
  return ve_function_arg_1 (cum, mode, type, named, true);
}

static unsigned int
ve_function_arg_boundary (machine_mode mode, const_tree type)
{
  unsigned int alignment;

  alignment = type ? TYPE_ALIGN (type) : GET_MODE_ALIGNMENT (mode);
  if (alignment < PARM_BOUNDARY)
    alignment = PARM_BOUNDARY;

  return alignment;
}

/* Handle the TARGET_PASS_BY_REFERENCE target hook.
   Specify whether to pass the argument by reference.  */

static bool
ve_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
		      machine_mode mode ATTRIBUTE_UNUSED,
		      const_tree type,
		      bool named ATTRIBUTE_UNUSED)
{
/* On the VE, structures and unions are passed by registers
   or paramlist smaller up to 16 byte.
   There are passed by reference if the */ 
  return (type && AGGREGATE_TYPE_P (type) 
	  && (HOST_WIDE_INT) int_size_in_bytes (type) 
	  > ve_struct_by_value);
}


/* Update the data in CUM to advance over an argument of mode MODE and
   data type TYPE.  (TYPE is null for libcalls where that information
   may not be available.)  */

void
ve_init_cumulative_args(CUMULATIVE_ARGS *cum, tree fntype,
			rtx libname ATTRIBUTE_UNUSED,
			tree fndecl)
{
  cum->arg_words = 0;
  cum->indirect = fntype && !fndecl;
  cum->stdarg = stdarg_p(fntype);
  cum->prototype = fntype && prototype_p (fntype);
}

static void
ve_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			 const_tree type,
			 bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int size;

  size = (mode != BLKmode)? GET_MODE_SIZE(mode)
    : int_size_in_bytes(type);
  cum->arg_words += UNITS_PER_WORD*((size+UNITS_PER_WORD-1)/UNITS_PER_WORD);  
}

/* Implement TARGET_SHIFT_TRUNCATION_MASK */

static unsigned HOST_WIDE_INT
ve_shift_truncation_mask (machine_mode mode)
{
  switch (mode) 
    {
    case  SImode: 
      return 31;
    case DImode:
      return 63;
    default: 
      return 0;
    }
}

/* Implement TARGET_MODE_REP_EXTENDED.  */
static int
ve_mode_rep_extended (machine_mode mode, machine_mode mode_rep)
{
  /* SImode register values are sign-extended to DImode.  */
  if (mode == SImode && mode_rep == DImode)
    return SIGN_EXTEND;
  if (mode == HImode && mode_rep == SImode)
    return SIGN_EXTEND;
  if (mode == QImode && mode_rep == HImode)
    return SIGN_EXTEND;

  return UNKNOWN;
}

void
ve_profile_hook (int labelno ATTRIBUTE_UNUSED)
{
  const char *fnname;
  rtx mcount_ref, fnname_ref;

  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
  while (*fnname == '*') fnname++;
  fnname_ref = gen_rtx_SYMBOL_REF (Pmode, fnname);
  mcount_ref = gen_rtx_SYMBOL_REF (Pmode, "mcount");

  emit_library_call(mcount_ref, LCT_NORMAL, 
		    VOIDmode, 2,
		    fnname_ref, Pmode,
		    gen_rtx_MEM(Pmode, 
				plus_constant(Pmode, frame_pointer_rtx,8)),
		    Pmode);
}


bool
ve_epilogue_uses (int regno)
{
  machine_mode mode;
  if (regno == STACK_LIMIT_REGNUM  || regno == FRAME_POINTER_REGNUM
  || regno == VE_RETURN_REGNUM || regno == STACK_POINTER_REGNUM
  || regno == VE_GOT_REGNUM || regno == VE_PLT_REGNUM )
    return true;
  if (REG_NEEDS_SAVE(regno)) 
    return true;

  mode = DECL_MODE(DECL_RESULT(current_function_decl));
  if (regno == 0)
    if (mode == DImode || mode == SImode || mode == HImode || mode == QImode 
	|| mode == CDImode || mode == CSImode || mode == CHImode || mode == CQImode
	|| mode == TImode || mode == CTImode)
      return true;
  if (regno == 0)
    if (mode == DFmode || mode == SFmode || mode == DCmode || mode == SCmode
	|| mode == TFmode || mode ==TCmode) 
      return true;
  if (regno == 1)
    if (mode == DCmode || mode == CDImode || mode == TFmode || mode ==TCmode
	|| mode == TImode || mode == CTImode)
      return true;
  if (regno == 2)
    if (mode ==TCmode || mode == CTImode)
      return true;
  if (regno == 3)
    if (mode ==TCmode || mode == CTImode)
      return true;

  return false;
}

void
ve_expand_epilogue(void)
{
  int i;
  rtx insn;

  emit_insn (gen_blockage ());
  emit_insn (gen_epilogue_start ());

/* recover saved registers (2) callee saved registers */
  for(i = VE_SMALLEST_CALLEE_SAVED_REGNUM ; i <= LAST_S_REG ; i++) {
    if(REG_NEEDS_SAVE(i)) 
      {
	insn = emit_move_insn (
			       gen_rtx_REG(Pmode,i),
			       gen_rtx_MEM(Pmode,
					   plus_constant(Pmode,frame_pointer_rtx,
							 (i-14)*8+16)));
      }
  }

  /* recover stack pointer */
  insn = emit_move_insn (
			 stack_pointer_rtx,
			 frame_pointer_rtx);
  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_CFA_REGISTER, 
		gen_rtx_SET(stack_pointer_rtx,frame_pointer_rtx));
  add_reg_note (insn, REG_CFA_DEF_CFA, 
		plus_constant(Pmode, stack_pointer_rtx,0));
  
/* recover saved registers (1) */
  for(i = FIRST_S_REG ; i < VE_SMALLEST_CALLEE_SAVED_REGNUM ; i++)
    {
      if(REG_NEEDS_SAVE(i)) 
	{
	  insn = emit_move_insn (
				 gen_rtx_REG(Pmode,i),
				 gen_rtx_MEM(Pmode,
					     plus_constant(Pmode,stack_pointer_rtx,
							   (i-14)*8+16)));
	}
    }

  /* recover return address register */
  insn = emit_move_insn (
			 gen_rtx_REG(Pmode,VE_RETURN_REGNUM),
			 gen_rtx_MEM(Pmode,
				     plus_constant(Pmode,stack_pointer_rtx,
						   UNITS_PER_WORD)));

  /* recover frame pointer */
  emit_move_insn (
		  frame_pointer_rtx,
		  gen_rtx_MEM(Pmode,
			      plus_constant(Pmode,stack_pointer_rtx, 0)));

  emit_jump_insn (ret_rtx);
} 

static void
ve_output_function_epilogue (FILE *file,
                              HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  fputs("\t# End of function\n", file);
}

/* Output the rest of the textual info surrounding the epilogue.  */
void
ve_end_function (FILE *file, const char *fnname, tree decl ATTRIBUTE_UNUSED)
{
  if (!flag_inhibit_size_directive)
   {
     fputs(SIZE_ASM_OP,file);
     assemble_name(file,fnname);
     fputs(",.-", file);
     assemble_name(file,fnname);
     fputs("\n", file);
   }
}

/* Return an RTX indicating where the return address to the
   calling function can be found.  */
rtx
ve_return_addr_rtx (int count ATTRIBUTE_UNUSED, rtx frame)
{
  return gen_rtx_MEM(Pmode, plus_constant(Pmode, frame, UNITS_PER_WORD));
}

static section *
ve_function_section (tree decl, enum node_frequency freq,
                     bool startup, bool exit)
{
  /* Put functions in text section if target doesn't have named sections.  */
  if (!targetm_common.have_named_sections)
    return text_section;

  /* Force nested functions into the same section as the containing
     function.  */
  if (decl
      && DECL_SECTION_NAME (decl) == NULL
      && DECL_CONTEXT (decl) != NULL_TREE
      && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL
      && DECL_SECTION_NAME (DECL_CONTEXT (decl)) == NULL)
    return function_section (DECL_CONTEXT (decl));

  /* Otherwise, use the default function section.  */
  return default_function_section (decl, freq, startup, exit);
}

/* Put constant pool in text section */
static section *
ve_select_rtx_section (machine_mode mode, rtx x,
		       unsigned HOST_WIDE_INT align)
{
  return default_elf_select_rtx_section (mode, x, align);
}

static section*
ve_select_section (tree decl, int reloc, 
		   unsigned HOST_WIDE_INT align)
{
  return default_elf_select_section (decl, reloc, align);
}

/* This is called from dwarf2out.c via TARGET_ASM_OUTPUT_DWARF_DTPREL.
   We need to emit DTP-relative relocations.  */

static void
ve_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  switch (size)
    {
    case 4:
      fputs ("\t.4byte\t", file);
      break;
    case 8:
      fputs ("\t.8byte\t", file);
      break;
    default:
      gcc_unreachable ();
    }
  output_addr_const (file, x);
  fputs ("@dtpoff", file);
}

static void
ve_file_start(void)
{
  default_file_start();
}

/* constant_alignment, almost the same as i386 */
int
ve_constant_alignment (tree exp, int align)
{
  if (TREE_CODE (exp) == REAL_CST || TREE_CODE (exp) == VECTOR_CST
      || TREE_CODE (exp) == INTEGER_CST)
    {
      if (TYPE_MODE (TREE_TYPE (exp)) == DFmode && align < 64)
        return 64;
      else if (TYPE_MODE (TREE_TYPE (exp)) == TFmode  && align < 128)
        return 128;
      else if (TYPE_MODE (TREE_TYPE (exp)) == TImode  && align < 128)
        return 128;
    }
  else if (!optimize_size && TREE_CODE (exp) == STRING_CST
           && TREE_STRING_LENGTH (exp) >= 31 && align < BITS_PER_WORD)
    return BITS_PER_WORD;
  
  return align;
}

int
ve_check_symbol(rtx x, machine_mode mode)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
      return false;
      
    case LABEL_REF:
      return true;
      
    case SYMBOL_REF:
      return true;
      
    case CONST:
      return ve_check_symbol(XEXP(x,0), mode);
      
    case PLUS:
    case MINUS:
      return ve_check_symbol(XEXP(x,0), mode)
	|| ve_check_symbol(XEXP(x,1), mode);
      
    default:
      return false;
    }
}

rtx
ve_force_const_mem (rtx x, bool direct_call, bool ins_call)
{
  rtx p,ret;

  if (ve_tls_symbol_p(x)) 
    {
      rtx tmp,tp;
      
      if (!TARGET_TLS_ASIS)
	{
   /* change all tls model to be global-dynamic anyway */
	  ret = gen_rtx_REG(Pmode,0);
	  emit_insn(gen_tgd_load_call(ret,x,gen_tls_get_addr()));
	  return ret;
	}


      switch (SYMBOL_REF_TLS_MODEL (x))
	{
	case TLS_MODEL_GLOBAL_DYNAMIC:
	  ret = gen_rtx_REG(Pmode,0);
	  emit_insn(gen_tgd_load_call(ret,x,gen_tls_get_addr()));
	  break;
	  
	case TLS_MODEL_LOCAL_DYNAMIC:
	  ret = gen_reg_rtx (Pmode);
	  tmp = gen_rtx_REG(Pmode,0);
	  emit_insn(gen_tld_load_call(tmp,x,gen_tls_get_addr()));
	  emit_insn(gen_tld_offset_load (ret, x, tmp));
	  break;
	  
	case TLS_MODEL_INITIAL_EXEC:
	  tp = gen_rtx_REG(Pmode, VE_THREAD_POINTER_REGNUM);
	  tmp = gen_reg_rtx (Pmode);
	  ret = gen_reg_rtx (Pmode);
          emit_insn (gen_tie_load (tmp, x));
	  emit_move_insn (ret, gen_rtx_PLUS (Pmode, tp, tmp));
	  break;
	  
	case TLS_MODEL_LOCAL_EXEC:
	  tp = gen_rtx_REG(Pmode, VE_THREAD_POINTER_REGNUM);
	  ret = gen_reg_rtx (Pmode);
	  emit_insn (gen_tle_load (ret, x, tp));
	  break;
	  
	default:
	  gcc_unreachable ();
	}
      return ret;
      
    }
  else if (flag_pic)
    {
      if (SYMBOL_REF_FUNCTION_P(x))
	{
	  
	  if (SYMBOL_REF_LOCAL_P(x))
	    {
	      /* pic static function */
              ret = gen_reg_rtx (Pmode);
              emit_insn (gen_ve_move_pic_pc(ret,x));
              return ret;
	    }
	  else if (direct_call)
	    {
	      /* pic direct external function */
              ret = gen_reg_rtx (Pmode);
              emit_insn (gen_ve_move_pic_plt(ret,x));

	      crtl->uses_pic_offset_table = 1;
	      emit_use (gen_rtx_REG(Pmode,VE_GOT_REGNUM));
	      emit_use (gen_rtx_REG(Pmode,VE_PLT_REGNUM));
              return ret;
	    }
	  else
	    {
	      /* pic indirect external function */
              ret = gen_reg_rtx (Pmode);
              emit_insn (gen_ve_move_pic_got(ret,x));

	      crtl->uses_pic_offset_table = 1;
	      emit_use (gen_rtx_REG(Pmode,VE_GOT_REGNUM));
              return ret;
	    }
	}
      else 
	{
	  if (SYMBOL_REF_LOCAL_P(x))
	    {
	      /* pic static variable */
              ret = gen_reg_rtx (Pmode);
              emit_insn (gen_ve_move_pic_gotoff(ret,x));

	      crtl->uses_pic_offset_table = 1;
	      emit_use (gen_rtx_REG(Pmode,VE_GOT_REGNUM));
              return ret;
	    }
	  else
	    {
	      /* pic external variable */
              ret = gen_reg_rtx (Pmode);
              emit_insn (gen_ve_move_pic_got(ret,x));

	      crtl->uses_pic_offset_table = 1;
	      emit_use (gen_rtx_REG(Pmode,VE_GOT_REGNUM));
              return ret;
	    }
	}
    }
  else if (!ve_symbol_indirect)
    {
      /* notls, nopic functions or variables */
      p = x;
      ret = gen_reg_rtx (Pmode);
      if (SYMBOL_REF_FUNCTION_P(x) && ins_call)
        emit_insn (gen_ve_move_call(ret,x));
      else
        emit_insn (gen_ve_move(ret,x));

      if (SYMBOL_REF_FUNCTION_P(x) && !SYMBOL_REF_LOCAL_P(x))
	{
          emit_use (gen_rtx_REG(Pmode,VE_GOT_REGNUM));
          emit_use (gen_rtx_REG(Pmode,VE_PLT_REGNUM));
	}
      return ret;
    }
  else 
    {
      if (SYMBOL_REF_FUNCTION_P(x) && !SYMBOL_REF_LOCAL_P(x))
	{
          emit_use (gen_rtx_REG(Pmode,VE_GOT_REGNUM));
          emit_use (gen_rtx_REG(Pmode,VE_PLT_REGNUM));
	}
      /* nopic force use memory */
      return force_const_mem(Pmode,x);
    }
  
  /*  VE_SCRATCH_REGISTER may be destroyed between here and bsic.
      So, we do not use it 
      if (SYMBOL_REF_FUNCTION_P(x)) 
      tmpreg = gen_rtx_REG(Pmode,VE_SCRATCH_REGNUM);
      else
  */
}
 
rtx
ve_force_label_relative(rtx x)
{
  rtx p,tmpreg;
  if (!ve_symbol_indirect) 
    {
      p = gen_rtx_UNSPEC(Pmode,gen_rtvec(1,x),
			 UNSPEC_MOVE_PIC_LABEL); 
    }
  else
    p = x;
  tmpreg = gen_reg_rtx(Pmode);
  emit_insn (gen_rtx_SET (tmpreg, p));
  return tmpreg;
}

rtx
ve_indirect_addr(rtx x, bool direct_call, bool ins_call)
{
  rtx ret;
  rtx tmpreg;
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
      ret = x;
      break;
      
    case LABEL_REF:
      ret = ve_force_label_relative(x);
      break;
      
    case SYMBOL_REF:
      tmpreg = gen_reg_rtx(GET_MODE(x));
      emit_move_insn(tmpreg,ve_force_const_mem(x,direct_call,ins_call));
      ret = tmpreg;
      break;

    case CONST:
      /* remove CONST */
      ret = ve_indirect_addr(XEXP(x,0),direct_call,ins_call);
      break;
      
    case PLUS:
      ret =  gen_rtx_PLUS(GET_MODE(x),
			  ve_indirect_addr(XEXP(x,0),direct_call,ins_call),
			  ve_indirect_addr(XEXP(x,1),direct_call,ins_call));
      break;
      
    case MINUS:
      ret =  gen_rtx_MINUS(GET_MODE(x),
			   ve_indirect_addr(XEXP(x,0),direct_call,ins_call),
			   ve_indirect_addr(XEXP(x,1),direct_call,ins_call));
      break;
      
    default:
      ret = x;
    }
  return ret;
}

 static rtx
ve_expand_mem_scratch(rtx x, int *changed) 
 {
   rtx tmp,tmpreg;
   *changed = 0;
   
   if (REG_P(x)) 
     return x;
   
   if (GET_CODE(x) == SYMBOL_REF)
     {
       return x;
     }
   
   if (GET_CODE(x) == MEM) 
     {
       int c1;
       tmp = ve_expand_mem_scratch(XEXP(x,0),&c1);
       tmpreg = gen_rtx_REG(GET_MODE(x),VE_SCRATCH_REGNUM);
       emit_move_insn(tmpreg,gen_rtx_MEM(GET_MODE(x),tmp));
       *changed = 1;
       return tmpreg;
     }
   
   if (GET_CODE(x) == PLUS)
     {
       int c1,c2;
       rtx x1 = ve_expand_mem_scratch(XEXP(x,0),&c1);
       rtx x2 = ve_expand_mem_scratch(XEXP(x,1),&c2);
       if (c1 || c2)
         {
	   x = gen_rtx_PLUS(GET_MODE(x),x1,x2);
	   *changed = 1 ;
         }
       return x;
     }
   
   return x;
 }
 
int
ve_expand_move(rtx *operands, machine_mode mode)
{
   int ret;
   rtx lhs,rhs;
   if (lra_in_progress == 0 && reload_in_progress ==0 && reload_completed == 0)
     {
       if (ve_check_symbol(operands[1],mode)) 
	 { 
	   rhs = ve_indirect_addr(operands[1],false,false);       
	   if (ve_check_symbol(operands[0],mode))
	     {
	       lhs = ve_indirect_addr(operands[0],false,false);
	     }
	   else
	     lhs = operands[0];
	   if (GET_CODE(lhs) != REG && GET_CODE(rhs) != REG) 
	     {
	       rtx tmpreg = gen_reg_rtx(GET_MODE(rhs));
	       emit_move_insn(tmpreg,rhs);
	       rhs = tmpreg;
	     }        
	   emit_move_insn(lhs,rhs);
	   ret = 1;
	 }
       
       else if (ve_check_symbol(operands[0],mode)) 
	 {
	   lhs = ve_indirect_addr(operands[0],false,false);
	   rhs = operands[1];
	   if (GET_CODE(lhs) != REG && GET_CODE(rhs) != REG) 
	     {
	       rtx tmpreg = gen_reg_rtx(GET_MODE(rhs));
	       emit_move_insn(tmpreg,rhs);
	       rhs = tmpreg;
	     }        
	   emit_move_insn(lhs,rhs);
	   ret = 1;
	 }
       
       else
	 ret = 0;
     }
   
   else {
     int c1,c2;
     rtx x1,x2;
     
     if (GET_CODE(operands[0]) == MEM)
       {
	 x1 = ve_expand_mem_scratch(XEXP(operands[0],0),&c1);
	 if (GET_CODE(operands[1]) == MEM)
	   {
	     x2 = ve_expand_mem_scratch(XEXP(operands[1],0),&c2);
	     if (c1 || c2)
	       emit_move_insn(gen_rtx_MEM(mode,x1),gen_rtx_MEM(mode,x2));
	   }
	 else
	   {
	     x2 = ve_expand_mem_scratch(operands[1],&c2);
	     if (c1 || c2)
	       emit_move_insn(gen_rtx_MEM(mode,x1),x2);
	   }
       }
     else
       {
	 x1 = ve_expand_mem_scratch(operands[0],&c1);
	 if (GET_CODE(operands[1]) == MEM)
	   {
	     x2 = ve_expand_mem_scratch(XEXP(operands[1],0),&c2);
	     if (c1 || c2)
	       emit_move_insn(x1,gen_rtx_MEM(mode,x2));
	   }
	 else
	   {
	     x2 = ve_expand_mem_scratch(operands[1],&c2);
	     if (c1 || c2)
	       emit_move_insn(x1,x2);
	   }
       }
     ret = c1 || c2;
   }
   
   return ret;
}

bool ve_const_si_p(long long x) {
  return x >= -2147483647LL-1 && x <= 2147483647LL;
}

enum unwind_info_type
ve_debug_unwind_info (void)
{
  return UI_SJLJ;
}

void
ve_asm_output_anchor (rtx symbol)
{
  char buf[VE_BUF_SIZE];
  
  snprintf (buf,VE_BUF_SIZE,"$ + " HOST_WIDE_INT_PRINT_DEC,
	    SYMBOL_REF_BLOCK_OFFSET (symbol));
  ASM_OUTPUT_DEF (asm_out_file, XSTR (symbol, 0), buf);
}

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.short\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.int\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"
#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\t.byte\t"

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS ve_legitimize_address

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P     ve_legitimate_address_p

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE	ve_output_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE	ve_output_function_epilogue

#undef  TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR		ve_elf_asm_constructor

#undef  TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR		ve_elf_asm_destructor

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START	ve_file_start

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX	ve_struct_value_rtx

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY ve_return_in_memory

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM ve_cannot_force_const_mem

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE ve_promote_function_mode

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND ve_print_operand

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS ve_print_operand_address

#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P ve_print_operand_punct_valid_p

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST ve_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST ve_memory_move_cost
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_mode_as_bool_0
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS ve_rtx_costs

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P ve_legitimate_constant_p

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE ve_option_override

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE ve_can_eliminate

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE ve_function_arg_advance
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG ve_function_arg

/* VE specific */
#undef TARGET_FUNCTION_INCOMING_ARG
#define TARGET_FUNCTION_INCOMING_ARG ve_function_incoming_arg
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY ve_function_arg_boundary

/* Complex arguments to be split and treated as their individual components */
#undef TARGET_SPLIT_COMPLEX_ARG
#define TARGET_SPLIT_COMPLEX_ARG  hook_bool_const_tree_true

#undef TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true

#undef  TARGET_MODE_REP_EXTENDED
#define TARGET_MODE_REP_EXTENDED ve_mode_rep_extended

#undef TARGET_ASM_FUNCTION_SECTION
#define TARGET_ASM_FUNCTION_SECTION ve_function_section

#undef	TARGET_ASM_SELECT_RTX_SECTION
#define	TARGET_ASM_SELECT_RTX_SECTION	ve_select_rtx_section
#undef	TARGET_ASM_SELECT_SECTION
#define	TARGET_ASM_SELECT_SECTION	ve_select_section

#undef TARGET_ASM_OUTPUT_IDENT
#define TARGET_ASM_OUTPUT_IDENT ve_asm_output_ident

#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL ve_output_dwarf_dtprel

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P ve_scalar_mode_supported_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P ve_vector_mode_supported_p

#undef  TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE ve_trampoline_template

#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT         ve_trampoline_init

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE ve_pass_by_reference

#undef TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK ve_shift_truncation_mask

/* disable new local register allocator */
#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-ve.h"
