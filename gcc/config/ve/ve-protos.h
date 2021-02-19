/* Function prototype definitions GNU compiler.  VE version.
   Copyright (C) 2007-2016 Free Software Foundation, Inc.

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

/* gen call instruction */
extern const char *ve_output_call_instr(rtx *);
extern const char *ve_output_call_instr_value(rtx *);

/* gen move sf/df/tf immediate */
extern const char *ve_movsf_reg_immediate(rtx *);
extern const char *ve_movdf_reg_immediate(rtx *);
extern const char *ve_movtf_reg_immediate(rtx *);

/* gen load/store instrunction */
extern const char *ve_asm_output_ldst_unified(const char *, 
                   int , rtx *, rtx);

extern void ve_asm_output_function_label (FILE *file, const char *fnname,
                                tree decl ATTRIBUTE_UNUSED);

extern void ve_asm_output_function_prefix(FILE *file, const char *fnname);
extern void ve_asm_output_pool_prologue(FILE *file, const char *fnname,
                          tree fundecl ATTRIBUTE_UNUSED,
                          int size ATTRIBUTE_UNUSED);

extern bool ve_legitimate_address_p (machine_mode mode, rtx x, bool strict);
extern rtx ve_legitimize_address (rtx, rtx, machine_mode);
extern int ve_regno_mode_ok_for_base_p (int regno, machine_mode mode, bool strict_p);

extern int ve_check_symbol(rtx x, machine_mode mode);

extern rtx ve_force_const_mem (rtx x, bool direct_call, bool ins_call);
extern rtx ve_force_label_relative (rtx x);
extern rtx ve_indirect_addr(rtx x, bool direct_call, bool ins_call);
extern int ve_expand_move(rtx *operands, machine_mode mode);


/* Allocate registers appropriate to data types. doubles 
   require even/odd pairs of long double registers.  */

extern int ve_hard_regno_mode_ok(unsigned int regno, machine_mode mode);
/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.*/
extern unsigned int ve_dbx_register_number (unsigned int regno);


extern bool ve_cannot_change_mode_class (machine_mode from, machine_mode to,
                             enum reg_class rclass);

extern bool ve_modes_tieable_p (machine_mode mode1, machine_mode mode2);

extern int ve_constant_alignment (tree, int);

extern void ve_asm_output_anchor (rtx symbol);

extern rtx ve_function_value (const_tree valtype, const_tree func);
extern rtx ve_libcall_value (machine_mode mode);

extern rtx ve_addr_cut_mem(rtx operand, int n);
extern bool ve_epilogue_uses (int regno);

extern void ve_expand_prologue(void);
extern void ve_expand_epilogue(void);

extern void ve_asm_output_ident (const char *ident_str);
extern rtx ve_return_addr_rtx (int count, rtx frame);

extern void ve_init_cumulative_args(CUMULATIVE_ARGS *, tree, rtx, tree);
extern bool ve_const_si_p (long long x);

extern void ve_init_expanders (void);
extern HOST_WIDE_INT ve_initial_elimination_offset (int from, int to);
extern void ve_profile_hook (int);
extern void ve_end_function(FILE *, const char *, tree);
