;; Machine description for VE architecture for GCC compiler
;;   Copyright (C) 2007-2017 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.   not see
;; <http://www.gnu.org/licenses/>.
;; Changes by NEC Corporation for the VE port, 2017-2021

(define_c_enum "unspec" [
  UNSPEC_COPYSIGN
  UNSPEC_COMPARE
  UNSPEC_MOVE
  UNSPEC_MOVE_CALL
  UNSPEC_MOVE_PIC_PLT
  UNSPEC_MOVE_PIC_PC
  UNSPEC_MOVE_PIC_PC2
  UNSPEC_MOVE_PIC_GOT
  UNSPEC_MOVE_PIC_GOT32
  UNSPEC_MOVE_PIC_GOTOFF
  UNSPEC_MOVE_PIC_LABEL
  UNSPEC_TLSGD
  UNSPEC_TLSLD
  UNSPEC_TLSLDO
  UNSPEC_TLSIE
  UNSPEC_TLSLE
  UNSPEC_MEMORY_BARRIER
  UNSPEC_SP_SET
  UNSPEC_SP_TEST
])

(define_c_enum "unspecv" [
  UNSPEC_BLOCKAGE     ; blockage insn to prevent scheduling across an insn 
  UNSPEC_STACK_PROBE
  UNSPEC_STACK_PROLOGUE
  UNSPEC_FLUSH_ICACHE ; blockage insn for i cache flush
  UNSPEC_INIT_PROGRAM_MODE ; initiallize fp mode and exception flag
  UNSPEC_PROLOGUE_END
  UNSPEC_EPILOGUE_START
  UNSPEC_COMPARE_AND_SWAP
  UNSPEC_SYNC_LOCK_TEST_AND_SET
  UNSPEC_SYNC_LOCK_RELEASE
  UNSPEC_SYNC_OP
  UNSPEC_SYNC_NEW_OP
])


;; Include constrains
 (include "constraints.md")

;; Include predicate definitions
 (include "predicates.md")

(define_attr "type" 
  "unknown,load,store,move,alu,branch,jump,fp,multi,ghost,misc" 
  (const_string "unknown"))

(define_attr "mode" "unknown,none,QI,HI,SI,DI,TI,SF,DF,TF"
             (const_string "unknown"))

;;

(define_mode_iterator MODEANYF [SF DF TF])
(define_mode_iterator MODEF [SF DF])
(define_mode_iterator MODEI [SI DI])
(define_mode_iterator MODEIF [SI DI SF DF])
(define_mode_iterator MODEANYI [QI HI SI DI TI])

(define_mode_attr suffix [(SI "w") (DI "l") 
                              (SF "s") (DF "d") 
                              (TF "q")])
(define_mode_attr reg_or_o_operand [(SI "reg_or_o_operand")
                                  (DI "reg_or_m_operand")])
(define_mode_attr reg_or_io_operand [(SI "reg_or_io_operand")
                                  (DI "reg_or_im_operand")])
(define_mode_attr reg_or_ip_operand [(SI "reg_or_ip_operand")
                                  (DI "reg_or_im_operand")])
(define_mode_attr o_const [(SI "O")
                           (DI "M")])
(define_mode_attr p_const [(SI "P")
                           (DI "M")])
(define_mode_attr ext [(SI ".sx")
                       (DI "")])

;; Used for signed and unsigned widening extensions.
(define_code_iterator any_extend [sign_extend zero_extend])
(define_code_attr ex [(sign_extend "sx") (zero_extend "zx")])

;; Used for logical operations
(define_code_iterator any_logic [and ior xor])
(define_code_attr logic_insn [(and "and") (ior "ior") (xor "xor")])
(define_code_attr logic_code [(and "and") (ior "or") (xor "xor")])

;; Used for shiftl operations
(define_code_iterator any_shift [ashift ashiftrt lshiftrt])
(define_code_iterator any_ashift [ashift ashiftrt])
(define_code_attr shift_insn_w
    [(ashift "ashl") (ashiftrt "ashr")])
(define_code_attr shift_insn_l
    [(ashift "ashl") (ashiftrt "ashr") (lshiftrt "lshr")])
(define_code_attr shift_code_w
    [(ashift "sla.w") (ashiftrt "sra.w")])
(define_code_attr shift_code_l
    [(ashift "sla.l") (ashiftrt "sra.l") (lshiftrt "srl")])

;; Used for max/min operations
(define_code_iterator maxmin [smax smin])
(define_code_attr maxmin_insn
    [(smax "max") (smin "min")])
(define_code_attr maxmin_code
    [(smax "max") (smin "min")])

;; Used for atomic operations
(define_code_iterator atomic [and ior plus minus])
(define_code_attr atomic_insn
    [(and "and") (ior "ior") (plus "add") (minus "sub")])
(define_code_attr atomic_set
    [(and "or") (ior "or") (plus "or") (minus "subs.l")])
(define_code_attr atomic_op
    [(and "0") (ior "1") (plus "2") (minus "2")])


;; Used for sign/zero extend
(define_mode_iterator MODEOVQI [HI SI DI])
(define_mode_iterator MODEOVHI [SI DI])

;; attributes 
(define_attr "length" "" (const_int 1))

;;
;; instructions 
;;
(define_insn "add<mode>3"
  [(set (match_operand:MODEANYF 0 "register_operand" "=r")
        (plus:MODEANYF (match_operand:MODEANYF 1 "register_operand" "%r")
                       (match_operand:MODEANYF 2 "register_operand" "r")))]
  ""
  "fadd.<suffix>\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "<MODE>")])

(define_insn "sub<mode>3"
  [(set (match_operand:MODEANYF 0 "register_operand" "=r")
        (minus:MODEANYF (match_operand:MODEANYF 1 "register_operand" "r")
                        (match_operand:MODEANYF 2 "register_operand" "r")))]
  ""
 "fsub.<suffix>\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "<MODE>")])

(define_insn "mul<mode>3"
  [(set (match_operand:MODEANYF 0 "register_operand" "=r")
        (mult:MODEANYF (match_operand:MODEANYF 1 "register_operand" "%r")
                       (match_operand:MODEANYF 2 "register_operand" "r")))]
  ""
  "fmul.<suffix>\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "<MODE>")])

(define_insn "div<mode>3"
  [(set (match_operand:MODEF 0 "register_operand" "=r")
        (div:MODEF (match_operand:MODEF 1 "register_operand" "r")
                   (match_operand:MODEF 2 "register_operand" "r")))]
  ""
  "fdiv.<suffix>\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "<MODE>")])

;; c = a / b 
;; (1) normalize a and b; (sign=0; exp=0x3ff )
;; (2) first approx. uses double division result
;;      bh(Df) = b
;;      rh = 1.0d0 / bh
;;      r = rh
;; (3) second applox. appply newton method
;;      r = r * (2.0q0 - b *r)
;;      s = a * r
;; (4) final approx. calculate exact rounding
;; (4-1) highly accurate calc. of a - s * b 
;;      su = s & 0xffffffffffffffff fe00000000000000
;;      sl = s - su
;;      bu = b & 0xffffffffffffffff fe00000000000000
;;      bl = b - bu
;;      r = a - su * bu - su * bl - sl * bu - sl * bl
;; (4-2) get final delta
;;      rh(DF) = r
;;      rh = rh / bh
;;      r = rh
;;      s = s + r
;; (5) set proper exponet and sign bit
;;     sign,expo(s) +=  sign,expo(a) - sign,expo(b) 
;; (6) move s to c  
;;      This is necessary since the register allocator may 
;;      assign the same register for c and any temporaries
;; 


; This pattern is not used. Why?
(define_insn "<maxmin_insn><mode>3"
  [(set (match_operand:MODEF 0 "register_operand" "=r")
        (maxmin:MODEF (match_operand:MODEF 1 "register_operand" "%r")
                    (match_operand:MODEF 2 "register_operand" "r")))]
  ""
  "f<maxmin_code>.<suffix>\\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "mode"     "<MODE>")])

(define_insn "*addsi3_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (any_extend:DI 
        (plus:SI (match_operand:SI 1 "register_operand" "%r,r,r")
                 (match_operand:SI 2 "reg_or_io_operand" "r,I,O"))))]
  ""
  "@
   adds.w.<ex>\\t%0,%1,%2
   adds.w.<ex>\\t%0,%2,%1
   adds.w.<ex>\\t%0,%1,%O2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")
   (set_attr "length" "1,1,1")])

(define_insn "*addsi4_r_rc"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (match_operand:SI 1 "register_operand" "r")
                 (plus:SI (match_operand:SI 2 "register_operand" "%r")
                          (match_operand:SI 3 "const_n_operand" "N"))))]
  ""
  "lea\\t%0,%3(%1,%2)"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")
   (set_attr "length" "1")])

(define_insn "*addsi4_c_rr"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (match_operand:SI 1 "const_n_operand" "N")
                 (plus:SI (match_operand:SI 2 "register_operand" "r")
                          (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "lea\\t%0,%1(%2,%3)"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")
   (set_attr "length" "1")])

(define_insn "*addsi4_rc_r"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (plus:SI (match_operand:SI 1 "register_operand" "%r")
                          (match_operand:SI 2 "const_n_operand" "N"))
                 (match_operand:SI 3 "register_operand" "r")))]
  ""
  "lea\\t%0,%2(%1,%3)"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")
   (set_attr "length" "1")])

(define_insn "*addsi4_rr_c"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "register_operand" "r"))
                 (match_operand:SI 3 "const_n_operand" "N")))]
  ""
  "lea\\t%0,%3(%1,%2)"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")
   (set_attr "length" "1")])

(define_insn "*adddi4_r_rc"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (plus:DI (match_operand:DI 1 "register_operand" "r,r")
                 (plus:DI (match_operand:DI 2 "register_operand" "%r,r")
                          (match_operand:DI 3 "const_int_operand" "N,i"))))]
  ""
  "*
{
  rtx xop[5];
  switch(which_alternative)
  {
     default:
     case 0:
        return \"lea\\t%0,%3(%1,%2)\";
     case 1:
        xop[0] = operands[0];
        xop[1] = operands[1];
        xop[2] = operands[2];
        xop[3] = operands[3];
        xop[4] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        output_asm_insn(\"lea.sl\\t%4,%H3(%1)\\n\\tlea\\t%0,%L3(%2,%4)\",xop);
        return \"\";
  }
}"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "DI")
   (set_attr "length" "1,2")])

(define_insn "*adddi4_lc_rr"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (plus:DI (match_operand:DI 1 "const_int_operand" "N,i")
                 (plus:DI (match_operand:DI 2 "register_operand" "r,r")
                          (match_operand:DI 3 "register_operand" "r,r"))))]
  ""
  "*
{
  rtx xop[5];
  switch(which_alternative)
  {
     default:
     case 0:
        return \"lea\\t%0,%1(%2,%3)\";
     case 1:
        xop[0] = operands[0];
        xop[1] = operands[1];
        xop[2] = operands[2];
        xop[3] = operands[3];
        xop[4] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        output_asm_insn(\"lea.sl\\t%4,%H1(%2)\\n\\tlea\\t%0,%L1(%3,%4)\",xop);
        return \"\";
  }
}"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "DI")
   (set_attr "length" "1,2")])

(define_insn "*adddi4_rc_r"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (plus:DI (plus:DI (match_operand:DI 1 "register_operand" "%r,r")
                          (match_operand:DI 2 "const_int_operand" "N,i"))
                 (match_operand:DI 3 "register_operand" "r,r")))]
  ""
  "*
{
  rtx xop[5];
  switch(which_alternative)
  {
     default:
     case 0:
        return \"lea\\t%0,%2(%1,%3)\";
     case 1:
        xop[0] = operands[0];
        xop[1] = operands[1];
        xop[2] = operands[2];
        xop[3] = operands[3];
        xop[4] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        output_asm_insn(\"lea.sl\\t%4,%H2(%1)\\n\\tlea\\t%0,%L2(%3,%4)\",xop);
        return \"\";
  }
}"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "DI")
   (set_attr "length" "1,2")])

(define_insn "*adddi4_rr_c"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (plus:DI (plus:DI (match_operand:DI 1 "register_operand" "r,r")
                          (match_operand:DI 2 "register_operand" "r,r"))
                 (match_operand:DI 3 "const_int_operand" "N,i")))]
  ""
  "*
{
  rtx xop[5];
  switch(which_alternative)
  {
     default:
     case 0:
        return \"lea\\t%0,%3(%1,%2)\";
     case 1:
        xop[0] = operands[0];
        xop[1] = operands[1];
        xop[2] = operands[2];
        xop[3] = operands[3];
        xop[4] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        output_asm_insn(\"lea.sl\\t%4,%H3(%1)\\n\\tlea\\t%0,%L3(%2,%4)\",xop);
        return \"\";
  }
}"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "DI")
   (set_attr "length" "1,2")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
        (plus:SI (match_operand:SI 1 "register_operand" "%r,r,r,r")
                 (match_operand:SI 2 "reg_or_in_operand" "r,I,O,N")))]
  ""
  "@
   adds.w.sx\\t%0,%1,%2
   adds.w.sx\\t%0,%2,%1
   adds.w.sx\\t%0,%1,%O2
   lea\\t%0,%2(,%1)"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")
   (set_attr "length" "1,1,1,1")])

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r,r")
        (plus:DI (match_operand:DI 1 "register_operand" "%r,r,r,r,r")
                 (match_operand:DI 2 "reg_or_cint_operand" "r,I,M,N,i")))]
  ""
  "*
{
  rtx xop[4];
  switch(which_alternative)
  {
     default:
     case 0:
        return \"adds.l\\t%0,%1,%2\";
     case 1:
        return \"adds.l\\t%0,%2,%1\";
     case 2:
        return \"adds.l\\t%0,%1,%M2\";
     case 3:
        return \"lea\\t%0,%2(,%1)\";
     case 4:
        xop[0] = operands[0];
        xop[1] = operands[1];
        xop[2] = operands[2];
        xop[3] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        output_asm_insn(\"lea\\t%3,%L2\\n\\tlea.sl\\t%0,%H2(%3,%1)\",xop);
        return \"\";
  }

}"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "DI")
   (set_attr "length" "1,1,1,1,2")])

(define_insn "addti3"
  [(set (match_operand:TI 0 "register_operand" "=&r,&r,&r,&r")
        (plus:TI (match_operand:TI 1 "register_operand" "%r,r,r,r")
                 (match_operand:TI 2 "reg_or_in_operand" "r,I,M,N")))
   (clobber (match_scratch:DI 3 "=&r,&r,&r,&r"))]
  ""
  "*
{
  rtx xop[5];
  unsigned long long c;
  switch(which_alternative)
  {
     default:
     case 0: return 
\"addu.l\\t%Q0,%Q1,%Q2\\n\\
\\tsubu.l\\t%3,-1,%Q2\\n\\
\\tadds.l\\t%0,%1,%2\\n\\
\\tcmpu.l\\t%3,%3,%Q1\\n\\
\\tsrl\\t%3,%3,63\\n\\
\\tadds.l\\t%0,%3,%0\";

     case 1: 
      c = 0xffffffffffffffff;
      c = c - UINTVAL(operands[2]);
      xop[0] = operands[0];
      xop[1] = operands[1];
      xop[2] = operands[2];
      xop[3] = operands[3];
      xop[4] = GEN_INT(c);
      output_asm_insn(\"addu.l\\t%Q0,%2,%Q1\",xop);
      output_asm_insn(\"cmpu.l\\t%3,%4,%Q1\",xop);
      output_asm_insn(\"srl\\t%3,%3,63\",xop);
      if (INTVAL(operands[2]) >= 0) 
      {
        output_asm_insn(\"adds.l\\t%0,%3,%1\",xop);
      }
      else
      {
       output_asm_insn(\"adds.l\\t%0,%Z2,%1\",xop);
       output_asm_insn(\"adds.l\\t%0,%3,%0\",xop);
      }
      return  \"\";

     case 2: 
      c = 0xffffffffffffffff;
      c = c - UINTVAL(operands[2]);
      xop[0] = operands[0];
      xop[1] = operands[1];
      xop[2] = operands[2];
      xop[3] = operands[3];
      xop[4] = GEN_INT(c);
      output_asm_insn(\"addu.l\\t%Q0,%Q1,%M2\",xop);
      output_asm_insn(\"or\\t%3,0,%M4\",xop);
      output_asm_insn(\"cmpu.l\\t%3,%3,%Q1\",xop);
      output_asm_insn(\"srl\\t%3,%3,63\",xop);
      if (INTVAL(operands[2]) >= 0) 
      {
        output_asm_insn(\"adds.l\\t%0,%3,%1\",xop);
      }
      else
      {
       output_asm_insn(\"adds.l\\t%0,%Z2,%1\",xop);
       output_asm_insn(\"adds.l\\t%0,%3,%0\",xop);
      }
      return  \"\";

     case 3: 
      c = 0xffffffffffffffff;
      c = c - UINTVAL(operands[2]);
      xop[0] = operands[0];
      xop[1] = operands[1];
      xop[2] = operands[2];
      xop[3] = operands[3];
      xop[4] = GEN_INT(c);
      output_asm_insn(\"lea\\t%Q0,%2(,%Q1)\",xop);
      output_asm_insn(\"lea\\t%3,%4\",xop);
      output_asm_insn(\"cmpu.l\\t%3,%3,%Q1\",xop);
      output_asm_insn(\"srl\\t%3,%3,63\",xop);
      if (INTVAL(operands[2]) >= 0) 
      {
        output_asm_insn(\"adds.l\\t%0,%3,%1\",xop);
      }
      else
      {
       output_asm_insn(\"adds.l\\t%0,%Z2,%1\",xop);
       output_asm_insn(\"adds.l\\t%0,%3,%0\",xop);
      }
      return  \"\";
  }
}"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "TI")
   (set_attr "length" "6,5,6,6")])

(define_insn "sub<mode>3"
  [(set (match_operand:MODEI 0 "register_operand" "=r,r,r")
        (minus:MODEI 
          (match_operand:MODEI 1 "reg_or_i_operand" "r,I,r")
          (match_operand:MODEI 2 "<reg_or_o_operand>" "r,r,<o_const>")))]
  ""
  "@
   subs.<suffix><ext>\\t%0,%1,%2
   subs.<suffix><ext>\\t%0,%1,%2
   subs.<suffix><ext>\\t%0,%1,%<o_const>2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "<MODE>")
   (set_attr "length" "1,1,1")])

(define_insn "*subsi3_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (any_extend:DI
        (minus:SI (match_operand:SI 1 "reg_or_i_operand" "r,I,r")
                  (match_operand:SI 2 "reg_or_o_operand" "r,r,O"))))]
  ""
  "@
   subs.w.<ex>\\t%0,%1,%2
   subs.w.<ex>\\t%0,%1,%2
   subs.w.<ex>\\t%0,%1,%O2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")
   (set_attr "length" "1,1,1")])

(define_insn "subti3"
  [(set (match_operand:TI 0 "register_operand" "=&r,&r")
        (minus:TI (match_operand:TI 1 "reg_or_i_operand" "r,I")
                  (match_operand:TI 2 "register_operand" "r,r")))
   (clobber (match_scratch:DI 3 "=&r,&r"))]
  ""
  "*
{
  switch(which_alternative)
  {
     default:
     case 0: return 
\"subu.l\\t%Q0,%Q1,%Q2\\n\\
\\tsubs.l\\t%0,%1,%2\\n\\
\\tcmpu.l\\t%3,%Q1,%Q2\\n\\
\\tsrl\\t%3,%3,63\\n\\
\\tsubs.l\\t%0,%0,%3\";

     case 1: return
\"subu.l\\t%Q0,%1,%Q2\\n\\
\\tsubs.l\\t%0,%Z1,%2\\n\\
\\tcmpu.l\\t%3,%1,%Q2\\n\\
\\tsrl\\t%3,%3,63\\n\\
\\tsubs.l\\t%0,%0,%3\";
  }
}"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "TI")
   (set_attr "length" "5,5")])

;; and, ior, xor

(define_insn "<logic_insn>si3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (any_logic:SI (match_operand:SI 1 "register_operand" "%r,r,r")
                (match_operand:SI 2 "reg_or_ip_operand" "r,I,P")))]
  ""
  "@
   <logic_code>\\t%0,%1,%2
   <logic_code>\\t%0,%2,%1
   <logic_code>\\t%0,%1,%P2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1,1,1")])

(define_insn "<logic_insn>di3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (any_logic:DI (match_operand:DI 1 "register_operand" "%r,r,r")
                (match_operand:DI 2 "reg_or_im_operand" "r,I,M")))]
  ""
  "@
   <logic_code>\\t%0,%1,%2
   <logic_code>\\t%0,%2,%1
   <logic_code>\\t%0,%1,%M2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1,1")])

(define_insn "<logic_insn>ti3"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (any_logic:TI (match_operand:TI 1 "register_operand" "%r")
                (match_operand:TI 2 "register_operand" "r")))]
  ""
  "<logic_code>\\t%Q0,%Q1,%Q2\\n\\
\\t<logic_code>\\t%0,%1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "2")])

;; loading zero, population count, parity

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (clz:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))
   (clobber (match_scratch:DI 3 "=&r"))]
  ""
  "sll\\t%2,%1,32\\n\\tor\\t%3,%2,(32)0\\n\\tldz\\t%0,%3"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "3")])

(define_insn "clzdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (clz:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "ldz\\t%0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_insn "clzti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (clz:TI (match_operand:TI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "ldz\\t%Q0,%1
\\tbrne.l\\t%1,0,24\\n\\
\\tldz\\t%2,%Q1\\n\\
\\taddu.l\\t%Q0,%Q0,%2\\n\\
\\tor\\t%0,0,(0)1"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "5")])

(define_insn "ctzsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ctz:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))
   (clobber (match_scratch:DI 3 "=&r"))]
  ""
  "brv\\t%2,%1\\n\\tor\\t%3,%2,(32)0\\n\\tldz\\t%0,%3"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "2")])

(define_insn "ctzdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (ctz:DI (match_operand:DI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "brv\\t%2,%1\\n\\tldz\\t%0,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "2")])

(define_insn "ctzti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (ctz:TI (match_operand:TI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "brv\\t%2,%Q1\\n\\
\\tldz\\t%Q0,%2\\n\\
\\tbrne.l\\t%2,0,32\\n\\
\\tbrv\\t%2,%1\\n\\
\\tldz\\t%2,%2\\n\\
\\taddu.l\\t%Q0,%Q0,%2\\n\\
\\tor\\t%0,0,(0)1"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "7")])

(define_insn "popcountsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (popcount:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "and\\t%2,%1,(32)0\\n\\tpcnt\\t%0,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "2")])

(define_insn "popcountdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (popcount:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "pcnt\\t%0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_insn "popcountti2"
  [(set (match_operand:TI 0 "register_operand" "=r")
        (popcount:TI (match_operand:TI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))
   (clobber (match_scratch:DI 3 "=&r"))]
  ""
  "pcnt\\t%2,%Q1\\n\\
\\tpcnt\\t%3,%1\\n\\
\\tor\\t%0,0,(0)1\\n\\
\\taddu.l\\t%Q0,%2,%3"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "4")])

(define_insn "parity<mode>2"
  [(set (match_operand:MODEI 0 "register_operand" "=r")
        (parity:MODEI (match_operand:MODEI 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "pcnt\\t%2,%1\\n\\tand\\t%0,1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "2")])

(define_insn "parityti2"
  [(set (match_operand:TI 0 "register_operand" "=r")
        (parity:TI (match_operand:TI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))
   (clobber (match_scratch:DI 3 "=&r"))]
  ""
  "pcnt\\t%2,%Q1\\n\\
\\tpcnt\\t%3,%1\\n\\
\\taddu.l\\t%2,%2,%3\\n\\
\\tor\\t%0,0,(0)1\\n\\
\\tand\\t%Q0,1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "5")])

; Byte swap
(define_insn "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (bswap:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "bswp\\t%2,%1,1\\n\\tadds.w.sx\\t%0,0,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "2")])

(define_insn "*bswapsi2_zero_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI
           (bswap:SI (match_operand:SI 1 "register_operand" "r"))))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "bswp\\t%2,%1,1\\n\\tand\\t%0,%2,(32)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "2")])

(define_insn "*bswapsi2_sign_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (sign_extend:DI
           (bswap:SI (match_operand:SI 1 "register_operand" "r"))))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "bswp\\t%2,%1,1\\n\\tadds.w.sx\\t%0,0,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "2")])

(define_insn "bswapdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (bswap:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "bswp\\t%0,%1,0"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_insn "bswapti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (bswap:TI (match_operand:TI 1 "register_operand" "r")))]
  ""
  "bswp\\t%0,%Q1,0\\n\\tbswp\\t%Q0,%1,0"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "2")])

;;
;; -x = 0-x
;; This is ok for integers.
;;

;; SI, DI, and TI mode
(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "subs.w.sx\\t%0,0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

(define_insn "*negsi2_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI
        (neg:SI (match_operand:SI 1 "register_operand" "r"))))]
  ""
  "subs.w.<ex>\\t%0,0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (neg:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "subs.l\\t%0,0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

(define_insn "negti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (neg:TI (match_operand:TI 1 "register_operand" "r")))]
  ""
  "subs.l\\t%0,-1,%1\\n\\
\\tsubs.l\\t%Q0,0,%1\\n\\
\\tcmov.l.eq\\t%0,%Q0,%Q1\\n\\
\\tsubu.l\\t%Q0,0,%Q1"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "4")])

; allows negative zero

(define_insn "neg<mode>2"
  [(set (match_operand:MODEF 0 "register_operand" "=r")
        (neg:MODEF (match_operand:MODEF 1 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,(1)1"
  [(set_attr "type" "alu")
   (set_attr "mode" "<MODE>")])

;  "fsub.<suffix>\\t%0,0,%1"

(define_insn "negtf2"
  [(set (match_operand:TF 0 "register_operand" "=r,r")
        (neg:TF (match_operand:TF 1 "register_operand" "0,r")))]
  ""
  "@
   xor\\t%0,%0,(1)1
   xor\\t%0,%1,(1)1\\n\\tor\\t%Q0,0,%Q1"
  [(set_attr "type" "alu")
   (set_attr "mode" "TF")
   (set_attr "length" "1,2")])

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (not:QI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,(0)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "QI")
   (set_attr "length" "1")])
;;  "xor\\t%0,%1,(56)0"

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (not:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,(0)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "HI")
   (set_attr "length" "1")])
;;  "xor\\t%0,%1,(48)0"

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,(0)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1")])
;;  "xor\\t%0,%1,(32)0"

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (not:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,(0)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_insn "one_cmplti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (not:TI (match_operand:TI 1 "register_operand" "r")))]
  ""
  "xor\\t%Q0,%Q1,(0)0\\n\\txor\\t%0,%1,(0)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "2")])

(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (abs:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "subs.w.sx\\t%0,0,%1\\n\\
\\tcmov.w.ge\\t%0,%1,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "2")])

(define_insn "*abssi2_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (any_extend:DI
        (abs:SI (match_operand:SI 1 "register_operand" "r"))))]
  ""
  "subs.w.sx\\t%0,0,%1\\n\\
\\tcmov.w.ge\\t%0,%1,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "2")])

(define_insn "absdi2"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (abs:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "subs.l\\t%0,0,%1\\n\\
\\tcmov.l.ge\\t%0,%1,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "2")])

(define_insn "absti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (abs:TI (match_operand:TI 1 "register_operand" "r")))]
  ""
  "subs.l\\t%0,-1,%1\\n\\
\\tsubs.l\\t%Q0,0,%1\\n\\
\\tcmov.l.eq\\t%0,%Q0,%Q1\\n\\
\\tsubu.l\\t%Q0,0,%Q1\\n\\
\\tcmov.l.ge\\t%0,%1,%1\\n\\
\\tcmov.l.ge\\t%Q0,%Q1,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "6")])

(define_insn "abs<mode>2"
  [(set (match_operand:MODEF 0 "register_operand" "=r")
        (abs:MODEF (match_operand:MODEF 1 "register_operand" "r")))]
  ""
  "and\\t%0,%1,(1)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1")])

(define_insn "abstf2"
  [(set (match_operand:TF 0 "register_operand" "=r,r")
        (abs:TF (match_operand:TF 1 "register_operand" "0,r")))]
  ""
  "@
   and\\t%0,%0,(1)0
   and\\t%0,%1,(1)0\\n\\tor\\t%Q0,0,%Q1"
  [(set_attr "type" "alu")
   (set_attr "mode" "TF")
   (set_attr "length" "1,2")])

(define_insn "copysign<mode>3"
  [(set (match_operand:MODEF 0 "register_operand" "=&r,&r")
        (unspec:MODEF [ 
          (match_operand:MODEF 1 "register_operand" "0,r")
          (match_operand:MODEF 2 "register_operand" "r,r")]
          UNSPEC_COPYSIGN))]
  ""
  "@
   mrg\\t%0,%2,(1)1
   or\\t%0,0,%1\\n\\tmrg\\t%0,%2,(1)1"
  [(set_attr "type" "alu")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1,2")])

(define_insn "copysigntf3"
  [(set (match_operand:TF 0 "register_operand" "=&r,&r")
        (unspec:TF [ 
          (match_operand:TF 1 "register_operand" "0,r")
          (match_operand:TF 2 "register_operand" "r,r")]
          UNSPEC_COPYSIGN))]
  ""
  "@
  mrg\\t%0,%2,(1)1
  or\\t%0,0,%1\\n\\tmrg\\t%0,%2,(1)1\\n\\tor\\t%Q0,0,%Q1"
  [(set_attr "type" "alu")
   (set_attr "mode" "TF")
   (set_attr "length" "1,3")])



;; ashlsi3 and ashrsi3
(define_insn "<shift_insn_w>si3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (any_ashift:SI (match_operand:SI 1 "reg_or_o_operand" "r,r,O")
                       (match_operand:SI 2 "reg_or_l_operand" "r,L,r")))]
  ""
  "@
   <shift_code_w>.sx\\t%0,%1,%2
   <shift_code_w>.sx\\t%0,%1,%2
   <shift_code_w>.sx\\t%0,%O1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

(define_insn "*<shift_insn_w>si3_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (any_extend:DI
        (any_ashift:SI (match_operand:SI 1 "reg_or_o_operand" "r,r,O")
                       (match_operand:SI 2 "reg_or_l_operand" "r,L,r"))))]
  ""
  "@
   <shift_code_w>.<ex>\\t%0,%1,%2
   <shift_code_w>.<ex>\\t%0,%1,%2
   <shift_code_w>.<ex>\\t%0,%O1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

;;sign extension is necessary in case of zero shift

(define_insn "*lshrsi31_addsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI 
         (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
                      (const_int 31))
         (match_dup 1)))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "sra.w.sx\\t%2,%1,31\\n\\tsubs.w.sx\\t%0,%1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "3")])

(define_insn "*lshrsi_addsi4"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
        (plus:SI 
         (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r,r,r")
                      (match_operand:SI 2 "reg_or_l_operand" "r,L,r,L"))
         (match_operand:SI 3 "reg_or_i_operand" "r,r,I,I")))
   (clobber (match_scratch:SI 4 "=&r,&r,&r,&r"))
   (clobber (match_scratch:SI 5 "=&r,&r,&r,&r"))]
  ""
  "*
{
  switch(which_alternative) 
  {
   default: case 0: case 2:
    return 
\"and\\t%4,%1,(32)0\\n\\
\\tand\\t%5,%2,(59)0\\n\\
\\tsrl\\t%5,%4,%5\\n\\
\\tadds.w.sx\\t%0,%3,%5\";
   case 1: case 3:
    if ((UINTVAL(operands[2]) & 0x1f) == 0)
     return \"adds.w.sx\\t%0,%3,%1\";
   return \"and\\t%4,%1,(32)0\\n\\
\\tsrl\\t%5,%4,%V2\\n\\
\\tadds.w.sx\\t%0,%3,%5\";
  }
}"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "4,3,4,3")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (lshiftrt:SI (match_operand:SI 1 "reg_or_i_operand" "r,r,I")
                     (match_operand:SI 2 "reg_or_l_operand" "r,L,r")))
   (clobber (match_scratch:SI 3 "=&r,&r,&r"))
   (clobber (match_scratch:SI 4 "=&r,X,&r"))]
  ""
  "*
{
  switch(which_alternative)
  {
     default: case 0: case 2:
      return 
\"and\\t%3,%1,(32)0\\n\\
\\tand\\t%4,%2,(59)0\\n\\
\\tsrl\\t%4,%3,%4\\n\\
\\tadds.w.sx\\t%0,0,%4\";
     case 1:
      if ((UINTVAL(operands[2]) & 0x1f) == 0)
       return \"adds.w.sx\\t%0,0,%1\";
      return \"and\\t%3,%1,(32)0\\n\\tsrl\\t%0,%3,%V2\";
  }
}"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "4,2,4")])
; Note: no sign extension is necessary in case of non-zero const shift

(define_insn "*lshrsi3_sign_extend"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (sign_extend:DI
         (lshiftrt:SI (match_operand:SI 1 "reg_or_i_operand" "r,r,I")
                     (match_operand:SI 2 "reg_or_l_operand" "r,L,r"))))
   (clobber (match_scratch:SI 3 "=&r,&r,&r"))
   (clobber (match_scratch:SI 4 "=&r,X,&r"))]
  ""
  "*
{
  switch(which_alternative)
  {
     default: case 0: case 2:
      return 
\"and\\t%3,%1,(32)0\\n\\
\\tand\\t%4,%2,(59)0\\n\\
\\tsrl\\t%4,%3,%4\\n\\
\\tadds.w.sx\\t%0,0,%4\";
     case 1:
      if ((UINTVAL(operands[2]) & 0x1f) == 0)
       return \"adds.w.sx\\t%0,0,%1\";
      return \"and\\t%3,%1,(32)0\\n\\tsrl\\t%0,%3,%V2\";
  }
}"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "4,2,4")])

(define_insn "*lshrsi3_zero_extend"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (zero_extend:DI
         (lshiftrt:SI (match_operand:SI 1 "reg_or_i_operand" "r,r,I")
                     (match_operand:SI 2 "reg_or_l_operand" "r,L,r"))))
   (clobber (match_scratch:SI 3 "=&r,&r,&r"))
   (clobber (match_scratch:SI 4 "=&r,X,&r"))]
  ""
  "*
{
   switch(which_alternative)
   {
     default: case 0: case 2:
      return \"and\\t%3,%1,(32)0\\n\\tand\\t%4,%2,(59)0\\n\\tsrl\\t%0,%3,%4\";
     case 1:
      if ((UINTVAL(operands[2]) & 0x1f) == 0)
       return \"and\\t%0,%1,(32)0\";
      return \"and\\t%3,%1,(32)0\\n\\tsrl\\t%0,%3,%V2\";
   }
}"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "3,2,3")])

(define_insn "<shift_insn_l>di3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (any_shift:DI (match_operand:DI 1 "reg_or_m_operand" "r,r,M")
                      (match_operand:SI 2 "reg_or_l_operand" "r,L,r")))]
  ""
  "@
   <shift_code_l>\\t%0,%1,%2
   <shift_code_l>\\t%0,%1,%2
   <shift_code_l>\\t%0,%M1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

(define_insn "lshrti3"
  [(set (match_operand:TI 0 "register_operand" "=&r,&r")
        (lshiftrt:TI (match_operand:TI 1 "register_operand" "r,r")
                     (match_operand:SI 2 "reg_or_l_operand" "r,L")))]
  ""
  "@
   or\\t%Q0,0,%Q1\\n\\tsrd\\t%Q0,%1,%2\\n\\tor\\t%0,0,%1\\n\\tsrd\\t%0,(0)1,%2
   or\\t%Q0,0,%Q1\\n\\tsrd\\t%Q0,%1,%2\\n\\tor\\t%0,0,%1\\n\\tsrd\\t%0,(0)1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "4,4")])

(define_insn "ashlti3"
  [(set (match_operand:TI 0 "register_operand" "=&r,&r")
        (ashift:TI (match_operand:TI 1 "register_operand" "r,r")
                   (match_operand:SI 2 "reg_or_l_operand" "r,L")))]
  ""
  "@
   or\\t%0,0,%1\\n\\tsld\\t%0,%Q1,%2\\n\\tor\\t%Q0,0,%Q1\\n\\tsld\\t%Q0,(0)1,%2
   or\\t%0,0,%1\\n\\tsld\\t%0,%Q1,%2\\n\\tor\\t%Q0,0,%Q1\\n\\tsld\\t%Q0,(0)1,%2"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "4,4")])

(define_insn "ashrti3"
  [(set (match_operand:TI 0 "register_operand" "=&r,&r")
        (ashiftrt:TI (match_operand:TI 1 "register_operand" "r,r")
                     (match_operand:SI 2 "reg_or_l_operand" "r,L")))
   (clobber (match_scratch:DI 3 "=&r,&r"))
   (clobber (match_scratch:DI 4 "=&r,&r"))
   (clobber (match_scratch:DI 5 "=&r,&r"))]
  ""
  "@
   sra.l\\t%3,%1,63\\n\\txor\\t%4,%1,%3\\n\\txor\\t%5,%Q1,%3\\n\\tsrd\\t%5,%4,%2\\n\\tsrd\\t%4,(0)1,%2\\n\\txor\\t%Q0,%5,%3\\n\\txor\\t%0,%4,%3
   sra.l\\t%3,%1,63\\n\\txor\\t%4,%1,%3\\n\\txor\\t%5,%Q1,%3\\n\\tsrd\\t%5,%4,%2\\n\\tsrd\\t%4,(0)1,%2\\n\\txor\\t%Q0,%5,%3\\n\\txor\\t%0,%4,%3"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "7,7")])

(define_insn "mul<mode>3"
  [(set (match_operand:MODEI 0 "register_operand" "=r,r,r")
        (mult:MODEI
              (match_operand:MODEI 1 "register_operand" "%r,r,r")
              (match_operand:MODEI 2 "<reg_or_io_operand>" "r,I,<o_const>")))]
  ""
  "@
   muls.<suffix><ext>\\t%0,%1,%2
   muls.<suffix><ext>\\t%0,%2,%1
   muls.<suffix><ext>\\t%0,%1,%<o_const>2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "<MODE>")])

(define_insn "*mulsi3_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (any_extend:DI
        (mult:SI (match_operand:SI 1 "register_operand" "%r,r,r")
                 (match_operand:SI 2 "reg_or_io_operand" "r,I,O"))))]
  ""
  "@
   muls.w.<ex>\\t%0,%1,%2
   muls.w.<ex>\\t%0,%2,%1
   muls.w.<ex>\\t%0,%1,%O2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")])

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "%r,r,r"))
                 (sign_extend:DI (match_operand:SI 2 "reg_or_io_operand" "r,I,O"))))]
  ""
  "@
   muls.l.w\\t%0,%1,%2
   muls.l.w\\t%0,%2,%1
   muls.l.w\\t%0,%1,%O2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")])

(define_insn "div<mode>3"
  [(set (match_operand:MODEI 0 "register_operand" "=r,r,r")
        (div:MODEI 
              (match_operand:MODEI 1 "reg_or_i_operand" "r,I,r")
              (match_operand:MODEI 2 "<reg_or_o_operand>" "r,r,<o_const>")))]
  ""
  "@
   divs.<suffix><ext>\\t%0,%1,%2
   divs.<suffix><ext>\\t%0,%1,%2
   divs.<suffix><ext>\\t%0,%1,%<o_const>2"
  [(set_attr "type" "fp")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1,1,1")])

(define_insn "*divsi3_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (any_extend:DI
        (div:SI (match_operand:SI 1 "reg_or_i_operand" "r,I,r")
                (match_operand:SI 2 "reg_or_o_operand" "r,r,O"))))]
  ""
  "@
   divs.w.<ex>\\t%0,%1,%2
   divs.w.<ex>\\t%0,%1,%2
   divs.w.<ex>\\t%0,%1,%O2"
  [(set_attr "type" "fp")
   (set_attr "mode" "SI")
   (set_attr "length" "1,1,1")])

(define_insn "udiv<mode>3"
  [(set (match_operand:MODEI 0 "register_operand" "=r,r,r")
        (udiv:MODEI 
             (match_operand:MODEI 1 "reg_or_i_operand" "r,I,r")
             (match_operand:MODEI 2 "<reg_or_o_operand>" "r,r,<o_const>")))]
  ""
  "@
   divu.<suffix>\\t%0,%1,%2
   divu.<suffix>\\t%0,%1,%2
   divu.<suffix>\\t%0,%1,%<o_const>2"
  [(set_attr "type" "fp")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1,1,1")])

(define_insn "s<maxmin_insn><mode>3"
  [(set (match_operand:MODEI 0 "register_operand" "=r,r,r")
        (maxmin:MODEI
             (match_operand:MODEI 1 "register_operand" "%r,r,r")
             (match_operand:MODEI 2 "<reg_or_io_operand>" "r,I,<o_const>")))]
  ""
  "@
   <maxmin_code>s.<suffix><ext>\\t%0,%1,%2
   <maxmin_code>s.<suffix><ext>\\t%0,%2,%1
   <maxmin_code>s.<suffix><ext>\\t%0,%1,%<o_const>2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "<MODE>")])

(define_insn "*s<maxmin_insn>_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (any_extend:DI
        (maxmin:SI (match_operand:SI 1 "register_operand" "%r,r,r")
                   (match_operand:SI 2 "reg_or_io_operand" "r,I,O"))))]
  ""
  "@
   <maxmin_code>s.w.<ex>\\t%0,%1,%2
   <maxmin_code>s.w.<ex>\\t%0,%2,%1
   <maxmin_code>s.w.<ex>\\t%0,%1,%O2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SI")])

;;
;; Conversion patterns
;;

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (float:SF (match_operand:SI 1 "register_operand" "r")))]
  ""
  "cvt.s.w\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "1") ])

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (float:SF (match_operand:DI 1 "register_operand" "r")))
   (clobber (match_scratch:DF 2 "=&r"))]
  ""
  "cvt.d.l\\t%2,%1\\n\\
\\tcvt.s.d\\t%0,%2"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "2") ])

(define_insn "float<mode>df2"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (float:DF (match_operand:MODEI 1 "register_operand" "r")))]
  ""
  "cvt.d.<suffix>\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

(define_insn "floatsitf2"
  [(set (match_operand:TF 0 "register_operand" "=r")
        (float:TF (match_operand:SI 1 "register_operand" "r")))
   (clobber (match_scratch:DF 2 "=&r"))]
  ""
  "cvt.d.w\\t%2,%1\\n\\tcvt.q.d\\t%0,%2"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "2")])

;; full 64bit support
(define_insn "floatditf2"
  [(set (match_operand:TF 0 "register_operand" "=r")
        (float:TF (match_operand:DI 1 "register_operand" "r")))
   (clobber (match_scratch:TF 2 "=&r"))
   (clobber (match_scratch:TF 3 "=&r"))
   (clobber (match_scratch:DI 4 "=&r"))]
  ""
  "cvt.d.l\\t%4,%1\\n\\
\\tcvt.q.d\\t%2,%4\\n\\
\\tcvt.l.d\\t%4,%4\\n\\
\\tsubs.l\\t%4,%1,%4\\n\\
\\tcvt.d.l\\t%4,%4\\n\\
\\tcvt.q.d\\t%3,%4\\n\\
\\tfadd.q\\t%0,%2,%3"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "7")])

(define_insn "floattisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (float:SF (match_operand:TI 1 "register_operand" "r")))
   (clobber (match_scratch:DF 2 "=&r"))
   (clobber (match_scratch:DF 3 "=&r"))]
  ""
  "adds.l\\t%2,1,%1\\n\\
\\tcmov.l.ge\\t%2,%1,%Q1\\n\\
\\tcvt.d.l\\t%2,%2\\n\\
\\tlea.sl\\t%3,0x43f00000 # 2^64\\n\\
\\tfmul.d\\t%2,%2,%3\\n\\
\\tcvt.d.l\\t%3,%Q1\\n\\
\\tfadd.d\\t%2,%2,%3\\n\\
\\tcvt.s.d\\t%0,%2"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "8") ])

(define_insn "floattidf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (float:DF (match_operand:TI 1 "register_operand" "r")))
   (clobber (match_scratch:DF 2 "=&r"))
   (clobber (match_scratch:DF 3 "=&r"))]
  ""
  "adds.l\\t%2,1,%1\\n\\
\\tcmov.l.ge\\t%2,%1,%Q1\\n\\
\\tcvt.d.l\\t%2,%2\\n\\
\\tlea.sl\\t%3,0x43f00000 # 2^64\\n\\
\\tfmul.d\\t%2,%2,%3\\n\\
\\tcvt.d.l\\t%3,%Q1\\n\\
\\tfadd.d\\t%0,%2,%3"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "5") ])

(define_insn "floattitf2"
  [(set (match_operand:TF 0 "register_operand" "=r")
        (float:TF (match_operand:TI 1 "register_operand" "r")))
   (clobber (match_scratch:TF 2 "=&r"))
   (clobber (match_scratch:TF 3 "=&r"))
   (clobber (match_scratch:TF 4 "=&r"))
   (clobber (match_scratch:TF 5 "=&r"))
   (clobber (match_scratch:DI 6 "=&r"))]
  ""
  "adds.l\\t%3,1,%1\\n\\
\\tcmov.l.ge\\t%3,%1,%Q1\\n\\
\\tcvt.d.l\\t%6,%3\\n\\
\\tcvt.q.d\\t%2,%6\\n\\
\\tcvt.l.d\\t%6,%6\\n\\
\\tsubs.l\\t%6,%3,%6\\n\\
\\tcvt.d.l\\t%6,%6\\n\\
\\tcvt.q.d\\t%3,%6\\n\\
\\tfadd.q\\t%2,%2,%3\\n\\
\\tlea.sl\\t%4,0x403f0000 # 2^64\\n\\
\\tor\\t%Q4,0,(0)1\\n\\
\\tfmul.q\\t%4,%2,%4\\n\\
\\tcvt.d.l\\t%6,%Q1\\n\\
\\tcvt.q.d\\t%2,%6\\n\\
\\tcvt.l.d\\t%6,%6\\n\\
\\tsubs.l\\t%6,%Q1,%6\\n\\
\\tcvt.d.l\\t%6,%6\\n\\
\\tcvt.q.d\\t%3,%6\\n\\
\\tfadd.q\\t%2,%2,%3\\n\\
\\tfadd.q\\t%0,%4,%2"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "20")])

(define_insn "floatunsdisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (unsigned_float:SF (match_operand:DI 1 "register_operand" "r")))
   (clobber (match_scratch:DF 2 "=&r"))
   (clobber (match_scratch:DF 3 "=&r"))]
  ""
  "cvt.d.l\\t%2,%1\\n\\
\\tbrge.l\\t%1,0,24\\n\\
\\tlea.sl\\t%3,0x43f00000\\n\\
\\tfadd.d\\t%2,%2,%3\\n\\
\\tcvt.s.d\\t%0,%2"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "4") ])

(define_insn "floatunsdidf2"
  [(set (match_operand:DF 0 "register_operand" "=&r")
        (unsigned_float:DF (match_operand:DI 1 "register_operand" "r")))
   (clobber (match_scratch:DF 2 "=&r"))]
  ""
  "cvt.d.l\\t%0,%1\\n\\
\\tbrge.l\\t%1,0,24\\n\\
\\tlea.sl\\t%2,0x43f00000\\n\\
\\tfadd.d\\t%0,%0,%2"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "3") ])

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (fix:SI (match_operand:SF 1 "register_operand" "r")))]
  ""
  "cvt.w.s.sx.rz\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "1")])

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (fix:DI (match_operand:SF 1 "register_operand" "r")))
   (clobber (match_scratch:DF 2 "=&r"))]
  ""
  "cvt.d.s\\t%2,%1\\n\\tcvt.l.d.rz\\t%0,%2"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "2")])


(define_insn "*fix_truncsfsi2_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI
        (fix:SI (match_operand:SF 1 "register_operand" "r"))))]
  ""
  "cvt.w.s.<ex>.rz\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "1")])

(define_insn "fix_truncdf<mode>2"
  [(set (match_operand:MODEI 0 "register_operand" "=r")
        (fix:MODEI (match_operand:DF 1 "register_operand" "r")))]
  ""
  "cvt.<suffix>.d<ext>.rz\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

(define_insn "*fix_truncdfsi2_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI
        (fix:SI (match_operand:DF 1 "register_operand" "r"))))]
  ""
  "cvt.w.d.<ex>.rz\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "1")])

; To avoid rounding, we cut more than 52 bit fraction of quadruple 
(define_insn "fix_trunctfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (fix:SI (match_operand:TF 1 "register_operand" "r")))
   (clobber (match_scratch:TF 2 "=&r"))]
  ""
  "or\\t%2,0,%1\\n\\
\\tand\\t%Q2,%Q1,(4)1\\n\\
\\tcvt.d.q\\t%2,%2\\n\\
\\tcvt.w.d.sx.rz\\t%0,%2"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "4")])

(define_insn "*fix_trunctfsi2_any_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI
        (fix:SI (match_operand:TF 1 "register_operand" "r"))))
   (clobber (match_scratch:TF 2 "=&r"))]
  ""
  "or\\t%2,0,%1\\n\\
\\tand\\t%Q2,%Q1,(4)1\\n\\
\\tcvt.d.q\\t%2,%2\\n\\
\\tcvt.w.d.<ex>.rz\\t%0,%2"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "4")])

; To avoid rounding, we cut more than 52 bit fraction of quadruple 
(define_insn "fix_trunctfdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (fix:DI (match_operand:TF 1 "register_operand" "r")))
   (clobber (match_scratch:TF 2 "=&r"))
   (clobber (match_scratch:DF 3 "=&r"))
   (clobber (match_scratch:DI 4 "=&r"))]
  ""
  "or\\t%2,0,%1\\n\\
\\tand\\t%Q2,%Q1,(4)1\\n\\
\\tcvt.d.q\\t%3,%2\\n\\
\\tcvt.l.d.rz\\t%4,%3\\n\\
\\tcvt.d.l\\t%3,%4\\n\\
\\tcvt.q.d\\t%2,%3\\n\\
\\tfsub.q\\t%2,%1,%2\\n\\
\\tand\\t%Q2,%Q2,(4)1\\n\\
\\tcvt.d.q\\t%3,%2\\n\\
\\tcvt.l.d.rz\\t%3,%3\\n\\
\\tadds.l\\t%0,%3,%4"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "11")])

(define_insn "fixuns_truncsfqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (unsigned_fix:QI (match_operand:SF 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "cvt.w.s.zx.rz\\t%2,%1\\n\\
\\tand\\t%0,%2,(56)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "2")])

(define_insn "fixuns_truncsfqi2_zero_extendsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI
        (unsigned_fix:QI (match_operand:SF 1 "register_operand" "r"))))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "cvt.w.s.zx.rz\\t%2,%1\\n\\
\\tand\\t%0,%2,(56)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "2")])

(define_insn "fixuns_truncdfqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (unsigned_fix:QI (match_operand:DF 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "cvt.w.d.zx.rz\\t%2,%1\\n\\
\\tand\\t%0,%2,(56)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "2")])

(define_insn "fixuns_truncdfqi2_zero_extendsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI
        (unsigned_fix:QI (match_operand:DF 1 "register_operand" "r"))))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "cvt.w.d.zx.rz\\t%2,%1\\n\\
\\tand\\t%0,%2,(56)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "2")])

(define_insn "fixuns_trunctfqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (unsigned_fix:QI (match_operand:TF 1 "register_operand" "r")))
   (clobber (match_scratch:TF 2 "=&r"))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "or\\t%2,0,%1\\n\\
\\tand\\t%Q2,%Q1,(4)1\\n\\
\\tcvt.d.q\\t%2,%2\\n\\
\\tcvt.w.d.zx.rz\\t%3,%2\\n\\
\\tand\\t%0,%3,(56)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "5")])

(define_insn "fixuns_trunctfqi2_zero_extendsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI
        (unsigned_fix:QI (match_operand:TF 1 "register_operand" "r"))))
   (clobber (match_scratch:TF 2 "=&r"))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "or\\t%2,0,%1\\n\\
\\tand\\t%Q2,%Q1,(4)1\\n\\
\\tcvt.d.q\\t%2,%2\\n\\
\\tcvt.w.d.zx.rz\\t%3,%2\\n\\
\\tand\\t%0,%3,(56)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "5")])

(define_insn "fixuns_truncsfhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unsigned_fix:HI (match_operand:SF 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "cvt.w.s.zx.rz\\t%2,%1\\n\\
\\tand\\t%0,%2,(48)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "3")])

(define_insn "fixuns_truncsfhi2_zero_extendsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI
        (unsigned_fix:HI (match_operand:SF 1 "register_operand" "r"))))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "cvt.w.s.zx.rz\\t%2,%1\\n\\
\\tand\\t%0,%2,(48)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "3")])

(define_insn "fixuns_truncdfhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unsigned_fix:HI (match_operand:DF 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "cvt.w.d.zx.rz\\t%2,%1\\n\\
\\tand\\t%0,%2,(48)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "2")])

(define_insn "fixuns_truncdfhi2_zero_extendsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI
        (unsigned_fix:HI (match_operand:DF 1 "register_operand" "r"))))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "cvt.w.d.zx.rz\\t%2,%1\\n\\
\\tand\\t%0,%2,(48)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")
   (set_attr "length" "2")])

(define_insn "fixuns_trunctfhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (unsigned_fix:HI (match_operand:TF 1 "register_operand" "r")))
   (clobber (match_scratch:TF 2 "=&r"))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "or\\t%2,0,%1\\n\\
\\tand\\t%Q2,%Q1,(4)1\\n\\
\\tcvt.d.q\\t%2,%2\\n\\
\\tcvt.w.d.zx.rz\\t%3,%2\\n\\
\\tand\\t%0,%3,(48)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "5")])

(define_insn "fixuns_trunctfhi2_zero_extendsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI
        (unsigned_fix:HI (match_operand:TF 1 "register_operand" "r"))))
   (clobber (match_scratch:TF 2 "=&r"))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "or\\t%2,0,%1\\n\\
\\tand\\t%Q2,%Q1,(4)1\\n\\
\\tcvt.d.q\\t%2,%2\\n\\
\\tcvt.w.d.zx.rz\\t%3,%2\\n\\
\\tand\\t%0,%3,(48)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "5")])

(define_insn "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unsigned_fix:SI (match_operand:SF 1 "register_operand" "r")))
   (clobber (match_scratch:DF 2 "=&r"))
   (clobber (match_scratch:DI 3 "=&r"))]
  ""
  "cvt.d.s\\t%2,%1\\n\\
\\tcvt.l.d.rz\\t%3,%2\\n\\
\\tand\\t%0,%3,(32)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "3")])

(define_insn "fixuns_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unsigned_fix:SI (match_operand:DF 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "cvt.l.d.rz\\t%2,%1\\n\\
\\tand\\t%0,%2,(32)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")
   (set_attr "length" "2")])

(define_insn "fixuns_trunctfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unsigned_fix:SI (match_operand:TF 1 "register_operand" "r")))
   (clobber (match_scratch:TF 2 "=&r"))
   (clobber (match_scratch:DI 3 "=&r"))]
  ""
  "or\\t%2,0,%1\\n\\
\\tand\\t%Q2,%Q1,(4)1\\n\\
\\tcvt.d.q\\t%2,%2\\n\\
\\tcvt.l.d.rz\\t%3,%2\\n\\
\\tand\\t%0,%3,(32)0"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length" "5")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (float_truncate:SF (match_operand:DF 1 "register_operand" "r")))]
  ""
  "cvt.s.d\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "SF")])

(define_insn "trunctf<mode>2"
  [(set (match_operand:MODEF 0 "register_operand" "=r")
        (float_truncate:MODEF (match_operand:TF 1 "register_operand" "r")))]
  ""
  "cvt.<suffix>.q\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "<MODE>")])

;;
;; Assume QI, HI, and SI hold lower 64bit of S register
;; In case of sign extension of QI, HI, and SI 64bit is filled.
;;

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (truncate:SI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "adds.w.sx\\t%0,0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

(define_insn "*truncdisi2_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (sign_extend:DI
         (truncate:SI (match_operand:DI 1 "register_operand" "r"))))]
  ""
  "adds.w.sx\\t%0,0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

(define_insn "*truncdisi2_zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI
         (truncate:SI (match_operand:DI 1 "register_operand" "r"))))]
  ""
  "and\\t%0,%1,(32)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

(define_insn "trunc<mode>hi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (truncate:HI (match_operand:MODEOVHI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%1,48\\n\\tsra.l\\t%0,%2,48"
  [(set_attr "type" "alu")
   (set_attr "mode" "HI")
   (set_attr "length" "2")])

(define_insn "*trunc<mode>hi2_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI
         (truncate:HI (match_operand:MODEOVHI 1 "register_operand" "r"))))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%1,48\\n\\tsra.l\\t%0,%2,48"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "2")])

(define_insn "*trunc<mode>hi2_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (sign_extend:DI
         (truncate:HI (match_operand:MODEOVHI 1 "register_operand" "r"))))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%1,48\\n\\tsra.l\\t%0,%2,48"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "2")])

(define_insn "*trunc<mode>hi2_zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI
         (truncate:HI (match_operand:MODEOVHI 1 "register_operand" "r"))))]
  ""
  "and\\t%0,%1,(48)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1")])

(define_insn "*trunc<mode>hi2_zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI
         (truncate:HI (match_operand:MODEOVHI 1 "register_operand" "r"))))]
  ""
  "and\\t%0,%1,(48)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])

(define_insn "trunc<mode>qi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (truncate:QI (match_operand:MODEOVQI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%1,56\\n\\tsra.l\\t%0,%2,56"
  [(set_attr "type" "alu")
   (set_attr "mode" "QI")
   (set_attr "length" "2")])

(define_insn "trunc<mode>qi2_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (sign_extend:HI
         (truncate:QI (match_operand:MODEOVQI 1 "register_operand" "r"))))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%1,56\\n\\tsra.l\\t%0,%2,56"
  [(set_attr "type" "alu")
   (set_attr "mode" "HI")
   (set_attr "length" "2")])

(define_insn "trunc<mode>qi2_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI
         (truncate:QI (match_operand:MODEOVQI 1 "register_operand" "r"))))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%1,56\\n\\tsra.l\\t%0,%2,56"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "2")])

(define_insn "trunc<mode>qi2_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (sign_extend:DI
         (truncate:QI (match_operand:MODEOVQI 1 "register_operand" "r"))))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%1,56\\n\\tsra.l\\t%0,%2,56"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "2")])

(define_insn "trunc<mode>qi2_zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (zero_extend:HI
         (truncate:QI (match_operand:MODEOVQI 1 "register_operand" "r"))))]
  ""
  "and\\t%0,%1,(56)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "HI")
   (set_attr "length" "1")])

(define_insn "trunc<mode>qi2_zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI
         (truncate:QI (match_operand:MODEOVQI 1 "register_operand" "r"))))]
  ""
  "and\\t%0,%1,(56)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")
   (set_attr "length" "1")])

(define_insn "trunc<mode>qi2_zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI
         (truncate:QI (match_operand:MODEOVQI 1 "register_operand" "r"))))]
  ""
  "and\\t%0,%1,(56)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")
   (set_attr "length" "1")])


(define_insn "trunctidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (truncate:DI (match_operand:TI 1 "register_operand" "r")))]
  ""
  "or\\t%0,0,%Q1"
  [(set_attr "type" "alu")
   (set_attr "mode" "DI")])

(define_insn "*settrunctidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (subreg:DI (match_operand:TI 1 "register_operand" "r")
                   0 ))]
  ""
  "or\\t%0,0,%Q1"
  [(set_attr "type" "move")
   (set_attr "mode" "DI")])

(define_insn "trunctisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (truncate:SI (match_operand:TI 1 "register_operand" "r")))]
  ""
  "adds.w.sx\\t%0,0,%Q1"
  [(set_attr "type" "alu")
   (set_attr "mode" "SI")])

(define_insn "trunctihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (truncate:HI (match_operand:TI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%Q1,48\\n\\tsra.l\\t%0,%2,48"
  [(set_attr "type" "alu")
   (set_attr "mode" "HI")
   (set_attr "length" "2")])

(define_insn "trunctiqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (truncate:QI (match_operand:TI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%Q1,56\\n\\tsra.l\\t%0,%2,56"
  [(set_attr "type" "alu")
   (set_attr "mode" "QI")
   (set_attr "length" "2")])

(define_insn "zero_extendqi<mode>2"
  [(set (match_operand:MODEOVQI 0 "register_operand" "=r,r")
        (zero_extend:MODEOVQI (match_operand:QI 1 "general_operand" "r,a")))]
  ""
  "*
{
 int pos=1;
 rtx xop[2];
 switch(which_alternative) 
 {
     default:
     case 0:
        return \"and\\t%0,%1,(56)0\";
     case 1:
        ve_asm_output_ldst_unified(\"ld1b.zx\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
          xop[0] = ve_addr_cut_mem(operands[1],pos);
          if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld1b.zx\\t%0,%1\";
  }
}"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1,1")])

(define_insn "zero_extendhi<mode>2"
  [(set (match_operand:MODEOVHI 0 "register_operand" "=r,r")
        (zero_extend:MODEOVHI (match_operand:HI 1 "general_operand" "r,a")))]
  ""
  "*
{
 int pos=1;
 rtx xop[2];
 switch(which_alternative) 
 {
     default:
     case 0:
        return \"and\\t%0,%1,(48)0\";
     case 1:
        ve_asm_output_ldst_unified(\"ld2b.zx\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
          xop[0] = ve_addr_cut_mem(operands[1],pos);
          if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld2b.zx\\t%0,%1\";
  }
}"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1,1")])

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (zero_extend:DI (match_operand:SI 1 "general_operand" "r,a")))]
  ""
  "*
{
 int pos=1;
 rtx xop[2];
 switch(which_alternative) 
 {
     default:
     case 0:
        return \"and\\t%0,%1,(32)0\";
     case 1:
        ve_asm_output_ldst_unified(\"ldl.zx\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
          xop[0] = ve_addr_cut_mem(operands[1],pos);
          if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ldl.zx\\t%0,%1\";
  }
}"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1")])

(define_insn "zero_extendqiti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (zero_extend:TI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "or\\t%0,0,(0)1\\n\\tand\\t%Q0,%1,(56)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "2")])

(define_insn "zero_extendhiti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (zero_extend:TI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "or\\t%0,0,(0)1\\n\\tand\\t%Q0,%1,(48)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "2")])

(define_insn "zero_extendsiti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (zero_extend:TI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "or\\t%0,0,(0)1\\n\\tand\\t%Q0,%1,(32)0"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "2")])

(define_insn "zero_extendditi2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (zero_extend:TI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "or\\t%0,0,(0)1\\n\\tor\\t%Q0,0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "2")])

(define_insn "extendqi<mode>2"
  [(set (match_operand:MODEOVQI 0 "register_operand" "=r,r")
        (sign_extend:MODEOVQI (match_operand:QI 1 "general_operand" "r,a")))
   (clobber (match_scratch:DI 2 "=&r,X"))]
  ""
  "*
{
 int pos=1;
 rtx xop[2];
 switch(which_alternative) 
 {
     default:
     case 0:
        if (!TARGET_OPT_SIGN_EXTENSION)
          return \"sll\\t%2,%1,56\\n\\tsra.l\\t%0,%2,56\";
        else 
          return \"or\\t%0,0,%1\";
     case 1:
        ve_asm_output_ldst_unified(\"ld1b.sx\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1)
        {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld1b.sx\\t%0,%1\";
  }
}"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1,1")])

(define_insn "extendhi<mode>2"
  [(set (match_operand:MODEOVHI 0 "register_operand" "=r,r")
        (sign_extend:MODEOVHI (match_operand:HI 1 "general_operand" "r,a")))
   (clobber (match_scratch:DI 2 "=&r,X"))]
  ""
  "*
{
 int pos=1;
 rtx xop[2];
 switch(which_alternative) 
 {
     default:
     case 0:
        if (!TARGET_OPT_SIGN_EXTENSION)
          return \"sll\\t%2,%1,48\\n\\tsra.l\\t%0,%2,48\";
        else 
          return \"or\\t%0,0,%1\";
     case 1:
        ve_asm_output_ldst_unified(\"ld2b.sx\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld2b.sx\\t%0,%1\";
  }
}"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1,1")])

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (sign_extend:DI (match_operand:SI 1 "general_operand" "r,a")))]
  ""
  "*
{
 int pos=1;
 rtx xop[2];
 switch(which_alternative) 
 {
     default:
     case 0:
        if (!TARGET_OPT_SIGN_EXTENSION)
          return \"adds.w.sx\\t%0,0,%1\";
        else 
          return \"or\\t%0,0,%1\";
     case 1:
        ve_asm_output_ldst_unified(\"ldl.sx\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
          if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ldl.sx\\t%0,%1\";
  }
}"
  [(set_attr "type" "alu,load")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1")])

(define_insn "extendqiti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (sign_extend:TI (match_operand:QI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%1,56\\n\\tsra.l\\t%Q0,%2,56\\n\\tsra.l\\t%0,%Q0,63"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "3")])

(define_insn "extendhiti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (sign_extend:TI (match_operand:HI 1 "register_operand" "r")))
   (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "sll\\t%2,%1,48\\n\\tsra.l\\t%Q0,%2,48\\n\\tsra.l\\t%0,%Q0,63"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "3")])

(define_insn "extendsiti2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (sign_extend:TI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "adds.w.sx\\t%Q0,0,%1\\n\\tsra.l\\t%0,%Q0,63"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "2")])

(define_insn "extendditi2"
  [(set (match_operand:TI 0 "register_operand" "=&r")
        (sign_extend:TI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "sra.l\\t%0,%1,63\\n\\tor\\t%Q0,0,%1"
  [(set_attr "type" "alu")
   (set_attr "mode" "TI")
   (set_attr "length" "2")])

;;
;
(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (float_extend:DF (match_operand:SF 1 "register_operand" "r")))]
  ""
  "cvt.d.s\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "DF")])

(define_insn "extend<mode>tf2"
  [(set (match_operand:TF 0 "register_operand" "=r")
        (float_extend:TF (match_operand:MODEF 1 "register_operand" "r")))]
  ""
  "cvt.q.<suffix>\\t%0,%1"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")])
;
; pic code
;

(define_insn "*movdi_label_relative"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (unspec:DI
            [(match_operand:DI 1 "symbolic_operand" "")] 
               UNSPEC_MOVE_PIC_LABEL))]
  ""
  "sic\\t%0\\n\\
\\tlea\\t%0,%1-.(,%0)"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "2")])

(define_insn "ve_move_pic_got32"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI 
            [(match_operand:DI 1 "symbolic_operand" "")]
               UNSPEC_MOVE_PIC_GOT32))]
  ""
  "ld\\t%0,%S1@GOT32(,%%got)"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "1")])

(define_insn "ve_move_pic_pc"
 [(set (match_operand:DI 0 "register_operand" "=&r")
       (unspec:DI
           [(match_operand:DI 1 "symbolic_operand" "")] 
              UNSPEC_MOVE_PIC_PC))
       (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "addi\\t%0,-16,%S1@PC_LO\\n\\
\\tsic\\t%2\\n\\
\\tlea.sl\\t%0,%S1@PC_HI(%0,%2)"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "3")])

(define_insn "ve_move_pic_pc2"
 [(set (match_operand:DI 0 "register_operand" "=&r")
       (unspec:DI
           [(match_operand:DI 1 "symbolic_operand" "") 
            (match_operand:DI 2 "register_operand" "=&r")]
              UNSPEC_MOVE_PIC_PC2))]
  ""
  "addi\\t%0,-16,%S1@PC_LO\\n\\
\\tsic\\t%2\\n\\
\\tlea.sl\\t%0,%S1@PC_HI(%0,%2)"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "3")])

(define_insn "ve_move_pic_plt"
 [(set (match_operand:DI 0 "register_operand" "=&r")
       (unspec:DI
           [(match_operand:DI 1 "symbolic_operand" "")] 
              UNSPEC_MOVE_PIC_PLT))
       (clobber (match_scratch:DI 2 "=&r"))]
  ""
  "addi\\t%0,-16,%S1@PLT_LO\\n\\
\\tsic\\t%2\\n\\
\\tlea.sl\\t%0,%S1@PLT_HI(%0,%2)"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "3")])

(define_insn "ve_move_pic_got"
 [(set (match_operand:DI 0 "register_operand" "=&r")
       (unspec:DI
           [(match_operand:DI 1 "symbolic_operand" "")] 
              UNSPEC_MOVE_PIC_GOT))]
  ""
  "addi\\t%0,0,%S1@GOT_LO\\n\\
\\tlea.sl\\t%0,%S1@GOT_HI(%0,%%got)\\n\\
\\tld\\t%0,0(,%0)"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "3")])

(define_insn "ve_move_pic_gotoff"
 [(set (match_operand:DI 0 "register_operand" "=&r")
        (unspec:DI
            [(match_operand:DI 1 "symbolic_operand" "")] 
               UNSPEC_MOVE_PIC_GOTOFF))]
  ""
  "addi\\t%0,0,%S1@GOTOFF_LO\\n\\
\\tlea.sl\\t%0,%S1@GOTOFF_HI(%0,%%got)"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "2")])



(define_insn "ve_move"
 [(set (match_operand:DI 0 "register_operand" "=&r")
       (unspec:DI
           [(match_operand:DI 1 "symbolic_operand" "")] 
              UNSPEC_MOVE))]
  ""
  "addi\\t%0,0,%S1@LO\\n\\
\\tlea.sl\\t%0,%S1@HI(,%0)"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "2")])

(define_insn "ve_move_call"
 [(set (match_operand:DI 0 "register_operand" "=&r")
       (unspec:DI
           [(match_operand:DI 1 "symbolic_operand" "")]
              UNSPEC_MOVE_CALL))]
  ""
  "addi\\t%0,0,%S1@CALL_LO\\n\\
\\tlea.sl\\t%0,%S1@CALL_HI(,%0)"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "2")])

;; TLS Support

(define_insn "tgd_load_call"
 [(set (match_operand:DI 0 "register_operand" "=r")
       (unspec:DI
           [(match_operand:DI 1 "tgd_symbolic_operand" "") 
           (match_operand:DI 2 "symbolic_operand" "")] 
              UNSPEC_TLSGD))
       (use (reg:DI 0))
       (use (reg:DI 15))
       (use (reg:DI 16))
       (clobber (reg:DI 1))
       (clobber (reg:DI 2))
       (clobber (reg:DI 3))
       (clobber (reg:DI 4))
       (clobber (reg:DI 5))
       (clobber (reg:DI 6))
       (clobber (reg:DI 7))
       (clobber (reg:DI 10))
       (clobber (reg:DI 12))
       (clobber (reg:DI 13))
       (clobber (reg:DI 34))
       (clobber (reg:DI 35))
       (clobber (reg:DI 36))
       (clobber (reg:DI 37))
       (clobber (reg:DI 38))
       (clobber (reg:DI 39))
       (clobber (reg:DI 40))
       (clobber (reg:DI 41))
       (clobber (reg:DI 42))
       (clobber (reg:DI 43))
       (clobber (reg:DI 44))
       (clobber (reg:DI 45))
       (clobber (reg:DI 46))
       (clobber (reg:DI 47))
       (clobber (reg:DI 48))
       (clobber (reg:DI 49))
       (clobber (reg:DI 50))
       (clobber (reg:DI 51))
       (clobber (reg:DI 52))
       (clobber (reg:DI 53))
       (clobber (reg:DI 54))
       (clobber (reg:DI 55))
       (clobber (reg:DI 56))
       (clobber (reg:DI 57))
       (clobber (reg:DI 58))
       (clobber (reg:DI 59))
       (clobber (reg:DI 60))
       (clobber (reg:DI 61))
       (clobber (reg:DI 62))
       (clobber (reg:DI 63))]
  ""
  "*
{
 rtx xop[7];
 xop[0] = operands[0];
 xop[1] = operands[1];
 xop[2] = operands[2];
 xop[3] = gen_rtx_REG(DImode,0);
 xop[4] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
 xop[5] = gen_rtx_REG(DImode,VE_RETURN_REGNUM);
 output_asm_insn(\"addi\\t%3,-16,%S1@TLS_GD_LO\",xop);
 output_asm_insn(\"sic\\t%5\",xop);
 output_asm_insn(\"lea.sl\\t%3,%S1@TLS_GD_HI(%3,%5)\",xop);
 output_asm_insn(\"addi\\t%4,8,%S2@PLT_LO\",xop);
 output_asm_insn(\"lea.sl\\t%4,%S2@PLT_HI(%4,%5)\",xop);
 output_asm_insn(\"bsic\\t%5,(,%4)\",xop);
 if (REGNO(operands[0]) != 0)
    output_asm_insn(\"or\\t%0,0,%3\",xop);
 return \"\";
}"
  [(set_attr "type" "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "7")])


(define_insn "tld_load_call"
 [(set (match_operand:DI 0 "register_operand" "=r")
       (unspec:DI
           [(match_operand:DI 1 "tld_symbolic_operand" "") 
           (match_operand:DI 2 "symbolic_operand" "")] 
              UNSPEC_TLSLD))
       (use (reg:DI 0))
       (use (reg:DI 15))
       (use (reg:DI 16))
       (clobber (reg:DI 1))
       (clobber (reg:DI 2))
       (clobber (reg:DI 3))
       (clobber (reg:DI 4))
       (clobber (reg:DI 5))
       (clobber (reg:DI 6))
       (clobber (reg:DI 7))
       (clobber (reg:DI 10))
       (clobber (reg:DI 12))
       (clobber (reg:DI 13))
       (clobber (reg:DI 34))
       (clobber (reg:DI 35))
       (clobber (reg:DI 36))
       (clobber (reg:DI 37))
       (clobber (reg:DI 38))
       (clobber (reg:DI 39))
       (clobber (reg:DI 40))
       (clobber (reg:DI 41))
       (clobber (reg:DI 42))
       (clobber (reg:DI 43))
       (clobber (reg:DI 44))
       (clobber (reg:DI 45))
       (clobber (reg:DI 46))
       (clobber (reg:DI 47))
       (clobber (reg:DI 48))
       (clobber (reg:DI 49))
       (clobber (reg:DI 50))
       (clobber (reg:DI 51))
       (clobber (reg:DI 52))
       (clobber (reg:DI 53))
       (clobber (reg:DI 54))
       (clobber (reg:DI 55))
       (clobber (reg:DI 56))
       (clobber (reg:DI 57))
       (clobber (reg:DI 58))
       (clobber (reg:DI 59))
       (clobber (reg:DI 60))
       (clobber (reg:DI 61))
       (clobber (reg:DI 62))
       (clobber (reg:DI 63))]
  ""
  "*
{
 rtx xop[7];
 xop[0] = operands[0];
 xop[1] = operands[1];
 xop[2] = operands[2];
 xop[3] = gen_rtx_REG(DImode,0);
 xop[4] = gen_rtx_REG(DImode,VE_GOT_REGNUM);
 xop[5] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
 xop[6] = gen_rtx_REG(DImode,VE_RETURN_REGNUM);
 output_asm_insn(\"addi\\t%3,0,%S1@TLS_LD_LO\",xop);
 output_asm_insn(\"lea.sl\\t%3,%S1@TLS_LD_HI(%4,%3)\",xop);
 output_asm_insn(\"addi\\t%5,-16,%S2@PLT_LO\",xop);
 output_asm_insn(\"sic\\t%6\",xop);
 output_asm_insn(\"lea.sl\\t%5,%S2@PLT_HI(%5,%6)\",xop);
 output_asm_insn(\"bsic\\t%6,(,%5)\",xop);
 if (REGNO(operands[0]) != 0)
    output_asm_insn(\"or\\t%0,0,%3\",xop);
 return \"\";
}"
  [(set_attr "type" "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "6")])

(define_insn "tld_offset_load"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI (unspec:DI [(match_operand 1 "tld_symbolic_operand" "")]
                            UNSPEC_TLSLDO)
                 (match_operand:DI 2 "register_operand" "r")))]
  ""
  "lea\\t%0,%S1@DTPOFF(,%2)"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "tie_load"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (unspec:DI [(match_operand 1 "tie_symbolic_operand" "")] 
                UNSPEC_TLSIE))
   (use (reg:DI 15))]
  ""
  "addi\\t%0,0,%S1@TLS_IE_LO\\n\\
\\tlea.sl\\t%0,%S1@TLS_IE_HI(,%0)\\n\\
\\tld\\t%0,0(%%got,%0)"
  [(set_attr "type" "move")
   (set_attr "length" "2")])

(define_insn "tle_load"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (plus:DI (unspec:DI [(match_operand 1 "tle_symbolic_operand" "")]
                            UNSPEC_TLSLE)
                 (match_operand:DI 2 "register_operand" "r")))]
  ""
  "addi\\t%0,0,%S1@TPOFF_LO\\n\\
\\tlea.sl\\t%0,%S1@TPOFF_HI(%2,%0)"
  [(set_attr "type" "move")
   (set_attr "length" "2")])


;;
;; move instructions:
;; mem-reg, reg-mem, imm-reg
;;
;; The standard "movsi" pattern for RTL generation. It
;; makes sure one of the operands is in a register, but 
;; avoids trying to do this later during compilation when
;; the register allocation is complete.
;;
;; This pattern was lifted almost verbatim from the MIPS machine
;; description.
;;




;;  ve_expand_move(operands,DImode);
;;

;; reload may generate constant > 32bit (case 3)!

(define_expand "mov<mode>"
  [(set (match_operand:MODEANYI 0 "nonimmediate_operand" "")
        (match_operand:MODEANYI 1 "general_operand" ""))]
  ""
  "
{
  if (ve_expand_move(operands,<MODE>mode)) {
    DONE;
  }
}")


(define_insn "movti_general"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=r,r,r,&r,r,a,r")
        (match_operand:TI 1 "general_operand" "I,M,N,i,a,r,r"))]
  ""
  "*

{ 
  int pos=1;
  rtx xop[5];
  switch(which_alternative)
   {
     default:
     case 0:
       return \"or\\t%0,%Z1,(0)1\\n\\tor\\t%Q0,%G1,(0)1\";
     case 1:
       return \"or\\t%0,%Z1,(0)1\\n\\tor\\t%Q0,0,%M1\";
     case 2:
       return \"or\\t%0,%Z1,(0)1\\n\\tlea\\t%Q0,%1\";
     case 3:
       if(GET_CODE(operands[1]) == CONST_INT) {
        if ((INTVAL(operands[1]) & 0xffffffff) == 0) 
        {
         return \"or\\t%0,%Z1,(0)1\\n\\tlea.sl\\t%Q0,%H1\";
        }
        else
        {
         xop[0] = operands[0];
         xop[1] = operands[1];
         xop[2] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
         output_asm_insn(
          \"or\\t%0,%Z1,(0)1\\n\\tlea\\t%2,%L1\\n\\tlea.sl\\t%Q0,%H1(%2)\",xop);
         return \"\";  
        }
       }
       else if (GET_CODE(operands[1]) == CONST_DOUBLE)
       {
         xop[0] = operands[0];
         xop[1] = GEN_INT(CONST_DOUBLE_HIGH(operands[1]));
         xop[2] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
         if (-2147483648 <= INTVAL(xop[1]) && INTVAL(xop[1]) <= 2147483647)
            output_asm_insn(\"lea\\t%0,%1\",xop);
         else 
            output_asm_insn(\"lea\\t%2,%L1\\n\\tlea.sl\\t%0,%H1(%2)\",xop);
         xop[1] = GEN_INT(CONST_DOUBLE_LOW(operands[1]));
         if (-2147483648 <= INTVAL(xop[1]) && INTVAL(xop[1]) <= 2147483647)
            output_asm_insn(\"lea\\t%Q0,%1\",xop);
         else
            output_asm_insn(\"lea\\t%2,%L1\\n\\tlea.sl\\t%Q0,%H1(%2)\",xop);
         return \"\";  
       }
       gcc_unreachable();
     case 4:
        ve_asm_output_ldst_unified(\"ld\",VE_DIR_QLOAD,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld\\t%0,%1\";
     case 5:
        ve_asm_output_ldst_unified(\"st\",VE_DIR_QSTORE,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[0],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"st\\t%1,%0\";
     case 6:
        return \"or\\t%0,0,%1\\n\\tor\\t%Q0,0,%Q1\";
   }

}"
  [(set_attr "type" "move,move,move,move,load,store,move")
   (set_attr "mode" "TI")
   (set_attr "length" "2,2,2,3,2,2,2")])


(define_insn "movdi_general"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,r,r,a,r")
        (match_operand:DI 1 "general_operand" "I,M,N,i,a,r,r"))]
  ""
  "*

{ 
  int pos=1;
  rtx xop[5];
  switch(which_alternative)
   {
     default:
     case 0:
       return \"or\\t%0,%G1,(0)1\";
     case 1:
       return \"or\\t%0,0,%M1\";
     case 2:
       return \"lea\\t%0,%1\";
     case 3:
       if(GET_CODE(operands[1]) == CONST_INT) {
         if ((INTVAL(operands[1]) & 0xffffffff) == 0)
           return \"lea.sl\\t%0,%H1\";
         else {
           xop[0] = operands[0];
           xop[1] = operands[1];
           xop[2] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
           output_asm_insn(\"lea\\t%2,%L1\\n\\tlea.sl\\t%0,%H1(%2)\",xop);
           return \"\";  
          }
      }
      gcc_unreachable();
     case 4:
        ve_asm_output_ldst_unified(\"ld\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld\\t%0,%1\";
     case 5:
        ve_asm_output_ldst_unified(\"st\",VE_DIR_STORE,operands,NULL_RTX);
        return \"\";
	xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[0],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"st\\t%1,%0\";
     case 6:
        return \"or\\t%0,0,%1\";
   }

}"
  [(set_attr "type" "move,move,move,move,load,store,move")
   (set_attr "mode" "DI")
   (set_attr "length" "1,1,1,2,1,1,1")])


(define_insn "movsi_general"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,a,r")
        (match_operand:SI 1 "general_operand" "I,P,N,a,r,r"))]
  ""
  "*

{ 
  int pos=1;
  rtx xop[2];
  switch(which_alternative)
   {
     default:
     case 0:
       return \"or\\t%0,%G1,(0)1\";
     case 1:
       return \"or\\t%0,0,%P1\";
     case 2:
       return \"lea\\t%0,%1\";
     case 3:
        ve_asm_output_ldst_unified(\"ldl.sx\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ldl.sx\\t%0,%1\";
     case 4:
        ve_asm_output_ldst_unified(\"stl\",VE_DIR_STORE,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[0],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"stl\\t%1,%0\";
     case 5:
        return \"or\\t%0,0,%1\";
   }

}"
  [(set_attr "type" "move,move,move,load,store,move")
   (set_attr "mode" "SI")
   (set_attr "length" "1,1,1,1,1,1")])


;;
;; Move half words.
;;
(define_insn "movhi_general"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,a,r")
        (match_operand:HI 1 "general_operand"  "I,P,N,a,r,r"))]
  ""
  "*
{ 
  int pos=1;
  rtx xop[2];
  switch(which_alternative)
   {
     default:
     case 0:
       return \"or\\t%0,%G1,(0)1\";
     case 1:
       return \"or\\t%0,0,%P1\";
     case 2:
       return \"lea\\t%0,%1\";
     case 3:
        ve_asm_output_ldst_unified(\"ld2b.sx\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";

        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld2b.sx\\t%0,%1\";

     case 4:
        ve_asm_output_ldst_unified(\"st2b\",VE_DIR_STORE,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[0],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"st2b\\t%1,%0\";
     case 5:
        return \"or\\t%0,0,%1\";
   }

}"
  [(set_attr "type" "move,move,move,load,store,move")
   (set_attr "mode" "HI")
   (set_attr "length" "1,1,1,1,1,1")])

;;
;; move bytes.
;;

(define_insn "movqi_general"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,r,a,r")
        (match_operand:QI 1 "general_operand"  "I,P,N,a,r,r"))]
  ""
  "*
{ 
  int pos=1;
  rtx xop[2];
  switch(which_alternative)
   {
     default:
     case 0:
       return \"or\\t%0,%G1,(0)1\";
     case 1:
       return \"or\\t%0,0,%P1\";
     case 2:
       return \"lea\\t%0,%1\";
     case 3:
        ve_asm_output_ldst_unified(\"ld1b.sx\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld1b.sx\\t%0,%1\";
     case 4:
        ve_asm_output_ldst_unified(\"st1b\",VE_DIR_STORE,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[0],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"st1b\\t%1,%0\";
     case 5:
        return \"or\\t%0,0,%1\";
   }

}"
  [(set_attr "type" "move,move,move,load,store,move")
   (set_attr "mode" "QI")
   (set_attr "length" "1,1,1,1,1,1")])

;;
;; Move floats.
;;
(define_expand "mov<mode>"
  [(set (match_operand:MODEANYF 0 "nonimmediate_operand" "")
        (match_operand:MODEANYF 1 "general_operand" ""))]
  ""
  "
{
  if (ve_expand_move(operands,<MODE>mode)) {
    DONE;
  }
}")


(define_insn "movsf_general"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,a,r,r,r")
        (match_operand:SF 1 "general_operand"  "a,r,r,G,H"))]
  ""
  "*
{ 
  int pos=1;
  rtx xop[3];
  switch(which_alternative)
   {
     default:
     case 0:
        ve_asm_output_ldst_unified(\"ldu\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ldu\\t%0,%1\";
     case 1:
        ve_asm_output_ldst_unified(\"stu\",VE_DIR_STORE,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[0],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"stu\\t%1,%0\";
     case 2:
        return \"or\\t%0,0,%1\";
     case 3:
        return \"or\\t%0,0,(0)1\";
     case 4:
        return ve_movsf_reg_immediate(&operands[0]);
   }

}"
  [(set_attr "type" "load,store,move,move,move")
   (set_attr "mode" "SF")
   (set_attr "length" "1,1,1,1,1")])

(define_insn "movdf_general"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,a,r,r,r")
        (match_operand:DF 1 "general_operand"  "a,r,r,G,H"))]
  ""
  "*
{ 
  int pos=1;
  rtx xop[3];
  switch(which_alternative)
   {
     default:
     case 0:
        ve_asm_output_ldst_unified(\"ld\",VE_DIR_LOAD,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld\\t%0,%1\";
     case 1:
        ve_asm_output_ldst_unified(\"st\",VE_DIR_STORE,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[0],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"st\\t%1,%0\";
     case 2:
        return \"or\\t%0,0,%1\";
     case 3:
        return \"or\\t%0,0,(0)1\";
     case 4:
        return ve_movdf_reg_immediate(&operands[0]);
   }

}"
  [(set_attr "type" "load,store,move,move,move")
   (set_attr "mode" "DF")
   (set_attr "length" "1,1,1,1,2")])


(define_insn "movtf_general"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=r,a,r,r,r")
        (match_operand:TF 1 "general_operand" "a,r,r,G,H"))]
  ""
  "*
{ 
  int pos=1;
  rtx xop[3];
  switch(which_alternative)
   {
     default:
     case 0:
        ve_asm_output_ldst_unified(\"ld\",VE_DIR_QLOAD,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[1],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"ld\\t%Q0,%1\\n\\tld\\t%0,%A1\";
     case 1:
        ve_asm_output_ldst_unified(\"st\",VE_DIR_QSTORE,operands,NULL_RTX);
        return \"\";
        xop[1] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
        while(1) {
	  xop[0] = ve_addr_cut_mem(operands[0],pos);
	  if (xop[0] == NULL_RTX) break; 
          output_asm_insn(\"ld\\t%1,%0\",xop);
          pos++;
        }
        return \"st\\t%Q1,%0\\n\\tst\\t%1,%A0\";
     case 2:
        return \"or\\t%0,0,%1\\n\\tor\\t%Q0,0,%Q1\";
     case 3:
        return \"or\\t%0,0,(0)1\\n\\tor\\t%Q0,0,(0)1\";
     case 4:
        return ve_movtf_reg_immediate(&operands[0]);
   }

}"
  [(set_attr "type" "load,store,move,move,move")
   (set_attr "mode" "TF")
   (set_attr "length" "2,2,2,2,4")])


; Used for alloca 
(define_insn "allocate_stack"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec_volatile:DI [(match_operand:DI 1 "register_operand" "r")]
                             UNSPEC_STACK_PROBE))
   (clobber (match_scratch:DI 2 "=&r"))
   (clobber (match_scratch:DI 3 "=&r"))
   (clobber (reg:DI 11))] 
  ""
  "*
{
  rtx xop[10];
  xop[0] = operands[0];
  xop[1] = operands[1];
  xop[2] = operands[2];
  xop[3] = operands[3];
  xop[4] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
  xop[5] = stack_pointer_rtx;  
  xop[6] = gen_rtx_REG(DImode,STACK_LIMIT_REGNUM);  
  xop[7] = gen_rtx_CONST_INT(DImode,
                    VE_RSA_SIZE + crtl->outgoing_args_size);
  xop[8] = gen_rtx_REG(DImode,0);  
  xop[9] = gen_rtx_REG(DImode,VE_THREAD_POINTER_REGNUM);  
  output_asm_insn(\"subu.l\\t%5,%5,%1\",xop);
  output_asm_insn(\"brge.l.t\\t%5,%6,72\",xop);
  output_asm_insn(\"ld\\t%3,0x18(,%9)\",xop);
  output_asm_insn(\"or\\t%2,0,%8\",xop);
  output_asm_insn(\"lea\\t%4,315\",xop);
  output_asm_insn(\"shm.l\\t%4,0(%3)\",xop);
  output_asm_insn(\"shm.l\\t%6,8(%3)\",xop);
  output_asm_insn(\"shm.l\\t%5,16(%3)\",xop);
  output_asm_insn(\"monc\",xop);
  output_asm_insn(\"or\\t%8,0,%2\",xop);
  output_asm_insn(\"lea\\t%0,%7(,%5)\",xop); 
  return \"\";
}"
  [(set_attr "type" "load")
   (set_attr "mode" "none")
   (set_attr "length" "11")])

(define_insn "allocate_stack_prologue"
  [(set (match_operand:DI 0 "register_operand" "+&r")
        (unspec_volatile:DI [(match_operand:DI 1 "const_int_operand" "i")
                             (match_operand:DI 2 "register_operand" "=&r")
                             (match_operand:DI 3 "register_operand" "=&r")]
                             UNSPEC_STACK_PROLOGUE))]
  ""
  "*
{
  rtx xop[9];
  xop[0] = operands[0];
  xop[1] = operands[1];
  xop[2] = operands[2];
  xop[3] = operands[3];
  xop[4] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
  xop[5] = stack_pointer_rtx;  
  xop[6] = gen_rtx_REG(DImode,STACK_LIMIT_REGNUM);  
  xop[7] = gen_rtx_REG(DImode,0);  
  xop[8] = gen_rtx_REG(DImode,VE_THREAD_POINTER_REGNUM);  
  if (ve_const_si_p(INTVAL(xop[1])))
     output_asm_insn(\"lea\\t%0,%1(,%0)\",xop);
  else
   {
     output_asm_insn(\"lea.sl\\t%4,%H1(,%0)\",xop);
     output_asm_insn(\"lea\\t%0,%L1(,%4)\",xop);
   }
  output_asm_insn(\"brge.l.t\\t%0,%6,72\",xop);
  output_asm_insn(\"ld\\t%3,0x18(,%8)\",xop);
  output_asm_insn(\"or\\t%2,0,%7\",xop);
  output_asm_insn(\"lea\\t%4,315\",xop);
  output_asm_insn(\"shm.l\\t%4,0(%3)\",xop);
  output_asm_insn(\"shm.l\\t%6,8(%3)\",xop);
  output_asm_insn(\"shm.l\\t%0,16(%3)\",xop);
  output_asm_insn(\"monc\",xop);
  output_asm_insn(\"or\\t%7,0,%2\",xop);
  return \"\";
}"
  [(set_attr "type" "load")
   (set_attr "mode" "none")
   (set_attr "length" "10")])

; Stack protector instructions

(define_expand "stack_protect_set"
  [(set (match_operand 0 "memory_operand" "")
         (match_operand 1 "memory_operand" ""))]
  "!TARGET_MUSL_COMPAT"
{
  operands[1] = gen_rtx_MEM(Pmode,
                 gen_rtx_PLUS(Pmode, 
                              gen_rtx_REG(DImode,VE_THREAD_POINTER_REGNUM),
                              GEN_INT(TARGET_THREAD_SSP_OFFSET)));
  emit_insn (gen_stack_protect_setdi(operands[0], operands[1]));
 DONE;
})

(define_insn "stack_protect_setdi"
  [(set (match_operand:DI 0 "memory_operand" "=m")
        (unspec:DI [(match_operand:DI 1 "memory_operand" "m")] UNSPEC_SP_SET))
   (set (match_scratch:DI 2 "=&r") (const_int 0))]
  "!TARGET_MUSL_COMPAT"
  "ld\\t%2,%1\\n\\tst\\t%2,%0\\n\\tor\\t%2,0,(0)1"
  [(set_attr "type" "store")
   (set_attr "mode" "DI")
   (set_attr "length" "3")])

(define_expand "stack_protect_test"
  [(match_operand 0 "memory_operand" "")
   (match_operand 1 "memory_operand" "")
   (match_operand 2 "" "")]
  "!TARGET_MUSL_COMPAT"
{
  rtx result, test;

  operands[1] = gen_rtx_MEM(Pmode,
                 gen_rtx_PLUS(Pmode, 
                              gen_rtx_REG(DImode,VE_THREAD_POINTER_REGNUM),
                              GEN_INT(TARGET_THREAD_SSP_OFFSET)));
  result = gen_reg_rtx(DImode);
  emit_insn(gen_stack_protect_testdi(result,operands[0],operands[1]));
  test = gen_rtx_EQ(VOIDmode,result,const0_rtx);
  emit_jump_insn (gen_cbranchdi4(test,result,const0_rtx,operands[2]));
  DONE;
})

(define_insn "stack_protect_testdi"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (unspec:DI [(match_operand:DI 1 "memory_operand" "m")
                    (match_operand:DI 2 "memory_operand" "m")]
                   UNSPEC_SP_TEST))
  (set (match_scratch:DI 3 "=&r") (const_int 0))]
  "!TARGET_MUSL_COMPAT"
  "ld\\t%0,%1\\n\\tld\\t%3,%2\\n\\txor\\t%0,%3,%0\\n\\tor\\t%3,0,(0)1"
  [(set_attr "type" "load")
   (set_attr "mode" "DI")
   (set_attr "length" "4")])

(define_insn "prefetch"
  [(prefetch (match_operand:DI 0 "general_operand" "r")
             (match_operand 1 "const_int_operand" "")
             (match_operand 2 "const_int_operand" ""))]
  ""
  "pfch\\t0(,%0)"
  [(set_attr "type" "load")
   (set_attr "mode" "none")
   (set_attr "length" "1")])

(define_insn "flush_icache"
  [(unspec_volatile [(const_int 0)] UNSPEC_FLUSH_ICACHE)]
  ""
  "* return \"fencec\\t2\";"
  [(set_attr "type" "misc")
   (set_attr "mode" "none")
   (set_attr "length" "1")])

; set fp_mode to round_nearest and clean all exception masks
(define_insn "init_program_mode"
  [(unspec_volatile [(const_int 0)] UNSPEC_INIT_PROGRAM_MODE)]
  ""
  "* 
{
  rtx xop[1];
  xop[0] = gen_rtx_REG(DImode,VE_SCRATCH_REGNUM);
  output_asm_insn(\"lea\\t%0,0x3000\",xop);
  output_asm_insn(\"lpm\\t%0\",xop);
  return \"\";
}"
  [(set_attr "type" "misc")
   (set_attr "mode" "none")
   (set_attr "length" "2")])

;; Atomic operations
(define_expand "memory_barrier"
  [(set (match_dup 0)
        (unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
        (unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  ""
  "fencem\\t3"
  [(set_attr "type"     "misc")
   (set_attr "mode"     "none")
   (set_attr "length"   "1")])

(define_insn "sync_compare_and_swap<mode>"
  [(parallel
    [(set (match_operand:MODEI 0 "register_operand" "=r,r,&r,&r")
          (match_operand:MODEI 1 "memory_operand" "+m,m,m,m"))
     (set (match_dup 1)
          (unspec_volatile:MODEI
            [(match_dup 1)
             (match_operand:MODEI 2 "reg_or_i_operand" "r,I,r,I")
             (match_operand:MODEI 3 "register_operand" "0,0,r,r")]
            UNSPEC_COMPARE_AND_SWAP)) ])]
  ""
  "*
{
 switch(which_alternative)
 {
     default: case 2: case 3:
        output_asm_insn(\"or\\t%0,0,%3\",operands);

     case 0: case 1:
        ve_asm_output_ldst_unified(\"cas.<suffix>\",VE_DIR_ATOMIC01,operands,
                                   operands[2]);
        return \"\";
 }
}"
  [(set_attr "type"     "multi")
   (set_attr "mode"     "<MODE>")
   (set_attr "length"   "1,1,2,2")])

(define_insn "sync_<atomic_insn>di"
  [(set (match_operand:DI 0 "memory_operand" "+m")
        (unspec_volatile:DI
         [(atomic:DI (match_dup 0)
                  (match_operand:DI 1 "register_operand" "r"))]
         UNSPEC_SYNC_OP))
   (clobber (match_scratch:DI 2 "=&r")) ]
  ""
  "*
{
  output_asm_insn(\"<atomic_set>\\t%2,0,%1\",operands);
  ve_asm_output_ldst_unified(\"atmam\",VE_DIR_ATOMIC20,operands,
                     gen_rtx_CONST_INT(DImode,<atomic_op>));
  return \"\";
}"
  [(set_attr "type"     "multi")
   (set_attr "mode"     "DI")
   (set_attr "length"   "2")])

(define_insn "sync_old_<atomic_insn>di"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (match_operand:DI 1 "memory_operand" "+m"))
   (set (match_dup 1)
        (unspec_volatile:DI
         [(atomic:DI (match_dup 1)
                     (match_operand:DI 2 "register_operand" "r")) ]
         UNSPEC_SYNC_OP))]
  ""
  "*
{
  output_asm_insn(\"<atomic_set>\\t%0,0,%2\",operands);
  ve_asm_output_ldst_unified(\"atmam\",VE_DIR_ATOMIC01,operands,
                     gen_rtx_CONST_INT(DImode,<atomic_op>));
  return \"\";
}"
  [(set_attr "type"     "multi")
   (set_attr "mode"     "DI")
   (set_attr "length"   "2")])

(define_insn "sync_new_<atomic_insn>di"
  [(parallel [
     (set (match_operand:DI 0 "register_operand" "=&r")
          (unspec_volatile:DI 
            [(atomic:DI (match_operand:DI 1 "memory_operand" "+m")
                        (match_operand:DI 2 "register_operand" "r"))]
            UNSPEC_SYNC_NEW_OP))
     (set (match_dup 1)
          (unspec_volatile:DI [(match_dup 1) (match_dup 2)]
            UNSPEC_SYNC_NEW_OP))])]
  ""
  "*
{
  output_asm_insn(\"<atomic_set>\\t%0,0,%2\",operands);
  ve_asm_output_ldst_unified(\"atmam\",VE_DIR_ATOMIC01,operands,
                     gen_rtx_CONST_INT(DImode,<atomic_op>));
  output_asm_insn(\"ld\\t%0,%1\",operands);
  return \"\";
}"
  [(set_attr "type"     "multi")
   (set_attr "mode"     "DI")
   (set_attr "length"   "3")])

(define_insn "sync_lock_test_and_setdi"
  [(set (match_operand:DI 0 "register_operand" "=&r,&r")
        (match_operand:DI 1 "memory_operand" "+m,m"))
   (set (match_dup 1)
        (unspec_volatile:DI
            [(match_operand:DI 2 "register_operand" "0,r")]
         UNSPEC_SYNC_LOCK_TEST_AND_SET))
   (clobber (match_scratch:DI 3 "=&r,&r")) ]
  ""
  "*
{
 switch(which_alternative)
 {
   default: case 1:
     output_asm_insn(\"or\\t%0,0,%2\",operands);

   case 0:
     output_asm_insn(\"or\\t%3,0,(56)0\",operands);
     ve_asm_output_ldst_unified(\"ts1am.l\",VE_DIR_ATOMIC01,operands,
                     operands[3]);
     return \"\";
 }
}"
  [(set_attr "type"     "multi")
   (set_attr "mode"     "DI")
   (set_attr "length"   "3")])

(define_insn "sync_lock_test_and_setsi"
  [(set (match_operand:SI 0 "register_operand" "=&r,&r")
        (match_operand:SI 1 "memory_operand" "+m,m"))
   (set (match_dup 1)
        (unspec_volatile:SI
            [(match_operand:SI 2 "register_operand" "0,r")]
         UNSPEC_SYNC_LOCK_TEST_AND_SET))]
  ""
  "*
{
 switch(which_alternative)
 {
   default: case 1:
     output_asm_insn(\"or\\t%0,0,%2\",operands);

   case 0:
     ve_asm_output_ldst_unified(\"ts1am.w\",VE_DIR_ATOMIC01,operands,
                     gen_rtx_CONST_INT(DImode,15));
     return \"\";
 }
}"
  [(set_attr "type"     "multi")
   (set_attr "mode"     "SI")
   (set_attr "length"   "2")])

(define_insn "sync_lock_releasedi"
  [(set (match_operand:DI 0 "memory_operand" "=m")
        (unspec_volatile:DI
          [(match_operand:DI 1 "register_operand" "r")]
          UNSPEC_SYNC_LOCK_RELEASE))
   (clobber (match_scratch:DI 2 "=&r")) 
   (clobber (match_scratch:DI 3 "=&r")) ]
  ""
  "*
{
  output_asm_insn(\"or\\t%3,0,(56)0\",operands);
  output_asm_insn(\"or\\t%2,0,%1\",operands);
  ve_asm_output_ldst_unified(\"ts1am.l\",VE_DIR_ATOMIC20,operands,
                  operands[3]);
  return \"\";
}"
  [(set_attr "type"     "multi")
   (set_attr "mode"     "DI")
   (set_attr "length"   "3")])

(define_insn "sync_lock_releasesi"
  [(set (match_operand:SI 0 "memory_operand" "=m")
        (unspec_volatile:SI
          [(match_operand:SI 1 "register_operand" "r")]
          UNSPEC_SYNC_LOCK_RELEASE))
   (clobber (match_scratch:SI 2 "=&r"))] 
  ""
  "*
{
  output_asm_insn(\"or\\t%2,0,%1\",operands);
  ve_asm_output_ldst_unified(\"ts1am.w\",VE_DIR_ATOMIC20,operands,
                   gen_rtx_CONST_INT(DImode,15));
  return \"\";
}"
  [(set_attr "type"     "multi")
   (set_attr "mode"     "SI")
   (set_attr "length"   "2")])


;;
;; No-Op
;;


(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "none")
   (set_attr "length"   "1")])

;;
;; unconditional branches and such.
;;

(define_insn "indirect_jump"
  [(set (pc) (match_operand:DI 0 "register_operand" "r"))]
  ""
  "b.l.t\\t(,%0)"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == LABEL_REF)
   return \"br.l.t\\t%0\";
  else if (GET_CODE (operands[0]) == REG)
   return \"b.l.t\\t(,%0)\";
  else
   return \"br.l.t\\t%0\";
}"
  [(set_attr "type"     "jump")
   (set_attr "mode"     "none")])


;;
;; calls
;;

(define_expand "call_value"
   [(parallel [(set (match_operand 0 "register_operand" "=r")
                    (call (match_operand 1 "sym_ref_mem_operand" "")
                          (match_operand 2 "const_int_operand" "i")))
               (clobber (reg:DI 9))
               (clobber (reg:DI 10))
               (clobber (reg:DI 17)) ])]
  "" 
{
    rtx callee;
    callee = XEXP(operands[1],0);
    if (GET_CODE(callee) != REG && ve_check_symbol(callee,Pmode)) 
            callee = ve_indirect_addr(callee,true,true); 
    operands[1] = gen_rtx_MEM( GET_MODE (operands[1]),
                           force_reg (Pmode, callee));
})

(define_expand "call"
  [(parallel [(call (match_operand 0 "sym_ref_mem_operand" "")
                    (match_operand 1 "const_int_operand" "i"))
             (clobber (reg:DI 9))
             (clobber (reg:DI 10))
             (clobber (reg:DI 17)) ])]
  ""
{
    rtx callee;
    callee = XEXP(operands[0],0);
    if (GET_CODE(callee) != REG && ve_check_symbol(callee,Pmode)) 
            callee = ve_indirect_addr(callee,true,true); 
    operands[0] = gen_rtx_MEM( GET_MODE (operands[0]),
                           force_reg (Pmode, callee));
})

(define_insn "call_value_indirect"
  [(parallel [(set (match_operand 0 "" "")
                   (call (mem:DI (match_operand:DI 1 "register_operand" "r"))
                           (match_operand 2 "const_int_operand" "i")))
              (clobber (reg:DI 9))
              (clobber (reg:DI 10))
              (clobber (reg:DI 17)) ])]
  ""
  "* 
{
    return ve_output_call_instr_value(&operands[0]);
}"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_insn "call_indirect"
  [(parallel [(call (mem:DI (match_operand:DI 0 "register_operand" "r"))
                    (match_operand 1 "" "i"))
               (clobber (reg:DI 9))
               (clobber (reg:DI 10))
               (clobber (reg:DI 17)) ])]
  ""
  "* 
{
    return ve_output_call_instr(&operands[0]);
}"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])


;; Call subroutine returning any type.
;; used in __builtin_apply

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "")
                    (const_int 0))
              (match_operand 1 "")
              (match_operand 2 "")])]
  ""
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());
  DONE;
})


;;
;; tablejump insn; used in generating code for switches.
;;

(define_expand "tablejump"
  [(parallel [(set (pc)
                   (match_operand:DI 0 "register_operand" "r"))
              (use (label_ref (match_operand 1 "" "")))])]
 ""
{
 rtx tempreg = gen_reg_rtx(DImode);
 emit_move_insn(tempreg,gen_rtx_PLUS(DImode,operands[0],
   gen_rtx_LABEL_REF(DImode,operands[1])));
 operands[0] = tempreg;
})

(define_insn "tablejump_internal"
  [(parallel [(set (pc)
                   (match_operand:DI 0 "register_operand" "r"))
              (use (label_ref (match_operand 1 "" "")))])]
 ""
 "b.l.t\\t(,%0)"
  [(set_attr "type"     "jump")
   (set_attr "mode"     "none")
   (set_attr "length"   "1")])

;;
;; Floating point comparison
;;
(define_insn "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (match_operator:SI 1 "comparison_operator"
         [(match_operand:MODEF 2 "register_operand" "r")
          (match_operand:MODEF 3 "register_operand" "r")]))]
  ""
  "*
{
  if (flag_signaling_nans)
   {
    rtx xop[5];
    xop[0] = operands[0];
    xop[1] = operands[1];
    xop[2] = operands[2];
    xop[3] = operands[3];
    xop[4] = gen_rtx_REG(<MODE>mode,VE_SCRATCH_REGNUM);
    output_asm_insn(\"or\\t%0,1,(0)1\",xop);
    output_asm_insn(\"fcmp.<suffix>\\t%4,%2,%3\",xop);
    output_asm_insn(\"br%F1.<suffix>%+\\t%4,0,16\",xop);
    output_asm_insn(\"or\\t%0,0,(0)1\",xop);
    return \"\";
   }
  output_asm_insn(\"or\\t%0,1,(0)1\",operands);
  output_asm_insn(\"br%F1.<suffix>%+\\t%2,%3,16\",operands);
  output_asm_insn(\"or\\t%0,0,(0)1\",operands);
  return \"\";
}"
  [(set_attr "type" "fp")
   (set_attr "mode" "<MODE>")
   (set_attr "length"   "3")])

(define_insn "cstoretf4"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (match_operator:SI 1 "comparison_operator"
         [(match_operand:TF 2 "register_operand" "r")
          (match_operand:TF 3 "register_operand" "r")]))
   (clobber (match_scratch:DI 4 "=&r")) 
   (clobber (match_scratch:DI 5 "=&r"))] 
  ""
  "*
{
  if (flag_signaling_nans)
   {
    output_asm_insn(\"or\\t%0,1,(0)1\",operands);
    output_asm_insn(\"fcmp.q\\t%4,%2,%3\",operands);
    output_asm_insn(\"br%F1.d%+\\t%4,0,16\",operands);
    output_asm_insn(\"or\\t%0,0,(0)1\",operands);
    return \"\";
   }
  switch (GET_CODE(operands[1]))
   {
    default:
    case EQ: case LTGT: case GT: case GE: case LT: case LE:
    case ORDERED:
      output_asm_insn(\"srl\\t%4,(15)1,1\",operands);
      output_asm_insn(\"and\\t%5,%2,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,80\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q2,0,64\",operands);
      output_asm_insn(\"and\\t%5,%3,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,48\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q3,0,32\",operands);
      output_asm_insn(\"fcmp.q\\t%4,%2,%3\",operands);
      output_asm_insn(\"or\\t%0,1,(0)1\",operands);
      output_asm_insn(\"br%F1.d%+\\t%4,0,16\",operands);
      output_asm_insn(\"or\\t%0,0,(0)1\",operands);
      return \"\";
    case UNEQ: case NE: case UNGT: case UNGE: case UNLT: case UNLE:
    case UNORDERED:
      output_asm_insn(\"srl\\t%4,(15)1,1\",operands);
      output_asm_insn(\"and\\t%5,%2,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,80\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q2,0,64\",operands);
      output_asm_insn(\"and\\t%5,%3,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,48\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q3,0,32\",operands);
      output_asm_insn(\"fcmp.q\\t%4,%2,%3\",operands);
      output_asm_insn(\"or\\t%0,0,(0)1\",operands);
      output_asm_insn(\"br%E1.d%-\\t%4,0,16\",operands);
      output_asm_insn(\"or\\t%0,1,(0)1\",operands);
      return \"\";
   }
}"
  [(set_attr "type" "fp")
   (set_attr "mode" "TF")
   (set_attr "length"   "13")])

;;
;; Conditional moves 
;;

(define_insn "movsi<mode>cc_internal"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
        (if_then_else:SI (match_operator 1 "ve_signed_comparison_operator" 
                       [(match_operand:MODEI 2 "register_operand" "r,r,r,r")
                        (const_int 0)])
                      (match_operand:SI 3 "reg_or_o_operand" "r,O,0,0")
                      (match_operand:SI 4 "reg_or_o_operand" "0,0,r,O")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%C1\\t%0,%3,%2
   cmov.<suffix>.%C1\\t%0,%O3,%2
   cmov.<suffix>.%N1\\t%0,%4,%2
   cmov.<suffix>.%N1\\t%0,%O4,%2"
  [(set_attr "type"     "move")
   (set_attr "mode"     "SI")
   (set_attr "length" "1,1,1,1")])

(define_insn "movsi<mode>cc_internal"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
        (if_then_else:SI (match_operator 1 "comparison_operator"
                       [(match_operand:MODEF 2 "register_operand" "r,r,r,r")
                        (match_operand:MODEF 5 "const_zero_operand" "G,G,G,G")])
                      (match_operand:SI 3 "reg_or_o_operand" "r,O,0,0")
                      (match_operand:SI 4 "reg_or_o_operand" "0,0,r,O")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%F1\\t%0,%3,%2
   cmov.<suffix>.%F1\\t%0,%O3,%2
   cmov.<suffix>.%E1\\t%0,%4,%2
   cmov.<suffix>.%E1\\t%0,%O4,%2"
  [(set_attr "type"     "move")
   (set_attr "mode"     "SI")
   (set_attr "length" "1,1,1,1")])


(define_insn "movdi<mode>cc_internal"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r")
        (if_then_else:DI (match_operator 1 "ve_signed_comparison_operator"
                       [(match_operand:MODEI 2 "register_operand" "r,r,r,r")
                        (const_int 0)])
                      (match_operand:DI 3 "reg_or_m_operand" "r,M,0,0")
                      (match_operand:DI 4 "reg_or_m_operand" "0,0,r,M")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%C1\\t%0,%3,%2
   cmov.<suffix>.%C1\\t%0,%M3,%2
   cmov.<suffix>.%N1\\t%0,%4,%2
   cmov.<suffix>.%N1\\t%0,%M4,%2"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "1,1,1,1")])

(define_insn "movdi<mode>cc_internal"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r")
        (if_then_else:DI (match_operator 1 "comparison_operator"
                       [(match_operand:MODEF 2 "register_operand" "r,r,r,r")
                        (match_operand:MODEF 5 "const_zero_operand" "G,G,G,G")])
                      (match_operand:DI 3 "reg_or_m_operand" "r,M,0,0")
                      (match_operand:DI 4 "reg_or_m_operand" "0,0,r,M")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%F1\\t%0,%3,%2
   cmov.<suffix>.%F1\\t%0,%M3,%2
   cmov.<suffix>.%E1\\t%0,%4,%2
   cmov.<suffix>.%E1\\t%0,%M4,%2"
  [(set_attr "type"     "move")
   (set_attr "mode"     "DI")
   (set_attr "length" "1,1,1,1")])

(define_insn "movti<mode>cc_internal"
  [(set (match_operand:TI 0 "register_operand" "=&r,&r,&r,&r")
        (if_then_else:TI (match_operator 1 "ve_signed_comparison_operator"
                       [(match_operand:MODEI 2 "register_operand" "r,r,r,r")
                        (const_int 0)])
                      (match_operand:TI 3 "reg_or_m_operand" "r,M,0,0")
                      (match_operand:TI 4 "reg_or_m_operand" "0,0,r,M")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%C1\\t%Q0,%Q3,%2\\n\\tcmov.<suffix>.%C1\\t%0,%3,%2
   cmov.<suffix>.%C1\\t%Q0,%M3,%2\\n\\tsra.l\\t%0,%Q0,63
   cmov.<suffix>.%N1\\t%Q0,%Q4,%2\\n\\tcmov.<suffix>.%C1\\t%0,%4,%2
   cmov.<suffix>.%N1\\t%Q0,%M4,%2\\n\\tsra.l\\t%0,%Q0,63"
  [(set_attr "type"     "move")
   (set_attr "mode"     "TI")
   (set_attr "length" "2,2,2,2")])

(define_insn "movti<mode>cc_internal"
  [(set (match_operand:TI 0 "register_operand" "=&r,&r,&r,&r")
        (if_then_else:TI (match_operator 1 "comparison_operator"
                       [(match_operand:MODEF 2 "register_operand" "r,r,r,r")
                        (match_operand:MODEF 5 "const_zero_operand" "G,G,G,G")])
                      (match_operand:TI 3 "reg_or_m_operand" "r,M,0,0")
                      (match_operand:TI 4 "reg_or_m_operand" "0,0,r,M")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%F1\\t%Q0,%Q3,%2\\n\\tcmov.<suffix>.%F1\\t%0,%3,%2
   cmov.<suffix>.%F1\\t%Q0,%M3,%2\\n\\tsra.l\\t%0,%Q0,63
   cmov.<suffix>.%E1\\t%Q0,%Q4,%2\\n\\tcmov.<suffix>.%E1\\t%0,%4,%2
   cmov.<suffix>.%E1\\t%Q0,%M4,%2\\n\\tsra.l\\t%0,%Q0,63"
  [(set_attr "type"     "move")
   (set_attr "mode"     "TI")
   (set_attr "length" "2,2,2,2")])

;;

(define_insn "movsf<mode>cc_internal"
  [(set (match_operand:SF 0 "register_operand" "=r,r")
        (if_then_else:SF (match_operator 1 "ve_signed_comparison_operator"
                       [(match_operand:MODEI 2 "register_operand" "r,r")
                        (const_int 0)])
                      (match_operand:SF 3 "register_operand" "r,0")
                      (match_operand:SF 4 "register_operand" "0,r")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%C1\\t%0,%3,%2
   cmov.<suffix>.%N1\\t%0,%4,%2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SF")
   (set_attr "length" "1,1")])

(define_insn "movsf<mode>cc_internal"
  [(set (match_operand:SF 0 "register_operand" "=r,r")
        (if_then_else:SF (match_operator 1 "comparison_operator"
                       [(match_operand:MODEF 2 "register_operand" "r,r")
                        (match_operand:MODEF 5 "const_zero_operand" "G,G")])
                      (match_operand:SF 3 "register_operand" "r,0")
                      (match_operand:SF 4 "register_operand" "0,r")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%F1\\t%0,%3,%2
   cmov.<suffix>.%E1\\t%0,%4,%2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "SF")
   (set_attr "length" "1,1")])

(define_insn "movdf<mode>cc_internal"
  [(set (match_operand:DF 0 "register_operand" "=r,r")
        (if_then_else:DF (match_operator 1 "ve_signed_comparison_operator"
                       [(match_operand:MODEI 2 "register_operand" "r,r")
                        (const_int 0)])
                      (match_operand:DF 3 "register_operand" "r,0")
                      (match_operand:DF 4 "register_operand" "0,r")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%C1\\t%0,%3,%2
   cmov.<suffix>.%N1\\t%0,%4,%2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "DF")
   (set_attr "length" "1,1")])

(define_insn "movdf<mode>cc_internal"
  [(set (match_operand:DF 0 "register_operand" "=r,r")
        (if_then_else:DF (match_operator 1 "comparison_operator"
                       [(match_operand:MODEF 2 "register_operand" "r,r")
                        (match_operand:MODEF 5 "const_zero_operand" "G,G")])
                      (match_operand:DF 3 "register_operand" "r,0")
                      (match_operand:DF 4 "register_operand" "0,r")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%F1\\t%0,%3,%2
   cmov.<suffix>.%E1\\t%0,%4,%2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "DF")
   (set_attr "length" "1,1")])

(define_insn "movtf<mode>cc_internal"
  [(set (match_operand:TF 0 "register_operand" "=r,r")
        (if_then_else:TF (match_operator 1 "ve_signed_comparison_operator"
                       [(match_operand:MODEI 2 "register_operand" "r,r")
                        (const_int 0)])
                      (match_operand:TF 3 "register_operand" "r,0")
                      (match_operand:TF 4 "register_operand" "0,r")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%C1\\t%0,%3,%2\\n\\tcmov.<suffix>.%C1\\t%Q0,%Q3,%2
   cmov.<suffix>.%N1\\t%0,%4,%2\\n\\tcmov.<suffix>.%N1\\t%Q0,%Q4,%2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "TF")
   (set_attr "length" "2,2")])

(define_insn "movtf<mode>cc_internal"
  [(set (match_operand:TF 0 "register_operand" "=r,r")
        (if_then_else:TF (match_operator 1 "comparison_operator"
                       [(match_operand:MODEF 2 "register_operand" "r,r")
                        (match_operand:MODEF 5 "const_zero_operand" "G,G")])
                      (match_operand:TF 3 "register_operand" "r,0")
                      (match_operand:TF 4 "register_operand" "0,r")))]
  "!TARGET_NO_CMOV"
  "@
   cmov.<suffix>.%F1\\t%0,%3,%2\\n\\tcmov.<suffix>.%F1\\t%Q0,%Q3,%2
   cmov.<suffix>.%E1\\t%0,%4,%2\\n\\tcmov.<suffix>.%E1\\t%Q0,%Q4,%2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "TF")
   (set_attr "length" "2,2")])

(define_insn "movtftfcc_internal"
  [(set (match_operand:TF 0 "register_operand" "=r,r")
        (if_then_else:TF (match_operator 1 "comparison_operator"
                       [(match_operand:TF 2 "register_operand" "r,r")
                        (match_operand:TF 5 "register_operand" "r,r")])
                      (match_operand:TF 3 "register_operand" "r,0")
                      (match_operand:TF 4 "register_operand" "0,r")))
   (clobber (match_scratch:DF 6 "=&r,&r"))]
  "!TARGET_NO_CMOV"
  "@
   fcmp.q\\t%6,%2,%5\\n\\tcmov.d.%F1\\t%0,%3,%6\\n\\tcmov.d.%F1\\t%Q0,%Q3,%6
   fcmp.q\\t%6,%2,%5\\n\\tcmov.d.%E1\\t%0,%4,%6\\n\\tcmov.d.%E1\\t%Q0,%Q4,%6"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "TF")
   (set_attr "length" "3,3")])

;;
;; No scratch registers can be used, after reload.
(define_insn "cmp<mode>3_internal"
  [(set (match_operand:MODEI 0 "register_operand" "=r,r")
        (unspec:MODEI
             [(match_operand:MODEI 1 "register_operand" "r,r")
              (match_operand:MODEI 2 "reg_or_i_operand" "r,I")]
              UNSPEC_COMPARE))]
  ""
  "*
{
  rtx xop[4];
  switch(which_alternative)
  {
     default: case 0:
       return \"cmps.<suffix><ext>\\t%0,%1,%2\";
     case 1:
       switch(INTVAL(operands[2])) 
       {
         case 1: case 3: case 7: case 15: case 31: case 63:
           return \"cmps.<suffix><ext>\\t%0,%1,%O2\";
         default:
           xop[0] = operands[0];
           xop[1] = operands[1];
           xop[2] = operands[2];
           xop[3] = gen_rtx_REG(<MODE>mode,VE_SCRATCH_REGNUM);
           output_asm_insn(
             \"or\\t%3,%2,(0)1\\n\\tcmps.<suffix><ext>\\t%0,%1,%3\",xop);
           return \"\";
       }
   }
}"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "<MODE>")
   (set_attr "length" "1,2")])

(define_insn "cmp<mode>3_internal"
  [(set (match_operand:MODEF 0 "register_operand" "=r")
        (unspec:MODEF
            [(match_operand:MODEF 1 "register_operand" "r")
             (match_operand:MODEF 2 "register_operand" "r")]
            UNSPEC_COMPARE))]
  ""
  "fcmp.<suffix>\\t%0,%1,%2"
  [(set_attr "type"     "alu")
   (set_attr "mode"     "<MODE>")
   (set_attr "length" "1")])

(define_expand "mov<mode>cc"
  [(set (match_operand:MODEI 0 "register_operand" "")
        (if_then_else:MODEI (match_operand 1 "ve_signed_comparison_operator" "")
                         (match_operand:MODEI 2 "register_operand" "")
                         (match_operand:MODEI 3 "register_operand" "")))]
  ""
{
    enum rtx_code code = GET_CODE(operands[1]);
    machine_mode mode10 = GET_MODE(XEXP(operands[1],0));
    machine_mode mode1 = GET_MODE(operands[1]);
    rtx tmpreg = gen_reg_rtx(mode10);
    if (XEXP(operands[1],1) != CONST0_RTX(mode10))
    {
      emit_insn(gen_rtx_SET(tmpreg,
          gen_rtx_UNSPEC(mode10,
               gen_rtvec(2,XEXP(operands[1],0),XEXP(operands[1],1)),
               UNSPEC_COMPARE)));
      operands[1] = gen_rtx_fmt_ee (code,mode1,tmpreg,CONST0_RTX(mode10));
    }
})

(define_expand "mov<mode>cc"
  [(set (match_operand:MODEF 0 "register_operand" "")
        (if_then_else:MODEF (match_operand 1 "comparison_operator" "")
                         (match_operand:MODEF 2 "register_operand" "")
                         (match_operand:MODEF 3 "register_operand" "")))]
  ""
{
    enum rtx_code code = GET_CODE(operands[1]);
    machine_mode mode10 = GET_MODE(XEXP(operands[1],0));
    machine_mode mode1 = GET_MODE(operands[1]);
    rtx tmpreg = gen_reg_rtx(mode10);
    if (XEXP(operands[1],1) != CONST0_RTX(mode10))
    {
      emit_insn(gen_rtx_SET(tmpreg,
          gen_rtx_UNSPEC(mode10,
               gen_rtvec(2,XEXP(operands[1],0),XEXP(operands[1],1)),
               UNSPEC_COMPARE)));
      operands[1] = gen_rtx_fmt_ee (code,mode1,tmpreg,CONST0_RTX(mode10));
    }
})


;;;
;;; Comparison + branches.
;;;

(define_insn "cbranch<mode>4"
  [(set (pc)
        (if_then_else 
            (match_operator 0 "comparison_operator"
               [(match_operand:MODEI 1 "register_operand" "r,r,r")
                (match_operand:MODEI 2 "<reg_or_ip_operand>" "r,<p_const>,I")])
              (label_ref (match_operand 3 ""))
              (pc)))
   (clobber (match_scratch:MODEI 4 "=&r,&r,&r"))]
  ""
  "*
{
  switch(which_alternative)
   {
     default: case 0: 
       { enum rtx_code code = GET_CODE(operands[0]);
       switch(code)
         {
           case GTU: case GEU: case LTU: case LEU:
              return
             \"cmpu.<suffix>\\t%4,%1,%2\\n\\tbr%C0.<suffix>%+\\t%4,0,%3\";
           default:
              return \"br%C0.<suffix>%+\\t%1,%2,%3\";
         }
       }
     case 1:
       { enum rtx_code code = GET_CODE(operands[0]);
       switch(code)
         {
           case GTU: case GEU: case LTU: case LEU:
              return
    \"cmpu.<suffix>\\t%4,%1,%<p_const>2\\n\\tbr%C0.<suffix>%+\\t%4,0,%3\";
           default:
              switch (INTVAL(operands[2]))
                {
                  case 0:
              	    return \"br%C0.<suffix>%+\\t%1,0,%3\";
                  default:
                    return
    \"cmps.<suffix>\\t%4,%1,%<p_const>2\\n\\tbr%C0.<suffix>%+\\t%4,0,%3\";
                }
         }
       }
     case 2:
       { enum rtx_code code = GET_CODE(operands[0]);
       switch(code)
         {
           case GTU: case GEU: case LTU: case LEU:
              return
             \"cmpu.<suffix>\\t%4,%2,%1\\n\\tbr%R0.<suffix>%+\\t%4,0,%3\";
           default:
              return \"br%R0.<suffix>%+\\t%2,%1,%3\";
         }
       }
    }
   
}"

  [(set_attr "type" "branch")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1,1,1")])


(define_insn "cbranchti4"
  [(set (pc)
        (if_then_else 
            (match_operator 0 "comparison_operator"
               [(match_operand:TI 1 "register_operand" "r,r")
                (match_operand:TI 2 "reg_or_i_operand" "r,I")])
              (label_ref (match_operand 3 ""))
              (pc)))
   (clobber (match_scratch:DI 4 "=&r,&r"))]
  ""
  "*
{
  switch(which_alternative)
   {
     default: case 0: 
       { 
       enum rtx_code code = GET_CODE(operands[0]);
       switch(code)
         {
           default: case EQ: 
              return \"brne.l\\t%1,%2,16\\n\\tbreq.l%+\\t%Q1,%Q2,%3\";
           case NE:
              return \"brne.l\\t%1,%2,%3\\n\\tbrne.l%+\\t%Q1,%Q2,%3\";
           case GT: case GE: case LT: case LE: 
              output_asm_insn(\"br%J0.l\\t%1,%2,%3\",operands);
              output_asm_insn(\"brne.l\\t%1,%2,24\",operands);
              output_asm_insn(\"cmpu.l\\t%4,%Q1,%Q2\",operands);
              return \"br%C0.l%+\\t%4,0,%3\";
           case GTU: case GEU: case LTU: case LEU:
              output_asm_insn(\"cmpu.l\\t%4,%1,%2\",operands);
              output_asm_insn(\"br%J0.l\\t%4,0,%3\",operands);
              output_asm_insn(\"brne.l\\t%4,0,24\",operands);
              output_asm_insn(\"cmpu.l\\t%4,%Q1,%Q2\",operands);
              return \"br%C0.l%+\\t%4,0,%3\";
         }
       }
     case 1:
       { enum rtx_code code = GET_CODE(operands[0]);
       switch(code)
         {
           default: case EQ: 
              return \"brne.l\\t%Z2,%1,16\\n\\tbr%R0.l%+\\t%2,%Q1,%3\";
           case NE:
              return \"brne.l\\t%Z2,%1,%3\\n\\tbr%R0.l%+\\t%2,%Q1,%3\";
           case GT: case GE: case LT: case LE: 
              output_asm_insn(\"br%J0.l\\t%Z2,%1,%3\",operands);
              output_asm_insn(\"brne.l\\t%Z2,%1,24\",operands);
              output_asm_insn(\"cmpu.l\\t%4,%2,%Q1\",operands);
              return \"br%R0.l%+\\t%4,0,%3\";
           case GTU: case GEU: case LTU: case LEU:
              output_asm_insn(\"cmpu.l\\t%4,%Z2,%1\",operands);
              output_asm_insn(\"br%J0.l\\t%4,0,%3\",operands);
              output_asm_insn(\"brne.l\\t%4,0,24\",operands);
              output_asm_insn(\"cmpu.l\\t%4,%2,%Q1\",operands);
              return \"br%R0.l%+\\t%4,0,%3\";
         }
       }
    }
   
}"

  [(set_attr "type" "branch")
   (set_attr "mode" "TI")
   (set_attr "length" "2,2")])


(define_insn "cbranch<mode>4"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(match_operand:MODEF 1 "register_operand" "r")
                        (match_operand:MODEF 2 "register_operand" "r")])
                      (label_ref (match_operand 3 ""))
                      (pc)))]
  ""
  "*
{
  if (flag_signaling_nans)
   {
    rtx xop[5];
    xop[0] = operands[0];
    xop[1] = operands[1];
    xop[2] = operands[2];
    xop[3] = operands[3];
    xop[4] = gen_rtx_REG(<MODE>mode,VE_SCRATCH_REGNUM);
    output_asm_insn(\"fcmp.<suffix>\\t%4,%1,%2\",xop);
    output_asm_insn(\"br%F0.<suffix>%+\\t%4,0,%3\",xop);
    return \"\";
   }
  return \"br%F0.<suffix>%+\\t%1,%2,%3\";
}"
  [(set_attr "type" "branch")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1")])

(define_insn "cbranchtf4"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(match_operand:TF 1 "register_operand" "r")
                        (match_operand:TF 2 "register_operand" "r")])
                      (label_ref (match_operand 3 ""))
                      (pc)))
   (clobber (match_scratch:DI 4 "=&r"))
   (clobber (match_scratch:DI 5 "=&r"))]
  ""
  "*
{ 
  if (flag_signaling_nans)
   {
    output_asm_insn(\"fcmp.q\\t%4,%1,%2\",operands);
    output_asm_insn(\"br%F0.d%+\\t%4,0,%3\",operands);
    return \"\";
   }
  switch (GET_CODE(operands[0]))
   {
    default:
    case EQ: case LTGT: case GT: case GE: case LT: case LE:
    case ORDERED:
      output_asm_insn(\"srl\\t%4,(15)1,1\",operands);
      output_asm_insn(\"and\\t%5,%1,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,72\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q1,0,56\",operands);
      output_asm_insn(\"and\\t%5,%2,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,40\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q2,0,24\",operands);
      output_asm_insn(\"fcmp.q\\t%4,%1,%2\",operands);
      output_asm_insn(\"br%F0.d%+\\t%4,0,%3\",operands);
      return \"\";

    case UNEQ: case NE: case UNGT: case UNGE: case UNLT: case UNLE:
    case UNORDERED:
      output_asm_insn(\"srl\\t%4,(15)1,1\",operands);
      output_asm_insn(\"and\\t%5,%1,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,%3\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q1,0,%3\",operands);
      output_asm_insn(\"and\\t%5,%2,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,%3\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q2,0,%3\",operands);
      output_asm_insn(\"fcmp.q\\t%4,%1,%2\",operands);
      output_asm_insn(\"br%F0.d%+\\t%4,0,%3\",operands);
      return \"\";
   }
}"
  [(set_attr "type" "branch")
   (set_attr "mode" "TF")
   (set_attr "length" "11")])

(define_insn "*cbranch<mode>4_rev"
  [(set (pc)
        (if_then_else (match_operator 0 "ve_ordered_comparison_operator"
                       [(match_operand:MODEF 1 "register_operand" "r")
                        (match_operand:MODEF 2 "register_operand" "r")])
                      (pc)
                      (label_ref (match_operand 3 ""))))]
  ""
  "*
{
  if (flag_signaling_nans)
   {
    rtx xop[5];
    xop[0] = operands[0];
    xop[1] = operands[1];
    xop[2] = operands[2];
    xop[3] = operands[3];
    xop[4] = gen_rtx_REG(<MODE>mode,VE_SCRATCH_REGNUM);
    output_asm_insn(\"fcmp.<suffix>\\t%4,%1,%2\",xop);
    output_asm_insn(\"br%E0.<suffix>%-\\t%4,0,%3\",xop);
    return \"\";
   }
  return \"br%E0.<suffix>%-\\t%1,%2,%3\";
}"
  [(set_attr "type" "branch")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "1")])

(define_insn "*cbranchtf4_rev"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(match_operand:TF 1 "register_operand" "r")
                        (match_operand:TF 2 "register_operand" "r")])
                      (pc)
                      (label_ref (match_operand 3 ""))))
   (clobber (match_scratch:DI 4 "=&r"))
   (clobber (match_scratch:DI 5 "=&r"))]
  ""
  "*
{
  if (flag_signaling_nans)
   {
    output_asm_insn(\"fcmp.q\\t%4,%1,%2\",operands);
    output_asm_insn(\"br%E0.d%-\\t%4,0,%3\",operands);
    return \"\";
   }
  switch (GET_CODE(operands[0]))
   {
    default:
    case EQ: case LTGT: case GT: case GE: case LT: case LE:
    case ORDERED:
      output_asm_insn(\"srl\\t%4,(15)1,1\",operands);
      output_asm_insn(\"and\\t%5,%1,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,%3\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q1,0,%3\",operands);
      output_asm_insn(\"and\\t%5,%2,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,%3\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q2,0,%3\",operands);
      output_asm_insn(\"fcmp.q\\t%4,%1,%2\",operands);
      output_asm_insn(\"br%E0.d%-\\t%4,0,%3\",operands);
      return \"\";

    case UNEQ: case NE: case UNGT: case UNGE: case UNLT: case UNLE:
    case UNORDERED:
      output_asm_insn(\"srl\\t%4,(15)1,1\",operands);
      output_asm_insn(\"and\\t%5,%1,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,72\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q1,0,56\",operands);
      output_asm_insn(\"and\\t%5,%2,(1)0\",operands);
      output_asm_insn(\"brgt.l.nt\\t%5,%4,40\",operands);
      output_asm_insn(\"brlt.l.t\\t%5,%4,16\",operands);
      output_asm_insn(\"brne.l.t\\t%Q2,0,24\",operands);
      output_asm_insn(\"fcmp.q\\t%4,%1,%2\",operands);
      output_asm_insn(\"br%E0.d%-\\t%4,0,%3\",operands);
      return \"\";
   }
}"
  [(set_attr "type" "branch")
   (set_attr "mode" "TF")
   (set_attr "length" "11")])

;;
;; prologue
;;

(define_expand "prologue"
  [(const_int 0)]
  ""
{
  ve_expand_prologue ();
  DONE;
})

;; Just output comment for debugging
(define_insn "prologue_end"
  [(unspec_volatile [(const_int 0)] UNSPEC_PROLOGUE_END)]
  ""
  "# End of function prologue"
  [(set_attr "type" "misc")
   (set_attr "mode" "none")])

;;
;; epilogue
;;

;; Block any insns from being moved before this point, since the
;; profiling call to mcount can use various registers that are not
;; saved or used to pass arguments.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "ghost")
   (set_attr "mode" "none")])

(define_expand "epilogue"
  [(return)]
  ""
{
  ve_expand_epilogue ();
  DONE;
})

(define_insn "epilogue_start"
  [(unspec_volatile [(const_int 0)] UNSPEC_EPILOGUE_START)]
  ""
  "# Start of function epilogue"
  [(set_attr "type" "misc")
   (set_attr "mode" "none")])

(define_expand "return"
  [(return)]
  ""
{
  ve_expand_epilogue ();
  DONE;
})

(define_insn "*return"
  [(return)]
  ""
  "*
{
  rtx xop[1];
  xop[0] = gen_rtx_REG(DImode,VE_RETURN_REGNUM);
  output_asm_insn(\"b.l.t\\t(,%0)\",xop);
  return \"# End of function epilogue\";
}"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;; peephole optimizations
(define_peephole2
  [(set (match_operand:MODEI 0 "register_operand")
        (plus:MODEI (match_operand:MODEI 1 "register_operand")
                    (match_operand:MODEI 2 "const_n_operand")))
   (set (match_operand:MODEI 3 "register_operand")
        (plus:MODEI (match_dup 0)
                    (match_operand:MODEI 4 "register_operand")))]
   "peep2_reg_dead_p (2, operands[0])"
  [(set (match_dup 3)
        (plus:MODEI (match_dup 4)
                 (plus:MODEI (match_dup 1) (match_dup 2))))])

(define_peephole2
  [(set (match_operand:MODEI 0 "register_operand")
        (plus:MODEI (match_operand:MODEI 1 "register_operand")
                    (match_operand:MODEI 2 "register_operand")))
   (set (match_operand:MODEI 3 "register_operand")
        (plus:MODEI (match_dup 0)
                    (match_operand:MODEI 4 "const_n_operand")))]
   "peep2_reg_dead_p (2, operands[0])"
  [(set (match_dup 3)
        (plus:MODEI (match_dup 1)
                 (plus:MODEI (match_dup 2) (match_dup 4))))])

(define_peephole2
  [(set (match_operand:DI 0 "register_operand")
        (sign_extend:DI (match_operand:SI 1 "register_operand")))]
    "TARGET_OPT_SIGN_EXTENSION 
     && true_regnum(operands[0]) == true_regnum(operands[1])"
    [(const_int 0)])

(define_peephole2
  [(parallel [(set (match_operand:MODEOVHI 0 "register_operand")
                (sign_extend:MODEOVHI (match_operand:HI 1 "register_operand")))
              (clobber (match_scratch:DI 2 ))])]
    "TARGET_OPT_SIGN_EXTENSION 
     && true_regnum(operands[0]) == true_regnum(operands[1])"
    [(const_int 0)])

(define_peephole2
  [(parallel [(set (match_operand:MODEOVQI 0 "register_operand")
                (sign_extend:MODEOVQI (match_operand:QI 1 "register_operand")))
              (clobber (match_scratch:DI 2 ))])]
    "TARGET_OPT_SIGN_EXTENSION 
     && true_regnum(operands[0]) == true_regnum(operands[1])"
    [(const_int 0)])

