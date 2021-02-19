;; Predicate definitions for VE
;; Copyright (C) 2007-2016 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Changes by NEC Corporation for the VE port, 2017-2021

;; Return 1 if OP is an I constant or any register.
(define_predicate "reg_or_i_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) < 64 && INTVAL (op) >= -64")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an J constant or any register.
(define_predicate "reg_or_j_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) < 32 && INTVAL (op) >= 0")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an K constant or any register.
(define_predicate "reg_or_k_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) < 64 && INTVAL (op) >= 0")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an L constant or any register.
(define_predicate "reg_or_l_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) < 128 && INTVAL (op) >= 0")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an M constant or any register.
(define_predicate "reg_or_m_operand"
  (if_then_else (match_code "const_int")
    (match_test " ((INTVAL(op) & (INTVAL(op)+1))==0)||((~INTVAL(op) & (~INTVAL(op)+1))==0) ")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an O constant or any register.
(define_predicate "reg_or_o_operand"
  (if_then_else (match_code "const_int")
    (match_test " INTVAL(op) <= 2147483647LL && INTVAL(op) >= -2147483648LL
     && (((INTVAL(op) & (INTVAL(op)+1))==0)
    ||(((0xffffffffLL^(0xffffffffLL & INTVAL(op))) 
     & ((0xffffffffLL^(0xffffffffLL &INTVAL(op)))+1))==0)) ")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is a P constant or any register.
(define_predicate "reg_or_p_operand"
  (if_then_else (match_code "const_int")
    (match_test " INTVAL(op) == 0 || INTVAL(op) == -1 
                || INTVAL(op) == 0xffffffff ||
      (INTVAL(op) <= 2147483647LL && INTVAL(op) >= 0
     && ((INTVAL(op) & (INTVAL(op)+1))==0)  ) ")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an N constant or any register.
(define_predicate "reg_or_in_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) <= 2147483647LL && INTVAL(op) >= -2147483648LL")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an I or M constant or any register.
(define_predicate "reg_or_im_operand"
  (if_then_else (match_code "const_int")
    (match_test "(INTVAL (op) < 64 && INTVAL (op) >= -64) ||
    ((INTVAL(op) & (INTVAL(op)+1))==0)||((~INTVAL(op) & (~INTVAL(op)+1))==0) ")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an I or O constant or any register.
(define_predicate "reg_or_io_operand"
  (if_then_else (match_code "const_int")
    (match_test "(INTVAL (op) < 64 && INTVAL (op) >= -64) ||
                ( INTVAL(op) <= 2147483647LL && INTVAL(op) >= -2147483648LL
  && (((INTVAL(op) & (INTVAL(op)+1))==0)
  ||(((0xffffffffLL^(0xffffffffLL&INTVAL(op))) 
   & ((0xffffffffLL^(0xffffffffLL&INTVAL(op)))+1))==0))) ")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is an I or P constant or any register.
(define_predicate "reg_or_ip_operand"
  (if_then_else (match_code "const_int")
    (match_test "(INTVAL (op) < 64 && INTVAL (op) >= -64) ||
                INTVAL(op) == 0 || INTVAL(op) == -1 
                || INTVAL(op) == 0xffffffff ||
      (INTVAL(op) <= 2147483647LL && INTVAL(op) >= 0
     && ((INTVAL(op) & (INTVAL(op)+1))==0)  ) ")
    (match_operand 0 "register_operand")))

(define_predicate "reg_or_imn_operand"
  (if_then_else (match_code "const_int")
    (match_test "(INTVAL (op) < 64 && INTVAL (op) >= -64) ||
    ((INTVAL(op) & (INTVAL(op)+1))==0)||((~INTVAL(op) & (~INTVAL(op)+1))==0) ||
    (INTVAL (op) <= 2147483647LL && INTVAL(op) >= -2147483648LL)")
    (match_operand 0 "register_operand")))

;; Return 1 if OP is a constant or any register.
(define_predicate "reg_or_cint_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_int_operand")))

(define_predicate "reg_or_zero_operand"
  (if_then_else (match_code "const_int,const_double")
    (match_test "op == CONST0_RTX (GET_MODE (op))")
    (match_operand 0 "register_operand")))

(define_predicate "const_n_operand"
  (and (match_code "const_int")
    (match_test "INTVAL (op) <= 2147483647LL && INTVAL(op) >= -2147483648LL")))

(define_predicate "const_zero_operand"
       (and (match_code "const_int,const_double")
            (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_predicate "sym_ref_mem_operand"
 	(match_code "mem")
{
 rtx t1 = XEXP(op, 0);
 if(GET_CODE(t1) == SYMBOL_REF)
    return 1;
  return 0;
})


;; Return 1 if OP is a valid VE comparison operator for "cmp" style
;; instructions.
(define_predicate "ve_signed_comparison_operator"
  (match_code "eq,ne,lt,le,ge,gt"))
(define_predicate "ve_ordered_comparison_operator"
  (match_code "eq,ltgt,lt,le,ge,gt,ordered"))
(define_predicate "ve_unordered_comparison_operator"
  (match_code "uneq,ne,unlt,unle,unge,ungt,unordered"))

(define_predicate "const_arith_operand"
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (INTVAL (op))")))

;; Returns 1 if OP is a symbolic operand, i.e. a symbol_ref or a label_ref,
;; possibly with an offset.

(define_predicate "symbolic_operand"
   (match_code "symbol_ref,label_ref"))

;; Return true if OP is a symbolic operand for the TLS Global Dynamic model.

(define_predicate "tgd_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "!TARGET_TLS_ASIS || SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_GLOBAL_DYNAMIC")))

;; Return true if OP is a symbolic operand for the TLS Local Dynamic model.

(define_predicate "tld_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_DYNAMIC")))

;; Return true if OP is a symbolic operand for the TLS Initial Exec model.

(define_predicate "tie_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_INITIAL_EXEC")))

;; Return true if OP is a symbolic operand for the TLS Local Exec model.

(define_predicate "tle_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_EXEC")))


