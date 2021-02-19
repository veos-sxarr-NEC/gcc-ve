;; Machine description for VE architecture.
;; Copyright (C) 2009-2016 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Changes by NEC Corporation for the VE port, 2017-2021

;; Register constraints.
;(define_register_constraint "f" "S_REGS"
;  "Single precision floating point registers. Same as r.")

(define_register_constraint "U" "QS_REGS"
  "Upper quadruple floating point registers, that is, even number registers.")

;;;; Memory constraints
(define_memory_constraint "a"
 "Memory address. Replaces 'm'"
 (match_code "mem"))

;; Integer constant constraints.
(define_constraint "I"
  "Integer constant in the range -64 @dots{} 63"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -64, 63)")))

(define_constraint "J"
  "Integer constant in the range 0 @dots{} 31"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 31)")))

(define_constraint "K"
  "Integer constant in the range 0 @dots{} 63"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 63)")))

(define_constraint "L"
  "Integer constant in the range 0 @dots{} 127"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 127)")))

(define_constraint "M"
  "Integer constant of 64 bit M format"
  (and (match_code "const_int")
       (match_test "(ival & (ival+1)) == 0 ||
                    (~ival & (~ival +1)) ==0")))

(define_constraint "N"
  "Integer constant of 32 bit"
  (and (match_code "const_int")
       (match_test "ival >= -2147483647LL-1 
                 && ival <= 2147483647LL")))

(define_constraint "O"
  "Integer constant of signed 32 bit M format"
  (and (match_code "const_int")
       (match_test "(ival >= -2147483647LL-1 && ival <= 2147483647LL) 
           && (( (ival & (ival + 1)) == 0)      
              || (( ( 0xffffffffLL^(0xffffffffLL&ival)) 
                  & ((0xffffffffLL^(0xffffffffLL&ival)) +1) ) == 0))")))  

(define_constraint "P"
  "Integer constant of unsigned 32 bit M format"
  (and (match_code "const_int")
       (match_test "ival == 0 || ival == -1 || ival == 0xffffffff ||
        ((ival >= 0 && ival <= 2147483647LL) 
           &&  ( (ival & (ival + 1)) == 0)) ")))  


;; Floating-point constant constraints.
(define_constraint "G"
  "The floating point zero constant"
  (and (match_code "const_double")
       (match_test "GET_MODE_CLASS (mode) == MODE_FLOAT
                    && op == CONST0_RTX (mode)")))

(define_constraint "H"
  "Any floating point constant"
  (and (match_code "const_double")
       (match_test "1")))

