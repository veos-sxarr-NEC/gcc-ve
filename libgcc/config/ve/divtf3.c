/* Copyright (C) 2017-2021 NEC Corporation

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <math.h>
long double __divtf3(long double a, long double b);
long double __divtf3(long double a, long double b) {
        long double t,c, cu, cl, bu, bl;
        double bh, th;
        long signa, signb, signab, expa, expb;
        expa = *((long *)&a+1) & 0xffff000000000000;
        expb = *((long *)&b+1) & 0xffff000000000000;
        signa = expa & 0x8000000000000000;
        signb = expb & 0x8000000000000000;
        expa = expa ^ signa;
        expb = expb ^ signb;
        signab = signa ^ signb;
        if (__builtin_isnanl(a) || __builtin_isnanl(b)) 
		return __builtin_nanl("");
        if (__builtin_isinfl(b)) {
                if (__builtin_isinf(a)) return __builtin_nanl("");
                else return (signab >=0)? 0.0L : -0.0L;
	}
        if (__builtin_isinfl(a)) {
                return (signab >=0)? __builtin_infl() : -__builtin_infl();
        }
        if (b == 0) {
                volatile double x = 1.0/0.0;
                if (a == 0) return __builtin_nanl("");
                return (signab >=0)? __builtin_infl() : -__builtin_infl();
        }
        if (a == 0) {
                return (signab >=0)? 0.0L : -0.0L;
        }
        *((long *)&a+1) = (*((long *)&a +1) & 0x0000ffffffffffff) 
                         + 0x3fff000000000000;
        *((long *)&b+1) = (*((long *)&b +1) & 0x0000ffffffffffff) 
                         + 0x3fff000000000000;
        bh = b;
        th = 1.0 / bh;
        t = th;
        t = t * (2.0L - b * t);
        c = a * t;
        cu = c;
        *(long *)&cu = *(long *)&cu & 0xfe00000000000000;
        cl = c - cu;
        bu = b;
        *(long *)&bu = (*(long *)&bu) & 0xfe00000000000000;
        bl = b - bu;
        t = a - cu * bu - cu * bl - cl * bu - cl * bl;
        th = t;
        th = th / bh;
        c = c + th;
        *((long *)&c+1) = *((long *)&c+1) + (expa - expb);
        if(expa >= expb) {
                if ((*((long *)&c+1) & 0x8000000000000000L) ||
                    ((*((long *)&c+1) & 0xffff000000000000L)
                     == 0x7fff000000000000L)) {
                        return (signab >=0)? __builtin_infl() : -__builtin_infl();
                }
        }
        if(expa <= expb) {
                if ((*((long *)&c+1) & 0x8000000000000000L) ||
                    ((*((long *)&c+1) & 0xffff000000000000L)
                     == 0x0000000000000000L)) {
                        return (signab >=0)? 0.0L : -0.0L;
                }
        }

        if (signab < 0) c = -c;
        return c;
}
