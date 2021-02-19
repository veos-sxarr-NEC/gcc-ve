/*   libgcc routines for the VE.
     Copyright (C) 2014,2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */
/* Changes by NEC Corporation for the VE port, 2017-2021 */

        .macro FUNC_START names:vararg
        .text
	.balign	16
	.irp name, \names
        .globl	__\name
        .type	__\name,@function
	.endr
	.endm

        .macro FUNC_ENTRY name
__\name:
        .endm

        .macro FUNC_COMPAT namec, name
	.symver	\namec, \name@GCC_1.0
        .endm

        .macro RETURN
	b.l.t	(,%lr)
        .endm

#ifdef L_unordsf2
FUNC_START unordsf2
FUNC_ENTRY unordsf2
	brnan.s	%s0,%s1,.L1
	or	%s0,0,(0)1
	RETURN
.L1:
	or	%s0,1,(0)1
	RETURN
#endif

#ifdef L_unorddf2
FUNC_START unorddf2
FUNC_ENTRY unorddf2
	brnan.d	%s0,%s1,.L2
	or	%s0,0,(0)1
	RETURN
.L2:
	or	%s0,1,(0)1
	RETURN
#endif

#ifdef L_unordtf2 
FUNC_START unordtf2
FUNC_ENTRY unordtf2
	srl	%s4,(15)1,1
	and	%s5,%s0,(1)0
	brgt.l	%s5,%s4,.L5
	brle.l	%s5,%s4,.L3
	brne.l	%s1,0,.L5
.L3:
	and	%s5,%s2,(1)0
	brgt.l	%s5,%s4,.L5
	brlt.l	%s5,%s4,.L4
	brne.l	%s3,0,.L5
.L4:
	or	%s0,0,(0)1
	RETURN
.L5:
	or	%s0,1,(0)1
	RETURN
#endif

#__int128 dfti2(double x) {
#        double t,x2;
#        long u,l,k;
#        __int128 ul;
#        t = pow(2.0,127);
#        .if (fabs(x) >= t) {
#                ul = 1;
#                ul = ul << 127;
#                return ul;
#        }
#        t = pow(2.0,-96);
#        k = (long) (x * t);
#        t = pow(2.0,96);
#        x2 = x - ((double)k) * t;
#        u = k << 32;
#
#        t = pow(2.0,-64);
#        k = (long) (x2 * t);
#        t = pow(2.0,64);
#        x2 = x2 - ((double)k) * t;
#        u = u + k ;
#        .if (x2 <0) u--;
#
#        t = pow(2.0,-32);
#        k = (long) (x2 * t);
#        t = pow(2.0,32);
#        x2 = x2 - ((double)k) * t;
#        l = k << 32 ;
#
#        k = (long) x2;
#        l = l + k;
#
#        ul = u;
#        ul = ul << 64;
#        ul = ul + (unsigned long)l ;
#        return ul;
#}
# register %s0:x,u %s1:l %s2:t %s3:x2, %s4:k, %s5:tmp 
        .macro  FIX2TI_CORE
	lea.sl	%s2,0x39f00000 # (double)2^-96
	fmul.d	%s5,%s0,%s2
	cvt.l.d.rz	%s4,%s5
	lea.sl	%s2,0x45f00000 # (double)2^96
	cvt.d.l	%s5,%s4
	fmul.d	%s5,%s5,%s2
	fsub.d	%s3,%s0,%s5
	sll	%s0,%s4,32
#
	lea.sl	%s2,0x3bf00000 # (double)2^-64
	fmul.d	%s5,%s3,%s2
	cvt.l.d.rz	%s4,%s5
	lea.sl	%s2,0x43f00000 # (double)2^64
	cvt.d.l	%s5,%s4
	fmul.d	%s5,%s5,%s2
	fsub.d	%s3,%s3,%s5
	addu.l	%s0,%s0,%s4
	addu.l	%s5,-1,%s0
	cmov.l.lt	%s0,%s5,%s3
#
	lea.sl	%s2,0x3df00000 # (double)2^-32
	fmul.d	%s5,%s3,%s2
	cvt.l.d.rz	%s4,%s5
	lea.sl	%s2,0x41f00000 # (double)2^32
	cvt.d.l	%s5,%s4
	fmul.d	%s5,%s5,%s2
	fsub.d	%s3,%s3,%s5
	sll	%s1,%s4,32
#
	cvt.l.d.rz	%s4,%s3
	addu.l	%s1,%s1,%s4
	RETURN
        .endm

#ifdef L_fixsfti
FUNC_START fixsfti
FUNC_ENTRY fixsfti
	cvt.d.s	%s0,%s0
	and	%s5,%s0,(1)0
	lea.sl	%s2,0x47e00000 # (double)2^127
	brlt.d	%s5,%s2,.L20
	or	%s0,0,(1)1
	or	%s1,0,(0)1
	RETURN
.L20:
        FIX2TI_CORE
#endif

#ifdef L_fixdfti
FUNC_START fixdfti
FUNC_ENTRY fixdfti
	and	%s5,%s0,(1)0
	lea.sl	%s2,0x47e00000 # (double)2^127
	brlt.d	%s5,%s2,.L20
	or	%s0,0,(1)1
	or	%s1,0,(0)1
	RETURN
.L20:
        FIX2TI_CORE
#endif

#ifdef L_fixunssfti
FUNC_START fixunssfti
FUNC_ENTRY fixunssfti
	cvt.d.s	%s0,%s0
	lea.sl	%s2,0x37f00000 # (double)2^-128
	fmul.d	%s5,%s0,%s2
	cvt.l.d.rz	%s4,%s5
	lea.sl	%s2,0x47f00000 # (double)2^128
	cvt.d.l	%s5,%s4
	fmul.d	%s5,%s5,%s2
	fsub.d	%s0,%s0,%s5
        FIX2TI_CORE
#endif

#ifdef L_fixunsdfti
FUNC_START fixunsdfti
FUNC_ENTRY fixunsdfti
	lea.sl	%s2,0x37f00000 # (double)2^-128
	fmul.d	%s5,%s0,%s2
	cvt.l.d.rz	%s4,%s5
	lea.sl	%s2,0x47f00000 # (double)2^128
	cvt.d.l	%s5,%s4
	fmul.d	%s5,%s5,%s2
	fsub.d	%s0,%s0,%s5
        FIX2TI_CORE
#endif
#
        .macro  FIXTFTI_CORE
# register %s0:xu,u %s1:xl,l %s2:x2u %s3:x2l, %4:tmpu, %s5:tmpl, %s6:tu, %s7:tl 
#          %s45:k
	lea.sl	%s6,0x3f9f0000 # (long double)2^-96
	or	%s7,0,(0)1
	fmul.q	%s4,%s0,%s6
	and	%s5,%s5,(4)1	# cut >52 bit to avoid rounding
	cvt.d.q	%s4,%s4
	cvt.l.d.rz	%s45,%s4
	cvt.d.l	%s4,%s45
	cvt.q.d	%s4,%s4
	lea.sl	%s6,0x405f0000 # (long double)2^96
	or	%s7,0,(0)1
	fmul.q	%s4,%s4,%s6
	fsub.q	%s2,%s0,%s4
	sll	%s0,%s45,32
#
	lea.sl	%s6,0x3fbf0000 # (long double)2^-64
	or	%s7,0,(0)1
	fmul.q	%s4,%s2,%s6
	and	%s5,%s5,(4)1	# cut >52 bit to avoid rounding
	cvt.d.q	%s4,%s4
	cvt.l.d.rz	%s45,%s4
	cvt.d.l	%s4,%s45
	cvt.q.d	%s4,%s4
	lea.sl	%s6,0x403f0000 # (long double)2^64
	or	%s7,0,(0)1
	fmul.q	%s4,%s4,%s6
	fsub.q	%s2,%s2,%s4
	addu.l	%s0,%s0,%s45
	addu.l	%s4,-1,%s0
	cmov.l.lt	%s0,%s4,%s2
#
	lea.sl	%s6,0x3fdf0000 # (long double)2^-32
	or	%s7,0,(0)1
	fmul.q	%s4,%s2,%s6
	and	%s5,%s5,(4)1	# cut >52 bit to avoid rounding
	cvt.d.q	%s4,%s4
	cvt.l.d.rz	%s45,%s4
	cvt.d.l	%s4,%s45
	cvt.q.d	%s4,%s4
	lea.sl	%s6,0x401f0000 # (long double)2^32
	or	%s7,0,(0)1
	fmul.q	%s4,%s4,%s6
	fsub.q	%s2,%s2,%s4
	sll	%s1,%s45,32
#
	and	%s3,%s3,(4)1	# cut >52 bit to avoid rounding
	cvt.d.q	%s4,%s2
	cvt.l.d.rz	%s45,%s4
	addu.l	%s1,%s1,%s45
        RETURN
        .endm

#ifdef L_fixtfti
FUNC_START fixtfti
FUNC_ENTRY fixtfti
# register %s0:xu,u %s1:xl,l %s2:x2u %s3:x2l, %4:tmpu, %s5:tmpl, %s6:tu, %s7:tl 
#          %s45:k
	and	%s4,%s0,(1)0
	lea.sl	%s6,0x407e0000 # (long double)2^127
	brlt.l	%s4,%s6,.L22
	or	%s0,0,(1)1
	or	%s1,0,(0)1
	RETURN
.L22:
        FIXTFTI_CORE
#endif

#ifdef L_fixunstfti
FUNC_START fixunstfti
FUNC_ENTRY fixunstfti
	lea.sl	%s6,0x3f7f0000 # (long double)2^-128
	or	%s7,0,(0)1
	fmul.q	%s4,%s0,%s6
	and	%s5,%s5,(4)1	# cut >52 bit to avoid rounding
	cvt.d.q	%s4,%s4
	cvt.l.d.rz	%s45,%s4
	cvt.d.l	%s4,%s45
	cvt.q.d	%s4,%s4
	lea.sl	%s6,0x407f0000 # (long double)2^128
	or	%s7,0,(0)1
	fmul.q	%s4,%s4,%s6
	fsub.q	%s0,%s0,%s4
        FIXTFTI_CORE
#endif

#
#__int128 mul(__int128 x, __int128 y) {
#        __int128 z;
#        unsigned long xu,xl,yu,yl,zu,zl;
#        unsigned long xl1,xl2,yl1,yl2;
#        unsigned long t;
#
#        unsigned long max = 0xffffffffffffffff;
#        unsigned long tt;
#        xu = x >> 64;
#        xl = x & 0xffffffffffffffff;
#        yu = y >> 64;
#        yl = y & 0xffffffffffffffff;
#        zu = xu * yl;
#        t = xl * yu;
#        zu = zu + t;
#
#        xl1 = xl >> 32;
#        xl2 = xl & 0xffffffff;
#        yl1 = yl >> 32;
#        yl2 = yl & 0xffffffff;
#
#        t = xl1 * yl1;
#        zu = zu + t;
#
#        t = xl2 * yl2;
#        zl = t;
#
#        t = xl1 * yl2;
#        zu = zu + (t >> 32);
#        t = t << 32;
#        tt = max - t;
#        .if (tt < zl) zu++;
#        zl = zl + t;
#
#        t = xl2 * yl1;
#        zu = zu + (t >> 32);
#        t = t << 32;
#        tt = max - t;
#        .if (tt < zl) zu++;
#        zl = zl + t ;
#
#        z = zu;
#        z = z << 64;
#        z = z + zl;
#        return z;
#}
#register %s0:xu,zu %s1:xl,zl %s2:yu,yl1 %s3:yl,yl2
#         %s4:xl1,tmp %s5:xl2 %s6:t %s7:tt

#ifdef L_multi3
FUNC_START multi3
FUNC_ENTRY multi3
	mulu.l	%s0,%s0,%s3
	mulu.l	%s4,%s1,%s2
	addu.l	%s0,%s0,%s4
#
	srl	%s4,%s1,32
	and	%s5,%s1,(32)0
	srl	%s2,%s3,32
	and	%s3,%s3,(32)0
#
	mulu.l	%s6,%s4,%s2
	addu.l	%s0,%s0,%s6
	mulu.l	%s1,%s5,%s3
#
	mulu.l	%s6,%s4,%s3
	srl	%s4,%s6,32
	addu.l	%s0,%s0,%s4
	sll	%s6,%s6,32
	subu.l	%s7,-1,%s6
	cmpu.l	%s7,%s7,%s1
	addu.l	%s4,1,%s0
	cmov.l.lt	%s0,%s4,%s7
	addu.l	%s1,%s1,%s6
#
	mulu.l	%s6,%s5,%s2
	srl	%s4,%s6,32
	addu.l	%s0,%s0,%s4
	sll	%s6,%s6,32
	subu.l	%s7,-1,%s6
	cmpu.l	%s7,%s7,%s1
	addu.l	%s4,1,%s0
	cmov.l.lt	%s0,%s4,%s7
	addu.l	%s1,%s1,%s6
	RETURN
#endif

#__int128 div3(__int128 x, __int128 y, int uns) {
#        __int128 z;     /* z(return) = x / y */
#        unsigned long xu,xl,yu,yl,zh,zu,zl,z0;
#        unsigned long y1,y2,y3,y4,yy;
#        unsigned long tu,tl,t;
#        unsigned long sign;
#        unsigned long maxint;
#        int shift=128;
#        int k,i;
#
#        maxint = 0xffffffffffffffff;
#
#        xu = x >> 64;
#        xl = x & 0xffffffffffffffff;
#        yu = y >> 64;
#        yl = y & 0xffffffffffffffff;
#
#        .if (uns) {
#                sign = 0;
#        } else {
#                sign = xu ^ yu;
#/*abs x*/
#                .if (((signed long)xu) < 0) {
#                        xu = xu ^ 0xffffffffffffffff;
#                        xl = -(signed long)xl;
#                        .if (xl == 0) xu++;
#                }
#/*abs y*/
#                .if (((signed long)yu) < 0) {
#                        yu = yu ^ 0xffffffffffffffff;
#                        yl = -(signed long)yl;
#                        .if (yl == 0) yu++;
#                }
#        }
#
#        .if (yu == 0 && yl == 0) {
#                xu / yu; /* generate zero-div exception */
#                return 0;
#        }
#
#        .if ((yu > xu) || ((yu == xu ) && (yl > xl))) {
#                z = 0;
#                return z;
#        }
#
#/* normalize y */
#        .if (yu == 0) {
#                shift -= 64;
#                yu = yl; yl = 0;
#        }
#        k = __builtin_clzl(yu);
#        .if (k != 0) {
#                yu = yu << k;
#                t = yl >> (64 -k);
#                yu = yu | t;
#                yl = yl << k;
#                shift -= k;
#        }
#
#/* normalize x */
#        .if (xu == 0) {
#                shift += 64;
#                xu = xl; xl = 0;
#        }
#        k = __builtin_clzl(xu);
#        .if (k != 0) {
#                xu = xu << k;
#                t = xl >> (64 -k);
#                xu = xu | t;
#                xl = xl << k;
#                shift += k;
#        }
#/* break y into (32bit) y1,y2,y3,y4 */
#        y1 = yu >> 32;
#        y2 = yu & 0xffffffff;
#        y3 = yl >> 32;
#        y4 = yl & 0xffffffff;
#        yy = y2 << 32 | y3;
#
#        zh = 0;
#        zu = 0;
#        zl = 0;
#
#/* if x >= y then { x = x -y; zh = 1;} */
#        .if ((xu > yu) || ((xu == yu) && (xl >= yl))) {
#                .if (xl < yl) xu --;
#                xu = xu - yu;
#                xl = xl - yl;
#                zl = 1;
#        }
#
#        for (i=shift/32; i < 4; i++) {
#                z0 = xu / y1;
#
#                tu = z0 * y1;
#
#                t = z0 * y2;
#                tl = t << 32;
#                t = t >> 32;
#                tu = tu + t;
#
#                t = z0 * y3;
#                .if ((maxint - t) < tl) tu++;
#                tl = tl + t;
#
#                t = (z0 * y4) >> 32;
#                .if ((maxint - t) < tl) tu++;
#                tl = tl + t;
#
#                while ((tu > xu) || ((tu == xu) && (tl > xl))) {
#                        /* t = t - (y >>32) */
#                        .if (tl < yy) tu--;
#                        tu = tu -  y1;
#                        tl = tl - yy;
#                        z0--;
#                }
#                /* x = x - t */
#
#                .if (xl < tl) xu--;
#                xu = xu - tu;
#                xl = xl - tl;
#
#/* shift x = x << 32; z = z << 32 */
#                xu = xu << 32;
#                t = xl >> 32;
#                xu = xu | t;
#                xl = xl << 32;
#
#                zh = zh <<32;
#                t = zu >> 32;
#                zh = zh | t;
#                zu = zu << 32;
#                t = zl >> 32;
#                zu = zu | t;
#                zl = zl <<32;
#                zl = zl | z0;
#        }
#
#        shift = shift % 32;
#        .if (shift > 0) {
#                zl = zl >> shift;
#                t = zu << (64 - shift);
#                zl = zl | t;
#                zu = zu >> shift;
#                t = zh << (64 - shift);
#                zu = zu | t;
#                zh = zh >> shift;
#        }
#
#/* set sign */
#        .if ((signed long)sign < 0) {
#                zu = zu ^ 0xffffffffffffffff;
#                zl = -(signed long)zl;
#                .if (zl == 0) zu++;
#        }
#        z = zu;
#        z = z << 64;
#        z = z | zl;
#        return z;
#}
#
# registers 
# %s0:xu, %s1:xl, %s2:yu, %s3:yl, %s4:zh, %s5:zu, %s6:zl, %s7:z0
# %s45:y1, %s46:y2, %s47:y3, %s48:y4, %s49:yy, %s50:tu, %s51:tl, %s52:t
# %s53:sign, %s54:shift, %s55:k/i, %s56:temp, %57:temp2
        .macro  DIVTI3_CORE
	brne.l	%s2,0,.L103	# if (y == 0) 
	brne.l	%s3,0,.L103
	divs.l	%s56,%s0,%s2	# gen zero-div exception
.L102:
	or	%s0,0,(0)1
	or	%s1,0,(0)1
	RETURN
.L103:
	cmpu.l	%s56,%s2,%s0	# if (y >x) return 0;
	brgt.l	%s56,0,.L102
	brne.l	%s56,0,.L104
	cmpu.l	%s56,%s3,%s1
	brgt.l	%s56,0,.L102
.L104:
	adds.l	%s54,1,(57)0	# shift = 128;
	brne.l	%s2,0,.L105	# if (yu == 0) y <<= 64; shift -=64;
	adds.l	%s54,%s54,(58)1	
	or	%s2,0,%s3
	or	%s3,0,(0)1
.L105:
	ldz	%s55,%s2	# y <<= (k = ldz(y));
	sld	%s2,%s3,%s55
	sll	%s3,%s3,%s55
	subs.l	%s54,%s54,%s55	# shift -= k;
	brne.l	%s0,0,.L106	# if (xu == 0) x <<=64; shift +=64;
	subs.l	%s54,%s54,(58)1
	or	%s0,0,%s1
	or	%s1,0,(0)1
.L106:
	ldz	%s55,%s0	# x <<= (k = ldz(x))
	sld	%s0,%s1,%s55
	sll	%s1,%s1,%s55
	adds.l	%s54,%s54,%s55	# shift += k;
	srl	%s45,%s2,32	# break y into 32bit y1,y2,y3,y4
	and	%s46,%s2,(32)0
	srl	%s47,%s3,32
	and	%s48,%s3,(32)0
	sll	%s49,%s46,32	# yy = y2 << 32 | y3;
	or	%s49,%s49,%s47
#
	or	%s4,0,(0)1	# zh = zu = zl = 0;
	or	%s5,0,(0)1
	or	%s6,0,(0)1
	cmpu.l	%s56,%s0,%s2	# if (x>=y) x -= y; zh =1;
	brgt.l	%s56,0,.L107
	brne.l	%s56,0,.L108
	cmpu.l	%s56,%s1,%s3
	brlt.l	%s56,0,.L108
.L107:
	addu.l	%s57,-1,%s0
	cmpu.l	%s56,%s1,%s3
	cmov.l.lt	%s0,%s57,%s56
	subu.l	%s0,%s0,%s2
	subu.l	%s1,%s1,%s3
	or	%s6,1,(0)1
.L108:
	srl	%s55,%s54,5	# for(i=shift/32; i<4; i++) { 
	brle.l	4,%s55,.L113
.L109:
	divu.l	%s7,%s0,%s45	# z0 = xu / y1;
	mulu.l	%s50,%s7,%s45	# tu = z0 * y1;
	mulu.l	%s52,%s7,%s46	# t = z0 * y2;
	sll	%s51,%s52,32	# tl = t << 32;
	srl	%s52,%s52,32	# t = t >> 32;
	addu.l	%s50,%s50,%s52	# tu = tu + t
#
	mulu.l	%s52,%s7,%s47	# t = z0 * y3;
	subu.l	%s56,-1,%s52	# if ((maxint -t) < tl) tu++;
	cmpu.l	%s56,%s56,%s51
	addu.l	%s57,1,%s50
	cmov.l.lt	%s50,%s57,%s56
	addu.l	%s51,%s51,%s52	# tl = tl + t;
#
	mulu.l	%s52,%s7,%s48	# t = (z0 * y4) >> 32;
	srl	%s52,%s52,32
	subu.l	%s56,-1,%s52	# if ((maxint -t) < tl) tu++;
	cmpu.l	%s56,%s56,%s51
	addu.l	%s57,1,%s50
	cmov.l.lt	%s50,%s57,%s56
	addu.l	%s51,%s51,%s52	# tl = tl + t;
.L110:
	cmpu.l	%s56,%s50,%s0	# while (t > x) {
	brgt.l	%s56,0,.L111
	brne.l	%s56,0,.L112
	cmpu.l	%s56,%s51,%s1
	brle.l	%s56,0,.L112
.L111:
	cmpu.l	%s56,%s51,%s49	#  t = t - (y >>32)
	addu.l	%s57,-1,%s50	#   if(tl<yy) tu--;
	cmov.l.lt	%s50,%s57,%s56
	subu.l	%s50,%s50,%s45	#   tu = tu - y1;
	subu.l	%s51,%s51,%s49	#   tl = tl - yy;
	addu.l	%s7,-1,%s7	# z0 --;
	br.l.t	.L110		# } /*end while*/	
.L112:
	cmpu.l	%s56,%s1,%s51	# if (xl < tl) xu--;
	addu.l	%s57,-1,%s0
	cmov.l.lt	%s0,%s57,%s56
	subu.l	%s0,%s0,%s50	# xu = xu - tu;
	subu.l	%s1,%s1,%s51	# xl = xl - tl;
#
	sld	%s0,%s1,32	# x <<= 32;
	sll	%s1,%s1,32
	sld	%s4,%s5,32	# (zh,zu,zl) <<= 32;
	sld	%s5,%s6,32
	sll	%s6,%s6,32
	or	%s6,%s6,%s7	# zl = zl | z0;
	adds.l	%s55,1,%s55	# } /*end for*/
	brgt.l	4,%s55,.L109
.L113:
	and	%s54,%s54,(59)0	# shift %= 32;
	srd	%s6,%s5,%s54	# (zh,zu,zl) >>= shift;
	srd	%s5,%s4,%s54

	or	%s0,0,%s5
	or	%s1,0,%s6
	brge.l	%s53,0,.L114
	xor	%s0,%s0,(0)0
	subs.l	%s1,0,%s1
	addu.l	%s56,1,%s0
	cmov.l.eq	%s0,%s56,%s1
.L114:
        RETURN
        .endm
#
#ifdef L_divti3
FUNC_START divti3
FUNC_ENTRY divti3
	xor	%s53,%s0,%s2	# sign = x ^ y;
	brge.l	%s0,0,.L100	# if (x <0) x = -x; 
	xor	%s0,%s0,(0)0
	subs.l	%s1,0,%s1
	addu.l	%s56,1,%s0
	cmov.l.eq	%s0,%s56,%s1
.L100:
	brge.l	%s2,0,.L101	# if (y<0) y = -y;
	xor	%s2,%s2,(0)0
	subs.l	%s3,0,%s3
	addu.l	%s56,1,%s2
	cmov.l.eq	%s2,%s56,%s3
.L101:
        DIVTI3_CORE
#endif

#ifdef L_udivti3
FUNC_START udivti3
FUNC_ENTRY udivti3
	or	%s53,0,(0)1
        DIVTI3_CORE
#endif
#
