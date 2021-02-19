/* Copyright (C) 2016,2017 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */
/* Changes by NEC Corporation for the VE port, 2017-2021 */

typedef unsigned int UTItype __attribute__ ((mode (TI)));
typedef int TItype __attribute__ ((mode (TI)));

TItype __divti3 (TItype u, TItype v); 
UTItype __udivti3 (UTItype u, UTItype v); 
TItype __modti3 (TItype u, TItype v); 
UTItype __umodti3 (UTItype u, UTItype v); 
UTItype __udivmodti4 (UTItype u, UTItype v, UTItype *w);

TItype __modti3 (TItype u, TItype v) 
{
	return u - __divti3(u,v)*v;
}

UTItype __umodti3 (UTItype u, UTItype v) 
{
	return u - __udivti3(u,v)*v;
}

UTItype __udivmodti4 (UTItype u, UTItype v, UTItype *w)
{
	UTItype x;
	x = __udivti3(u,v);
	if (w) *w = u - x*v;
	return x;
}
