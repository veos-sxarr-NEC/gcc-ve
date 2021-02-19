/* Definitions for option handling for VE.
   Copyright (C) 2010-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */
/* Changes by NEC Corporation for the VE port, 2017-2021 */

#ifndef VE_OPTS_H
#define VE_OPTS_H

/* Enumerates the setting of the -margmem option. */
enum ve_argmem_setting {
   ARGMEM_FORCE,
   ARGMEM_SAFE,
   ARGMEM_OPT
}; 

#endif
