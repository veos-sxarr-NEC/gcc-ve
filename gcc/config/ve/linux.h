/* This file is part of GCC.

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

/* Definitions for NEC VE running Linux-based GNU systems with ELF format.
   Changes by NEC Corporation for the VE port, 2018-2021 */

#undef LIB_SPEC
#define LIB_SPEC \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared:%{profile:-lc_p}%{!profile:-lc}}"

#ifdef ENABLE_SHARED_LIBGCC
#define SHARED_LIBGCC_SPEC " \
 %{static|static-libgcc:-lgcc -lgcc_eh} \
 %{!static: \
   %{!static-libgcc: \
     %{!shared: \
       %{!shared-libgcc:-lgcc -lgcc_eh} \
       %{shared-libgcc:-lgcc_s -lgcc} \
      } \
     %{shared:-lgcc_s -lgcc} \
    } \
  } "
#else
#define SHARED_LIBGCC_SPEC " -lgcc "
#endif

#undef REAL_LIBGCC_SPEC
#define REAL_LIBGCC_SPEC SHARED_LIBGCC_SPEC

#undef  STARTFILE_SPEC
#if defined HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#endif

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

/* At the officeal release, the install path is changed to '/opt/nec/ve'. */
#define GLIBC_DYNAMIC_LINKER "/opt/nec/ve/lib/ld-linux-ve.so.1"

#undef MUSL_DYNAMIC_LINKER
#define MUSL_DYNAMIC_LINKER "/opt/nec/ve/musl/lib/ld-musl-ve.so.1"

#if DEFAULT_LIBC == LIBC_MUSL
#define GNU_USER_DYNAMIC_LINKER MUSL_DYNAMIC_LINKER
#else
#define GNU_USER_DYNAMIC_LINKER GLIBC_DYNAMIC_LINKER
#endif

#undef LINK_SPEC
#define LINK_SPEC "\
  %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker " GNU_USER_DYNAMIC_LINKER "} \
    %{static:-static}}"
