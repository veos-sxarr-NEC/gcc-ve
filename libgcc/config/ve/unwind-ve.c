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

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "unwind.h"
#include "gthr.h"

_Unwind_Reason_Code LIBGCC2_UNWIND_ATTRIBUTE
_Unwind_SjLj_RaiseException(struct _Unwind_Exception *exc);

_Unwind_Reason_Code LIBGCC2_UNWIND_ATTRIBUTE
_Unwind_SjLj_ForcedUnwind (struct _Unwind_Exception *exc,
                      _Unwind_Stop_Fn stop, void * stop_argument);

_Unwind_Reason_Code LIBGCC2_UNWIND_ATTRIBUTE
_Unwind_SjLj_Resume_or_Rethrow (struct _Unwind_Exception *exc);

void LIBGCC2_UNWIND_ATTRIBUTE
_Unwind_Resume (struct _Unwind_Exception *exc);


_Unwind_Reason_Code LIBGCC2_UNWIND_ATTRIBUTE
_Unwind_RaiseException(struct _Unwind_Exception *exc)
{
    return _Unwind_SjLj_RaiseException(exc);
}

_Unwind_Reason_Code LIBGCC2_UNWIND_ATTRIBUTE
_Unwind_ForcedUnwind (struct _Unwind_Exception *exc,
                      _Unwind_Stop_Fn stop, void * stop_argument)
{
    return _Unwind_SjLj_ForcedUnwind (exc,stop,stop_argument);
}

void LIBGCC2_UNWIND_ATTRIBUTE
_Unwind_Resume (struct _Unwind_Exception *exc)
{
    _Unwind_SjLj_Resume (exc);
}

_Unwind_Reason_Code LIBGCC2_UNWIND_ATTRIBUTE
_Unwind_Resume_or_Rethrow (struct _Unwind_Exception *exc)
{
    return _Unwind_SjLj_Resume_or_Rethrow (exc);
}

