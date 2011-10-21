/* $Id: headers.h,v 1.2 1999/02/01 01:08:46 garrigue Exp $ */

#ifdef __cplusplus
extern "C" {
#endif
#pragma once
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/custom.h>
extern void invalid_argument (const char * msg) Noreturn;

#include "ml_vtable.h"
#include "qwrappers.h"



#ifdef __cplusplus
}
#endif
