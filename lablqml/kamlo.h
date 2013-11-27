#ifndef KAMLO_H
#define KAMLO_H


extern "C" {
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
}
#define CAMLOBJ_PROPERTY "_camlobj"

#define GET_CAML_OBJECT(cppobj,varname) \
   QVariant camlobj_variant = cppobj->property(CAMLOBJ_PROPERTY);\
 qlonglong varname = (camlobj_variant.isValid()) ? camlobj_variant.toLongLong() : 0;

#define Val_none Val_int(0)

#endif // KAMLO_H

