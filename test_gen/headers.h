#pragma once

#include "ml_headers.h"
#define Some_val(v) Field(v,0)

#define CAMLOBJ_PROPERTY "_camlobj"
#define GET_CAML_OBJECT(cppobj,varname) \
QVariant camlobj_variant = cppobj->property(CAMLOBJ_PROPERTY);\
 qlonglong varname = (camlobj_variant.isValid()) ? camlobj_variant.toLongLong() : 0;

