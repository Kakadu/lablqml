#ifndef HEADERS_H
#define HEADERS_H
#include <QtCore/QObject>
#include <assert.h>
#include "ml_headers.h"
#define Some_val(v) Field(v,0)

#define CAMLOBJ_PROPERTY "_camlobj"
#define GET_CAML_OBJECT(cppobj,varname) \
QVariant camlobj_variant = cppobj->property(CAMLOBJ_PROPERTY);\
 qlonglong varname = (camlobj_variant.isValid()) ? camlobj_variant.toLongLong() : 0;

value camlObj(QObject*);

#define QObject_val(v)            ((QObject *)        Field(v,0) )
/*
#define setAbstrClass(res,clas,val)\
do{res=caml_alloc_small(1,Abstract_tag);\
(*((clas **) &Field(res, 0)))=val;}while(false)
*/
template <typename T>
void setAbstrClass(value &res, T *newval) {
  res = caml_alloc_small(1,Abstract_tag);
  (*((T**) &Field(res,0))) = newval;
}
#endif

