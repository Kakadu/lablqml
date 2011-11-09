#pragma once
#include <QtCore/QObject>

#include "ml_headers.h"
#define Some_val(v) Field(v,0)

#define CAMLOBJ_PROPERTY "_camlobj"
#define GET_CAML_OBJECT(cppobj,varname) \
QVariant camlobj_variant = cppobj->property(CAMLOBJ_PROPERTY);\
varname = (camlobj_variant.isValid()) ? camlobj_variant.toLongLong() : 0;

value camlObj(QObject*);


#define Val_QApplication(v)       (*((QApplication **)   &Field(v, 0)))
#define Val_QWidget(v)            (*((QWidget **)        &Field(v, 0)))
#define Val_QObject(v)            (*((QObject **)        &Field(v, 0)))
#define Val_QWidget_twin(v)       (*((QWidget_twin **)   &Field(v, 0)))
#define Val_QKeyEvent(v)          (*((QKeyEvent **)      &Field(v, 0)))

#define QApplication_val(v)       ((QApplication *)   Field(v,0) )
#define QWidget_val(v)            ((QWidget *)        Field(v,0) )
#define QObject_val(v)            ((QObject *)        Field(v,0) )
#define QWidget_twin_val(v)       ((QWidget_twin *)   Field(v,0) )
#define QKeyEvent_val(v)          ((QKeyEvent *)      Field(v,0) )

#define setAbstrClass(res,clas,val)\
do {   res = caml_alloc_small(1, Abstract_tag);\
  (*((clas **) &Field(res, 0))) = val; } while (false)
