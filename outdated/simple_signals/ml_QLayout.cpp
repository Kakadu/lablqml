#include <qboxlayout.h>
#include <stdio.h>
#include "headers.h"
#include <qwidget.h>
#include <qdebug.h>
#define QVBoxLayout_val(obj) ((QVBoxLayout*)(obj))
extern "C" {
  value ml_qvboxlayout_addWidget(value lay,value item) {
    CAMLparam2(lay,item);
    QVBoxLayout* l = QVBoxLayout_val(lay);
    l->addWidget(QWidget_val(item));
    CAMLreturn(Val_unit);
  }
  value ml_createVBoxLayout(value parent) {
    CAMLparam1(parent);
    CAMLlocal1(ans);
    if (parent == Val_none)
      ans = (value) (new QVBoxLayout());
    else
      ans = (value) (new QVBoxLayout(QWidget_val(Some_val(parent))));
    CAMLreturn(ans);
  }
} // extern "C"
