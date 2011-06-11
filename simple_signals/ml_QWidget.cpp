#include <qboxlayout.h>
#include <stdio.h>
#include "headers.h"
#include <qwidget.h>
#include <qdebug.h>
extern "C" {
  /*
  value ml_qvboxlayout_addWidget(value lay,value item) {
    CAMLparam2(lay,item);
    QVBoxLayout* l = QVBoxLayout_val(lay);
    l->addWidget(QWidget_val(item));
    CAMLreturn(Val_unit);
  }
  */
  value ml_createWidget(value parent) {
    CAMLparam1(parent);
    CAMLreturn((value) (new QWidget()));
  }
} // extern "C"
