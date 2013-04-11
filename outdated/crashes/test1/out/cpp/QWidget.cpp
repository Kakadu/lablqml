#include <Qt/QtOpenGL>
#include "headers.h"
extern "C" {
#include "enum_headers.h"
// constructor QWidget(QWidget* parent  = 0,Qt::WindowFlags f  = 0)
//argnames = (arg0 arg1)
value native_pub_createeee_QWidget_QWidget_Qt_WindowFlags(value arg0,value arg1) {
  CAMLparam2(arg0,arg1);
  CAMLlocal1(_ans);
  QWidget* _arg0 = (arg0==Val_none) ? NULL : ((QWidget* )(Some_val(arg0)));
  Qt::WindowFlags _arg1 = enum_of_caml_Qt_WindowFlags(arg1);
  QWidget* ans = new QWidget(_arg0, _arg1);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((QWidget **) &Field(_ans, 0))) = ans;
  CAMLreturn(_ans);
}

// method void show()
//argnames = (self)
value native_pub_QWidget_show(value self) {
  CAMLparam1(self);
  QWidget *_self = dynamic_cast<QWidget *>((QObject*)self);
  _self -> show();
  CAMLreturn(Val_unit);
}

}  // extern "C"
