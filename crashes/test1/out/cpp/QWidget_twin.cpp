#include <Qt/QtOpenGL>
#include "headers.h"
#include "enum_headers.h"
#include <stdio.h>

#include "QWidget_twin.h"
extern "C" {
value native_pub_createeee_QWidget_twin_QWidget_Qt_WindowFlags(value arg0,value arg1) {
  CAMLparam2(arg0,arg1);
  QWidget* _arg0 = (arg0==Val_none) ? NULL : ((QWidget* )(Some_val(arg0)));
  Qt::WindowFlags _arg1 = enum_of_caml_Qt_WindowFlags(arg1);
  QWidget_twin *_ans = new QWidget_twin(_arg0,_arg1);
  CAMLreturn((value)_ans);
}

// method void keyPressEvent(QKeyEvent*  )
//argnames = (self arg0)
value native_prot_QWidget_keyPressEvent_QKeyEvent(value self,value arg0) {
  CAMLparam2(self,arg0);
  QWidget_twin *_self = dynamic_cast<QWidget_twin *>((QObject*)self);
  QKeyEvent* _arg0 = (QKeyEvent* ) (arg0);
  _self -> keyPressEvent(_arg0);
  CAMLreturn(Val_unit);
}


}
