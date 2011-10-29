#include "headers.h"
#include "enums.h"
#include <caml/mlvalues.h>
#include <stdio.h>
#include <QtGui/QWidget>

class QWidget_twin : public QWidget {
Q_OBJECT
public:
  QWidget_twin(QWidget* x0) : QWidget(x0,0) {}

  void virtual keyPressEvent(QKeyEvent *ev) {
    CAMLparam0();
    CAMLlocal2(meth,camlobj);
    GET_CAML_OBJECT(this,the_caml_object);
    camlobj = (value)the_caml_object;
    printf ("inside QWidget_twin::keyPressedEvent, camlobj = %x\n", camlobj);
    meth = caml_get_public_method( camlobj, caml_hash_variant("keyPressedEvent"));
    if (meth==0)
      printf ("total fail\n");
    printf("calling callback of meth = %x\n",meth);
    caml_callback2(meth, camlobj, (value)ev);
    printf ("exit from QWidget_twin::keyPressedEvent\n");
    CAMLreturn0;
  }
};

extern "C" {

value native_pub_createeee_QWidget_twin_QWidget_Qt_WindowFlags(value arg0) {
  CAMLparam1(arg0);
  QWidget* _arg0 = (arg0==Val_none) ? NULL : ((QWidget* )(Some_val(arg0)));
  QWidget_twin *_ans = new QWidget_twin(_arg0);
  CAMLreturn((value)_ans);
}

value native_prot_QWidget_keyPressEvent_QKeyEvent(value self,value arg0) {
  CAMLparam2(self,arg0);
  QWidget_twin *_self = (QWidget_twin*)self;
  QKeyEvent* _arg0 = (QKeyEvent* ) (arg0);
  _self -> keyPressEvent(_arg0);
  CAMLreturn(Val_unit);
}

}



