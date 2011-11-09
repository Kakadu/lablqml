#include "headers.h"
#include "enums.h"
#include <stdio.h>
#include <QtGui/QWidget>
#include "AA.h"

void QWidget_twin::call_super_keyPressEvent(QKeyEvent *ev) {
  foo(1);
  QWidget::keyPressEvent(ev);
}
void QWidget_twin::foo(int x) {
  if (x>0)
    foo(x-1);
  else 
    return;
}
void QWidget_twin::keyPressEvent(QKeyEvent *ev) {
    CAMLparam0();
    CAMLlocal3(meth,camlobj,_ev);
    GET_CAML_OBJECT(this,camlobj); // get ocaml object from QObject's property
    printf ("inside QWidget_twin::keyPressedEvent, camlobj = %p, this=%p\n", (void*)camlobj, this);
    meth = caml_get_public_method( camlobj, caml_hash_variant("keyPressEvent"));
    if (meth==0)
      printf ("total fail\n");
    printf ("tag of meth is %d\n", Tag_val(meth) );
    printf("calling callback of meth = %p\n",(void*)meth);
    setAbstrClass(_ev,QKeyEvent,ev);
    caml_callback(meth, camlobj);
    printf ("exit from QWidget_twin::keyPressedEvent\n");
    CAMLreturn0;
}

extern "C" {
CAMLprim
value create_QWidget_twin(value arg0) {
  CAMLparam1(arg0);
  CAMLlocal1(ans);
  QWidget* _arg0 = (arg0==Val_none) ? NULL : QWidget_val(Some_val(arg0));
  QWidget_twin *_ans = new QWidget_twin(_arg0);
  setAbstrClass(ans,QWidget,_ans);
  printf("QWidget_twin created: %p, abstr = %p\n", _ans, (void*)ans);
  CAMLreturn(ans);
}
CAMLprim
value qWidget_twin_super_keyPressEvent(value self,value arg0) {
  CAMLparam2(self,arg0);
  QWidget_twin *_self = QWidget_twin_val(self);
  QKeyEvent* _arg0 = QKeyEvent_val(arg0);
  printf("inside qWidget_twin_super_keyPressEvent\n");
  printf ("QkeyEvent = %p, QWidget_twin=%p\n", _arg0, _self);
  _self -> call_super_keyPressEvent(_arg0);
  CAMLreturn(Val_unit);
}
CAMLprim
value qWidget_twin_show(value self) {
  CAMLparam1(self);
  QWidget_twin *_self = QWidget_twin_val(self);
  printf("showing QWidget_twin = %p\n", _self);
  _self -> show();
  CAMLreturn(Val_unit);
}

}
