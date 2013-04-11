#include <Qt/QtOpenGL>
#include "headers.h"
#include "enum_headers.h"
#include <stdio.h>

class QWidget_twin : public QWidget {
Q_OBJECT
public:
  virtual ~QWidget_twin() {}
void keyPressEvent(QKeyEvent* arg0 ) {
  CAMLparam0();
  CAMLlocal2(camlobj,meth);
  GET_CAML_OBJECT(this,the_caml_object)
  camlobj = (value) the_caml_object;
  meth = caml_get_public_method( camlobj, caml_hash_variant("keyPressEvent"));
  if (meth == 0)
    printf("total fail\n");
  value *args = new value[2];
  args[0] = camlobj;
  args[1] = (value)(arg0);;
    // delete args or not?
  caml_callbackN(meth, 2, args);;
  CAMLreturn0;
}

QObject* parent() {
  CAMLparam0();
  CAMLlocal3(camlobj,_ans,meth);
  GET_CAML_OBJECT(this,the_caml_object)
  camlobj = (value) the_caml_object;
  meth = caml_get_public_method( camlobj, caml_hash_variant("parent"));
  if (meth == 0)
    printf("total fail\n");
  _ans = caml_callback(meth, camlobj);;
  QObject* ans = (QObject* ) (_ans);;
  Q_UNUSED(caml__frame); //caml_local_roots = caml__frame;
  return ans;
}

void setParent(QObject* arg0 ) {
  CAMLparam0();
  CAMLlocal2(camlobj,meth);
  GET_CAML_OBJECT(this,the_caml_object)
  camlobj = (value) the_caml_object;
  meth = caml_get_public_method( camlobj, caml_hash_variant("setParent"));
  if (meth == 0)
    printf("total fail\n");
  value *args = new value[2];
  args[0] = camlobj;
  args[1] = (value)(arg0);;
    // delete args or not?
  caml_callbackN(meth, 2, args);;
  CAMLreturn0;
}

bool signalsBlocked() {
  CAMLparam0();
  CAMLlocal3(camlobj,_ans,meth);
  GET_CAML_OBJECT(this,the_caml_object)
  camlobj = (value) the_caml_object;
  meth = caml_get_public_method( camlobj, caml_hash_variant("signalsBlocked"));
  if (meth == 0)
    printf("total fail\n");
  _ans = caml_callback(meth, camlobj);;
  bool ans = Bool_val(_ans);;
  Q_UNUSED(caml__frame); //caml_local_roots = caml__frame;
  return ans;
}

  QWidget_twin(QWidget* x0  = 0,Qt::WindowFlags x1  = 0) : QWidget(x0,x1) {}
};

