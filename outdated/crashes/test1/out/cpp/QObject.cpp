#include <Qt/QtOpenGL>
#include "headers.h"
extern "C" {
#include "enum_headers.h"
// constructor QObject(QObject* parent  = 0)
//argnames = (arg0)
value native_pub_createeee_QObject_QObject(value arg0) {
  CAMLparam1(arg0);
  CAMLlocal1(_ans);
  QObject* _arg0 = (arg0==Val_none) ? NULL : ((QObject* )(Some_val(arg0)));
  QObject* ans = new QObject(_arg0);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((QObject **) &Field(_ans, 0))) = ans;
  CAMLreturn(_ans);
}

// method QObject* parent()
//argnames = (self)
value native_pub_QObject_parent(value self) {
  CAMLparam1(self);
  CAMLlocal1(_ans);
  QObject *_self = (QObject*)self;
  QObject* ans = _self -> parent();
  _ans = (value)(ans);
  CAMLreturn( (ans) ? Val_some(_ans) : Val_none);
}

// method void setParent(QObject*  )
//argnames = (self arg0)
value native_pub_QObject_setParent_QObject(value self,value arg0) {
  CAMLparam2(self,arg0);
  QObject *_self = (QObject*)self;
  QObject* _arg0 = (QObject* ) (arg0);
  _self -> setParent(_arg0);
  CAMLreturn(Val_unit);
}

// method bool signalsBlocked()
//argnames = (self)
value native_pub_QObject_signalsBlocked(value self) {
  CAMLparam1(self);
  CAMLlocal1(_ans);
  QObject *_self = (QObject*)self;
  bool ans = _self -> signalsBlocked();
  _ans = Val_bool(ans);
  CAMLreturn(_ans);
}

}  // extern "C"
