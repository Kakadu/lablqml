#include <Qt/QtOpenGL>
#include "headers.h"
extern "C" {
#include "enum_headers.h"
// method int key()
//argnames = (self)
value native_pub_QKeyEvent_key(value self) {
  CAMLparam1(self);
  CAMLlocal1(_ans);
  QKeyEvent *_self = (QKeyEvent*)self;
  int ans = _self -> key();
  _ans = Val_int(ans);
  CAMLreturn(_ans);
}

}  // extern "C"
