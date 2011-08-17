#include <Qt/QtOpenGL>
#include "headers.h"
extern "C" {
value make_root_widget() {
  CAMLparam0();
  CAMLlocal1(_ans);
  QWidget *ans = new QWidget();
  _ans = (value)ans;
  CAMLreturn(_ans);
}

}  // extern "C"
