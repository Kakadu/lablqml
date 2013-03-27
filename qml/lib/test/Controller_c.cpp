#include "Controller_c.h"

Controller::Controller() {}
void Controller::onItemSelected(int x0,int x1) {
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling Controller::onItemSelected";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("onItemSelected"));
  value *args = new value[3];
  args[0] = _camlobj;
    args[1] = Val_int (x0); 
    args[2] = Val_int (x1); 
  // delete args or not?
 caml_callbackN(_meth, 3, args);
}
extern "C" value caml_create_Controller(value _dummyUnitVal) {
  CAMLparam1(_dummyUnitVal);
  CAMLlocal1(_ans);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((Controller **) &Field(_ans, 0))) = new Controller();
  CAMLreturn(_ans);
}
