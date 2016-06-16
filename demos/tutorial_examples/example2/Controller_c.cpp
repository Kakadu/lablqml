#include "Controller_c.h"

Controller::Controller() : _camlobjHolder(0) {
}
//onMouseClicked: string->unit
void Controller::onMouseClicked(QString x0) {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,2);
  CAMLlocal1(_cca0);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("onMouseClicked"));
  _args[0] = _camlobj;
  _cca0 = caml_copy_string(x0.toLocal8Bit().data() );
  _args[1] = _cca0;
  caml_callbackN(_meth, 2, _args);
  CAMLreturn0;
}
extern "C" value caml_create_Controller(value _dummyUnitVal) {
  CAMLparam1(_dummyUnitVal);
  CAMLlocal1(_ans);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((Controller **) &Field(_ans, 0))) = new Controller();
  CAMLreturn(_ans);
}
extern "C" value caml_store_value_in_Controller(value _cppobj,value _camlobj) {
  CAMLparam2(_cppobj,_camlobj);
  Controller *o = (Controller*) (Field(_cppobj,0));
  o->storeCAMLobj(_camlobj); // register global root in member function
  //caml_register_global_root(&(o->_camlobjHolder));
  CAMLreturn(Val_unit);
}
