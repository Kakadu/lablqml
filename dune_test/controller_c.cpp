/*
 * Generated at 16 Jul, 2018 20:52:40
 */
#include "controller.h"

// controller::getdescr: void,QString
QString controller::getdescr()  {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,2);
  CAMLlocal1(_cca0);
  caml_acquire_runtime_system();
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("getdescr"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);;
  caml_release_runtime_system();
  QString cppans;
  cppans = QString(String_val(_ans));
  CAMLreturnT(QString,cppans);
}
// stub: void name(QString)
extern "C" value caml_controller_descrChanged_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  // aux vars count = 0
  caml_release_runtime_system();
  controller *o = (controller*) (Field(_cppobj,0));
  QString z0;
  z0 = QString(String_val(_x0));
  o->descrChanged(z0);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}
extern "C" value caml_create_controller(value _dummyUnitVal) {
  CAMLparam1(_dummyUnitVal);
  CAMLlocal1(_ans);
  caml_release_runtime_system();
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((controller **) &Field(_ans, 0))) = new controller();
  caml_acquire_runtime_system();
  CAMLreturn(_ans);
}

extern "C" value caml_store_value_in_controller(value _cppobj,value _camlobj) {
  CAMLparam2(_cppobj,_camlobj);
  caml_release_runtime_system();
  controller *o = (controller*) (Field(_cppobj,0));
  o->storeCAMLobj(_camlobj);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}
