#include "Controller_c.h"

Controller::Controller() {}
//onItemSelected: int->int->unit
void Controller::onItemSelected(int x0,int x1) {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocal2(_cca0,_cca1);
  qDebug() << "Calling Controller::onItemSelected";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("onItemSelected"));
  _cca0 = Val_int(x0); 
  _cca1 = Val_int(x1); 
  value args[3] = { _camlobj,_cca0,_cca1 };
  caml_callbackN(_meth, 3, args);
  CAMLreturn0;
}
//isHasData: bool
bool Controller::isHasData() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling Controller::isHasData";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("isHasData"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);
  bool cppans;
  cppans = Bool_val(_ans);
  CAMLreturnT(bool,cppans);
}
extern "C" value caml_Controller_hasDataChanged_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  CAMLlocal1(_z0);
  Controller *o = (Controller*) (Field(_cppobj,0));
  bool z0;
  z0 = Bool_val(_x0);
  qDebug() << "Going to call Controller::hasDataChanged";
  o->hasDataChanged(z0);
  CAMLreturn(Val_unit);
}
//getDescr: string
QString Controller::getDescr() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling Controller::getDescr";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("getDescr"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);
  QString cppans;
  cppans = QString(String_val(_ans));
  CAMLreturnT(QString,cppans);
}
extern "C" value caml_Controller_descChanged_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  CAMLlocal1(_z0);
  Controller *o = (Controller*) (Field(_cppobj,0));
  QString z0;
  z0 = QString(String_val(_x0));
  qDebug() << "Going to call Controller::descChanged";
  o->descChanged(z0);
  CAMLreturn(Val_unit);
}
extern "C" value caml_create_Controller(value _dummyUnitVal) {
  CAMLparam1(_dummyUnitVal);
  CAMLlocal1(_ans);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((Controller **) &Field(_ans, 0))) = new Controller();
  CAMLreturn(_ans);
}
