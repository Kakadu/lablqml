#include "Controller_c.h"

Controller::Controller() : _camlobjHolder(0) {
}
//onItemSelected: int->int->unit
void Controller::onItemSelected(int x0,int x1) {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,3);
  CAMLlocal2(_cca0,_cca1);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("onItemSelected"));
  _args[0] = _camlobj;
  _cca0 = Val_int(x0); 
  _args[1] = _cca0;
  _cca1 = Val_int(x1); 
  _args[2] = _cca1;
  caml_callbackN(_meth, 3, _args);
  CAMLreturn0;
}
//setPaths: string list->unit
void Controller::setPaths(QList<QString> x0) {
  CAMLparam0();
  CAMLlocal5(_ans,_meth,_x0,_x1,_x2);
  CAMLlocalN(_args,2);
  CAMLlocal1(_cca0);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("setPaths"));
  _args[0] = _camlobj;
  _cca0 = Val_emptylist;
  if ((x0).length() != 0) {
    auto it = (x0).end() - 1;
    for (;;) {
      _x0 = caml_alloc(2,0);
    _x1 = caml_copy_string((*it).toLocal8Bit().data() );
      Store_field(_x0, 0, _x1);
      Store_field(_x0, 1, _cca0);
      _cca0 = _x0;
      if ((x0).begin() == it) break;
      it--;
    }
  }

  _args[1] = _cca0;
  caml_callbackN(_meth, 2, _args);
  CAMLreturn0;
}
//paths: string list
QList<QString> Controller::paths() {
  CAMLparam0();
  CAMLlocal5(_ans,_meth,_x0,_x1,_x2);
  CAMLlocalN(_args,1);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("paths"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);
  QList<QString> cppans;
  //generating: string list
  _x0 = _ans;
  while (_x0 != Val_emptylist) {
    _x1 = Field(_x0,0); /* head */
    QString xx1;
    xx1 = QString(String_val(_x1));
    cppans << xx1;
    _x0 = Field(_x0,1);
  }
  CAMLreturnT(QList<QString>,cppans);
}
//getFullPath: string
QString Controller::getFullPath() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,1);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("getFullPath"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);
  QString cppans;
  cppans = QString(String_val(_ans));
  CAMLreturnT(QString,cppans);
}
//isHasData: bool
bool Controller::isHasData() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,1);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
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
  o->hasDataChanged(z0);
  CAMLreturn(Val_unit);
}
//getDescr: string
QString Controller::getDescr() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,1);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
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
extern "C" value caml_store_value_in_Controller(value _cppobj,value _camlobj) {
  CAMLparam2(_cppobj,_camlobj);
  Controller *o = (Controller*) (Field(_cppobj,0));
  o->storeCAMLobj(_camlobj); // register global root in member function
  //caml_register_global_root(&(o->_camlobjHolder));
  CAMLreturn(Val_unit);
}
