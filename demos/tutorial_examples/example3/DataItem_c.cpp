#include "DataItem_c.h"

DataItem::DataItem() : _camlobjHolder(0) {
}
//author: string
QString DataItem::author() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,1);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("author"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);
  QString cppans;
  cppans = QString(String_val(_ans));
  CAMLreturnT(QString,cppans);
}
//setName: string->bool
bool DataItem::setName(QString x0) {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,2);
  CAMLlocal1(_cca0);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("setName"));
  _args[0] = _camlobj;
  _cca0 = caml_copy_string(x0.toLocal8Bit().data() );
  _args[1] = _cca0;
  _ans = caml_callbackN(_meth, 2, _args);
  bool cppans;
  cppans = Bool_val(_ans);
  CAMLreturnT(bool,cppans);
}
extern "C" value caml_DataItem_nameChanged_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  CAMLlocal1(_z0);
  DataItem *o = (DataItem*) (Field(_cppobj,0));
  QString z0;
  z0 = QString(String_val(_x0));
  o->nameChanged(z0);
  CAMLreturn(Val_unit);
}
//title: string
QString DataItem::title() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,1);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("title"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);
  QString cppans;
  cppans = QString(String_val(_ans));
  CAMLreturnT(QString,cppans);
}
//setTitle: string->bool
bool DataItem::setTitle(QString x0) {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,2);
  CAMLlocal1(_cca0);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("setTitle"));
  _args[0] = _camlobj;
  _cca0 = caml_copy_string(x0.toLocal8Bit().data() );
  _args[1] = _cca0;
  _ans = caml_callbackN(_meth, 2, _args);
  bool cppans;
  cppans = Bool_val(_ans);
  CAMLreturnT(bool,cppans);
}
extern "C" value caml_DataItem_titleChanged_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  CAMLlocal1(_z0);
  DataItem *o = (DataItem*) (Field(_cppobj,0));
  QString z0;
  z0 = QString(String_val(_x0));
  o->titleChanged(z0);
  CAMLreturn(Val_unit);
}
extern "C" value caml_create_DataItem(value _dummyUnitVal) {
  CAMLparam1(_dummyUnitVal);
  CAMLlocal1(_ans);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((DataItem **) &Field(_ans, 0))) = new DataItem();
  CAMLreturn(_ans);
}
extern "C" value caml_store_value_in_DataItem(value _cppobj,value _camlobj) {
  CAMLparam2(_cppobj,_camlobj);
  DataItem *o = (DataItem*) (Field(_cppobj,0));
  o->storeCAMLobj(_camlobj); // register global root in member function
  //caml_register_global_root(&(o->_camlobjHolder));
  CAMLreturn(Val_unit);
}
