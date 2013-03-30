#include "DataItem_c.h"

DataItem::DataItem() {}
//name: string
QString DataItem::name() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling DataItem::name";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("name"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);
  QString cppans;
  cppans = QString(String_val(_ans));
  CAMLreturnT(QString,cppans);
}
//sort: string
QString DataItem::sort() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling DataItem::sort";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("sort"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);
  QString cppans;
  cppans = QString(String_val(_ans));
  CAMLreturnT(QString,cppans);
}
extern "C" value caml_create_DataItem(value _dummyUnitVal) {
  CAMLparam1(_dummyUnitVal);
  CAMLlocal1(_ans);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((DataItem **) &Field(_ans, 0))) = new DataItem();
  CAMLreturn(_ans);
}
