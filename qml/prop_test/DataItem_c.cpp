#include "DataItem_c.h"

DataItem::DataItem() : _camlobjHolder(0) {
}
//cellX: int
int DataItem::cellX() {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,1);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("cellX"));
  _ans = caml_callback2(_meth, _camlobj, Val_unit);
  int cppans;
  cppans = Int_val(_ans);
  CAMLreturnT(int,cppans);
}
extern "C" value caml_DataItem_cellXChanged_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  CAMLlocal1(_z0);
  DataItem *o = (DataItem*) (Field(_cppobj,0));
  int z0;
  z0 = Int_val(_x0);
  o->cellXChanged(z0);
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
