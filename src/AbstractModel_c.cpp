#include "AbstractModel_c.h"

AbstractModel::AbstractModel() : _camlobjHolder(0) {
}
//parent: QModelIndex.t->QModelIndex.t
QModelIndex AbstractModel::parent(const QModelIndex & x0) const {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,2);
  CAMLlocal1(_cca0);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("parent"));
  _args[0] = _camlobj;
  _cca0 = caml_alloc(2,0);
  Store_field(_cca0,0,Val_int(x0.row()));
  Store_field(_cca0,1,Val_int(x0.column()));

  _args[1] = _cca0;
  _ans = caml_callbackN(_meth, 2, _args);
  QModelIndex cppans;
  cppans = createIndex(Int_val(Field(_ans,0)),Int_val(Field(_ans,1)));
  CAMLreturnT(QModelIndex,cppans);
}
//index: int->int->QModelIndex.t->QModelIndex.t
QModelIndex AbstractModel::index(int x0,int x1,const QModelIndex & x2) const {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,4);
  CAMLlocal3(_cca0,_cca1,_cca2);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("index"));
  _args[0] = _camlobj;
  _cca0 = Val_int(x0); 
  _args[1] = _cca0;
  _cca1 = Val_int(x1); 
  _args[2] = _cca1;
  _cca2 = caml_alloc(2,0);
  Store_field(_cca2,0,Val_int(x2.row()));
  Store_field(_cca2,1,Val_int(x2.column()));

  _args[3] = _cca2;
  _ans = caml_callbackN(_meth, 4, _args);
  QModelIndex cppans;
  cppans = createIndex(Int_val(Field(_ans,0)),Int_val(Field(_ans,1)));
  CAMLreturnT(QModelIndex,cppans);
}
//columnCount: QModelIndex.t->int
int AbstractModel::columnCount(const QModelIndex & x0) const {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,2);
  CAMLlocal1(_cca0);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("columnCount"));
  _args[0] = _camlobj;
  _cca0 = caml_alloc(2,0);
  Store_field(_cca0,0,Val_int(x0.row()));
  Store_field(_cca0,1,Val_int(x0.column()));

  _args[1] = _cca0;
  _ans = caml_callbackN(_meth, 2, _args);
  int cppans;
  cppans = Int_val(_ans);
  CAMLreturnT(int,cppans);
}
//rowCount: QModelIndex.t->int
int AbstractModel::rowCount(const QModelIndex & x0) const {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,2);
  CAMLlocal1(_cca0);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("rowCount"));
  _args[0] = _camlobj;
  _cca0 = caml_alloc(2,0);
  Store_field(_cca0,0,Val_int(x0.row()));
  Store_field(_cca0,1,Val_int(x0.column()));

  _args[1] = _cca0;
  _ans = caml_callbackN(_meth, 2, _args);
  int cppans;
  cppans = Int_val(_ans);
  CAMLreturnT(int,cppans);
}
//hasChildren: QModelIndex.t->bool
bool AbstractModel::hasChildren(const QModelIndex & x0) const {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,2);
  CAMLlocal1(_cca0);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("hasChildren"));
  _args[0] = _camlobj;
  _cca0 = caml_alloc(2,0);
  Store_field(_cca0,0,Val_int(x0.row()));
  Store_field(_cca0,1,Val_int(x0.column()));

  _args[1] = _cca0;
  _ans = caml_callbackN(_meth, 2, _args);
  bool cppans;
  cppans = Bool_val(_ans);
  CAMLreturnT(bool,cppans);
}
//data: QModelIndex.t->int->QVariant.t
QVariant AbstractModel::data(const QModelIndex & x0,int x1) const {
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  CAMLlocalN(_args,3);
  CAMLlocal2(_cca0,_cca1);
  value _camlobj = this->_camlobjHolder;
  Q_ASSERT(Is_block(_camlobj));
  Q_ASSERT(Tag_val(_camlobj) == Object_tag);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("data"));
  _args[0] = _camlobj;
  _cca0 = caml_alloc(2,0);
  Store_field(_cca0,0,Val_int(x0.row()));
  Store_field(_cca0,1,Val_int(x0.column()));

  _args[1] = _cca0;
  _cca1 = Val_int(x1); 
  _args[2] = _cca1;
  _ans = caml_callbackN(_meth, 3, _args);
  QVariant cppans;
  if (Is_block(_ans)) {
    if (caml_hash_variant("string")==Field(_ans,0))
      cppans = QVariant::fromValue(QString(String_val(Field(_ans,1))));
    else if(caml_hash_variant("qobject")==Field(_ans,0)) {
      cppans = QVariant::fromValue((QObject*) (Field(Field(_ans,1),0)));
    } else Q_ASSERT(false);
  } else // empty QVariant
      cppans = QVariant();
  CAMLreturnT(QVariant,cppans);
}
extern "C" value caml_AbstractModel_dataChanged_cppmeth_wrapper(value _cppobj,value _x0,value _x1) {
  CAMLparam3(_cppobj,_x0,_x1);
  CAMLlocal1(_z0);
  AbstractModel *o = (AbstractModel*) (Field(_cppobj,0));
  QModelIndex z0;
  z0 = o->makeIndex(Int_val(Field(_x0,0)),Int_val(Field(_x0,1)));
  QModelIndex z1;
  z1 = o->makeIndex(Int_val(Field(_x1,0)),Int_val(Field(_x1,1)));
  o->dataChanged(z0,z1);
  CAMLreturn(Val_unit);
}
extern "C" value caml_AbstractModel_beginInsertRows_cppmeth_wrapper(value _cppobj,value _x0,value _x1,value _x2) {
  CAMLparam4(_cppobj,_x0,_x1,_x2);
  CAMLlocal1(_z0);
  AbstractModel *o = (AbstractModel*) (Field(_cppobj,0));
  QModelIndex z0;
  z0 = o->makeIndex(Int_val(Field(_x0,0)),Int_val(Field(_x0,1)));
  int z1;
  z1 = Int_val(_x1);
  int z2;
  z2 = Int_val(_x2);
  o->beginInsertRows(z0,z1,z2);
  CAMLreturn(Val_unit);
}
extern "C" value caml_AbstractModel_endInsertRows_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  CAMLlocal1(_z0);
  AbstractModel *o = (AbstractModel*) (Field(_cppobj,0));
  o->endInsertRows();
  CAMLreturn(Val_unit);
}
extern "C" value caml_AbstractModel_beginRemoveRows_cppmeth_wrapper(value _cppobj,value _x0,value _x1,value _x2) {
  CAMLparam4(_cppobj,_x0,_x1,_x2);
  CAMLlocal1(_z0);
  AbstractModel *o = (AbstractModel*) (Field(_cppobj,0));
  QModelIndex z0;
  z0 = o->makeIndex(Int_val(Field(_x0,0)),Int_val(Field(_x0,1)));
  int z1;
  z1 = Int_val(_x1);
  int z2;
  z2 = Int_val(_x2);
  o->beginRemoveRows(z0,z1,z2);
  CAMLreturn(Val_unit);
}
extern "C" value caml_AbstractModel_endRemoveRows_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  CAMLlocal1(_z0);
  AbstractModel *o = (AbstractModel*) (Field(_cppobj,0));
  o->endRemoveRows();
  CAMLreturn(Val_unit);
}
extern "C" value caml_AbstractModel_addRole_cppmeth_wrapper(value _cppobj,value _x0,value _x1) {
  CAMLparam3(_cppobj,_x0,_x1);
  CAMLlocal1(_z0);
  AbstractModel *o = (AbstractModel*) (Field(_cppobj,0));
  int z0;
  z0 = Int_val(_x0);
  QByteArray z1;
  z1 = QByteArray(String_val(_x1));
  o->addRole(z0,z1);
  CAMLreturn(Val_unit);
}
extern "C" value caml_create_AbstractModel(value _dummyUnitVal) {
  CAMLparam1(_dummyUnitVal);
  CAMLlocal1(_ans);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((AbstractModel **) &Field(_ans, 0))) = new AbstractModel();
  CAMLreturn(_ans);
}
extern "C" value caml_store_value_in_AbstractModel(value _cppobj,value _camlobj) {
  CAMLparam2(_cppobj,_camlobj);
  AbstractModel *o = (AbstractModel*) (Field(_cppobj,0));
  o->storeCAMLobj(_camlobj); // register global root in member function
  //caml_register_global_root(&(o->_camlobjHolder));
  CAMLreturn(Val_unit);
}
