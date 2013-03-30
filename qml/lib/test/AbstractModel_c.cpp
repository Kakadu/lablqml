#include "AbstractModel_c.h"

AbstractModel::AbstractModel() {}
QModelIndex AbstractModel::parent(const QModelIndex & x0)  const{
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling AbstractModel::parent";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("parent"));
  value *args = new value[2];
  args[0] = _camlobj;
    args[1]=caml_alloc(2,0);
  Store_field(args[1],0,Val_int(x0.row()));
  Store_field(args[1],1,Val_int(x0.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 2, args);
  QModelIndex cppans;
  cppans = createIndex(Int_val(Field(_ans,0)),Int_val(Field(_ans,1)));
  CAMLreturnT(QModelIndex,cppans);
}
QModelIndex AbstractModel::index(int x0,int x1,const QModelIndex & x2)  const{
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling AbstractModel::index";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("index"));
  value *args = new value[4];
  args[0] = _camlobj;
    args[1] = Val_int (x0); 
    args[2] = Val_int (x1); 
    args[3]=caml_alloc(2,0);
  Store_field(args[3],0,Val_int(x2.row()));
  Store_field(args[3],1,Val_int(x2.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 4, args);
  QModelIndex cppans;
  cppans = createIndex(Int_val(Field(_ans,0)),Int_val(Field(_ans,1)));
  CAMLreturnT(QModelIndex,cppans);
}
int AbstractModel::columnCount(const QModelIndex & x0)  const{
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling AbstractModel::columnCount";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("columnCount"));
  value *args = new value[2];
  args[0] = _camlobj;
    args[1]=caml_alloc(2,0);
  Store_field(args[1],0,Val_int(x0.row()));
  Store_field(args[1],1,Val_int(x0.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 2, args);
  int cppans;
  cppans = Int_val(_ans);
  CAMLreturnT(int,cppans);
}
int AbstractModel::rowCount(const QModelIndex & x0)  const{
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling AbstractModel::rowCount";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("rowCount"));
  value *args = new value[2];
  args[0] = _camlobj;
    args[1]=caml_alloc(2,0);
  Store_field(args[1],0,Val_int(x0.row()));
  Store_field(args[1],1,Val_int(x0.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 2, args);
  int cppans;
  cppans = Int_val(_ans);
  CAMLreturnT(int,cppans);
}
bool AbstractModel::hasChildren(const QModelIndex & x0)  const{
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling AbstractModel::hasChildren";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("hasChildren"));
  value *args = new value[2];
  args[0] = _camlobj;
    args[1]=caml_alloc(2,0);
  Store_field(args[1],0,Val_int(x0.row()));
  Store_field(args[1],1,Val_int(x0.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 2, args);
  bool cppans;
  cppans = Bool_val(_ans);
  CAMLreturnT(bool,cppans);
}
QVariant AbstractModel::data(const QModelIndex & x0,int x1)  const{
  CAMLparam0();
  CAMLlocal3(_ans,_meth,_x0);
  qDebug() << "Calling AbstractModel::data";
  GET_CAML_OBJECT(this,_camlobj);
  _meth = caml_get_public_method(_camlobj, caml_hash_variant("data"));
  value *args = new value[3];
  args[0] = _camlobj;
    args[1]=caml_alloc(2,0);
  Store_field(args[1],0,Val_int(x0.row()));
  Store_field(args[1],1,Val_int(x0.column()));

    args[2] = Val_int (x1); 
  // delete args or not?
  _ans = caml_callbackN(_meth, 3, args);
  QVariant cppans;
  if (Is_block(_ans)) {
    if (caml_hash_variant("string")==Field(_ans,0))
      cppans = QVariant::fromValue(QString(String_val(Field(_ans,1))));
    else if(caml_hash_variant("qobject")==Field(_ans,0)) {
      //qDebug() << "PIZDA";
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
  qDebug() << "Going to call AbstractModel::dataChanged";
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
  qDebug() << "Going to call AbstractModel::beginInsertRows";
  o->beginInsertRows(z0,z1,z2);
  CAMLreturn(Val_unit);
}
extern "C" value caml_AbstractModel_endInsertRows_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  CAMLlocal1(_z0);
  AbstractModel *o = (AbstractModel*) (Field(_cppobj,0));
  qDebug() << "Going to call AbstractModel::endInsertRows";
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
  qDebug() << "Going to call AbstractModel::beginRemoveRows";
  o->beginRemoveRows(z0,z1,z2);
  CAMLreturn(Val_unit);
}
extern "C" value caml_AbstractModel_endRemoveRows_cppmeth_wrapper(value _cppobj,value _x0) {
  CAMLparam2(_cppobj,_x0);
  CAMLlocal1(_z0);
  AbstractModel *o = (AbstractModel*) (Field(_cppobj,0));
  qDebug() << "Going to call AbstractModel::endRemoveRows";
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
  qDebug() << "Going to call AbstractModel::addRole";
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
