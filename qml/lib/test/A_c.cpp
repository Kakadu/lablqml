#include "A_c.h"

A::A(value _camlobj) : camlobj(_camlobj) {
}
int A::sizey() {
  CAMLlocal2(_ans,_meth);
  _meth = caml_get_public_method(camlobj, caml_hash_variant("sizey"));
  _ans = caml_callback2(_meth, camlobj, Val_unit);
  int cppans;
  cppans = Int_val(_ans);
  return cppans;
}
QModelIndex A::parent(const QModelIndex & x0)  const{
  CAMLlocal2(_ans,_meth);
  _meth = caml_get_public_method(camlobj, caml_hash_variant("parent"));
  value *args = new value[1];
  args[0] = camlobj;
    args[1]=caml_alloc(2,0);
  Store_field(args[1],0,Val_int(x0.row()));
  Store_field(args[1],1,Val_int(x0.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 2, args);
  QModelIndex cppans;
  cppans = createIndex(Int_val(Field(_ans,0)),Int_val(Field(_ans,1)));
  return cppans;
}
QModelIndex A::index(int x0,int x1,const QModelIndex & x2)  const{
  CAMLlocal2(_ans,_meth);
  _meth = caml_get_public_method(camlobj, caml_hash_variant("index"));
  value *args = new value[3];
  args[0] = camlobj;
    args[1] = Val_int (x0); 
    args[2] = Val_int (x1); 
    args[3]=caml_alloc(2,0);
  Store_field(args[3],0,Val_int(x2.row()));
  Store_field(args[3],1,Val_int(x2.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 4, args);
  QModelIndex cppans;
  cppans = createIndex(Int_val(Field(_ans,0)),Int_val(Field(_ans,1)));
  return cppans;
}
int A::columnCount(const QModelIndex & x0)  const{
  CAMLlocal2(_ans,_meth);
  _meth = caml_get_public_method(camlobj, caml_hash_variant("columnCount"));
  value *args = new value[1];
  args[0] = camlobj;
    args[1]=caml_alloc(2,0);
  Store_field(args[1],0,Val_int(x0.row()));
  Store_field(args[1],1,Val_int(x0.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 2, args);
  int cppans;
  cppans = Int_val(_ans);
  return cppans;
}
int A::rowCount(const QModelIndex & x0)  const{
  CAMLlocal2(_ans,_meth);
  _meth = caml_get_public_method(camlobj, caml_hash_variant("rowCount"));
  value *args = new value[1];
  args[0] = camlobj;
    args[1]=caml_alloc(2,0);
  Store_field(args[1],0,Val_int(x0.row()));
  Store_field(args[1],1,Val_int(x0.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 2, args);
  int cppans;
  cppans = Int_val(_ans);
  return cppans;
}
bool A::hasChildren(const QModelIndex & x0)  const{
  CAMLlocal2(_ans,_meth);
  _meth = caml_get_public_method(camlobj, caml_hash_variant("hasChildren"));
  value *args = new value[1];
  args[0] = camlobj;
    args[1]=caml_alloc(2,0);
  Store_field(args[1],0,Val_int(x0.row()));
  Store_field(args[1],1,Val_int(x0.column()));

  // delete args or not?
  _ans = caml_callbackN(_meth, 2, args);
  bool cppans;
  cppans = Bool_val(_ans);
  return cppans;
}
QVariant A::data(const QModelIndex & x0,int x1)  const{
  CAMLlocal2(_ans,_meth);
  _meth = caml_get_public_method(camlobj, caml_hash_variant("data"));
  value *args = new value[2];
  args[0] = camlobj;
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
       qDebug() << "PIZDA";
       //pizda
    } else Q_ASSERT(false);
  } else // empty QVariant
      cppans = QVariant();
  return cppans;
}
extern "C" value caml_A_addRole(value cppobj,value num,value roleName) {
  CAMLparam3(cppobj,roleName,num);
  A *o  = (A*) Field(cppobj,0);
  o->addRole( Int_val(num), QByteArray(String_val(roleName)) );
  CAMLreturn(Val_unit);
}
extern "C" value caml_create_A(value camlObj) {
  CAMLparam1(camlObj);
  CAMLlocal1(_ans);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((A **) &Field(_ans, 0))) = new A(camlObj);
  CAMLreturn(_ans);
}
