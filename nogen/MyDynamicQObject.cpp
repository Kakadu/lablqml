#include "MyDynamicQObject.h"

const QMetaObject* MyDynamicQObject::metaObject() const {
    qDebug() << "MyDynamicQObject::metaObject()";
    return a_la_staticMetaObject;
}

int MyDynamicQObject::qt_metacall(QMetaObject::Call c, int id, void **arguments)
{
  qDebug() << "inside qt_metacall";
  // do CAMLlocal and CAMLparam
  id = QObject::qt_metacall(c, id, arguments);
  if (id < 0 || c != QMetaObject::InvokeMetaMethod)
    return id;
  Q_ASSERT(false);
  //Q_ASSERT(id < slotList.size());
  //auto slotInfo = slotList[id];
  //QString name =  slotInfo.first;
  //slotList[id]->call(sender(), arguments);
  //Q_ASSERT(false);
  return -1;
}

void MyDynamicQObject::addSlot(const QString& name, const QString& desc) {
  qDebug() << "adding slot " << name;
  q_invokables << QPair<QString,QString>(name, desc);
  reevalThreeFields();
}

// is only wrapper about member function. Was introduced
// because I can't pass a pointer to member-function to struct
void MyDynamicQObject::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        MyDynamicQObject *_t = static_cast<MyDynamicQObject *>(_o);
        _t->wrap_qt_static_metacall(_o,_c,_id,_a);
    }
}

extern "C" value caml_addSlot(value _cppobj, value _name, value _desc) {
  CAMLparam3(_cppobj, _name, _desc);

  MyDynamicQObject *obj = (MyDynamicQObject*)(Field(_cppobj,0));
  QString desc = QString(String_val(_desc));
  QString name = QString(String_val(_name));
  obj->addSlot(name, desc);

  CAMLreturn(Val_unit);
}
////////////////////////////////////////////////////////////////////////
// OCaml stubs
extern "C" value caml_create_myobject(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_ans);
  _ans = caml_alloc_small(1, Abstract_tag);
  (*((MyDynamicQObject **) &Field(_ans, 0))) = new MyDynamicQObject();
  CAMLreturn(_ans);
}

extern "C" value caml_storeCamlObject(value _cppobj,value _camlobj) {
  CAMLparam2(_cppobj,_camlobj);
  MyDynamicQObject *o = (MyDynamicQObject*) (Field(_cppobj,0));
  o->storeCAMLobj(_camlobj); // register global root in member function
  //caml_register_global_root(&(o->_camlobjHolder));
  CAMLreturn(Val_unit);
}
