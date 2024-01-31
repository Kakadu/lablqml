#include "lablqml.h"

#include <QtQml/QQmlEngine>
#include <QtQml/QQmlApplicationEngine>
#include <QtQml/QQmlProperty>
#include <QQuickWindow>

void registerContext(const QString& name, QQmlContext* v) {
  // LABLQML_LEAVE_OCAML;
  CAMLparam0();
  CAMLlocal3(_name,_view,_ans);
  static value *closure = nullptr;



  if (closure == nullptr) {
    closure = (value*) caml_named_value("register_view") ;
  }
  Q_ASSERT(closure!=nullptr);
  _name = caml_copy_string(name.toStdString().c_str());
  _view = caml_alloc_small(1, Abstract_tag);
  (*((QQmlContext **) &Field(_view, 0))) = v;
  _ans = caml_callback2(*closure, _name, _view); // should be a unit

  // LABLQML_ENTER_OCAML;

  Q_UNUSED(_ans);
  CAMLreturn0;
}

// ctx:t -> name:string -> cppobj -> unit
extern "C" value caml_setContextProperty(value _ctx, value _name, value _cppObj) {
  CAMLparam3(_ctx,_name,_cppObj);
  // LABLQML_LEAVE_OCAML;

  Q_ASSERT( Tag_val(_ctx) == Abstract_tag );
  Q_ASSERT( Tag_val(_cppObj) == Abstract_tag || Tag_val(_cppObj) == Custom_tag );
  QQmlContext *ctx = ((QQmlContext*) Field(_ctx,0));

  QString name = QString(String_val(_name));
  QObject *o =  Tag_val(_cppObj) == Custom_tag?
    (*(QObject**) (Data_custom_val(_cppObj))) : ((QObject*) Field(_cppObj,0));
  ctx->setContextProperty(name, o);
  //qDebug() << "setted property " << name << " to " << o;
  // LABLQML_ENTER_OCAML;
  CAMLreturn(Val_unit);
}

// string -> QQmlEngine.t -> unit
extern "C" value caml_QQmlEngine_registerContext(value _name, value _engine) {
  CAMLparam2(_name,_engine);
  // LABLQML_LEAVE_OCAML;

  QQmlEngine *engine = ((QQmlEngine*) Field(_engine,0));
  Q_ASSERT(engine != nullptr);
  QQmlContext *ctx = engine->rootContext();
  const QString &name = QString::fromLocal8Bit(String_val(_name));
  registerContext(name, ctx);
  // LABLQML_ENTER_OCAML;
  CAMLreturn(Val_unit);
}

// string -> QQmlEngine.t -> unit
extern "C" value caml_QQmlEngine_addImportPath(value _path, value _engine) {
  CAMLparam2(_path,_engine);
  // LABLQML_LEAVE_OCAML;

  QQmlEngine *engine = ((QQmlEngine*) Field(_engine,0));
  Q_ASSERT(engine != nullptr);
  const QString &path = QString::fromLocal8Bit(String_val(_path));
  engine->addImportPath(path);
  // LABLQML_ENTER_OCAML;
  CAMLreturn(Val_unit);
}

// QQmlApplicationEngine.t -> cppobj array
extern "C" value caml_qml_application_engine_root_objects(value app_engine_val) {
#warning "not tested"
  CAMLparam1(app_engine_val);
  CAMLlocal1(objects_array);

  QQmlApplicationEngine *app_engine = ((QQmlApplicationEngine*) Field(app_engine_val, 0));
  Q_ASSERT(app_engine != nullptr);

  QList<QObject*> list =  app_engine->rootObjects();
  objects_array = caml_alloc(list.size(), Abstract_tag);
  for (int i = 0; i < list.size(); ++i)
    Ctype_field(QObject, objects_array, i) = list.at(i);

  CAMLreturn(objects_array);
}

// QQmlApplicationEngine.t -> string -> Abstract
extern "C" value caml_qml_application_engine_root_named(value app_engine_val, value object_name_val) {
  CAMLparam2(app_engine_val, object_name_val);
  CAMLlocal2(obj_val, some_property);

  QQmlApplicationEngine *app_engine = ((QQmlApplicationEngine*) Field(app_engine_val, 0));
  Q_ASSERT(app_engine != nullptr);
  QList<QObject*> list = app_engine->rootObjects();
  if(list.empty())
	caml_failwith("Object tree is empty");
  QObject *o = 0;
  for (int i = 0; i < list.size(); ++i)
    if (0 != (o = list.at(i)->findChild<QObject*>(String_val(object_name_val))))
      break;
  if(0 == o)
    caml_failwith("No such object in QML");
  obj_val = caml_alloc(sizeof(QObject*), Abstract_tag);
  Ctype_field(QObject, obj_val, 0) = o;
  CAMLreturn(obj_val);
}

// Abstract (cppobj) -> string -> Abstract (cppobj)
extern "C" value caml_qml_child_named(value parent_object_val, value child_name_val) {
  CAMLparam2(parent_object_val, child_name_val);
  CAMLlocal1(obj_val);

  QObject *parent = ((QObject*) Field(parent_object_val, 0));
  Q_ASSERT(parent != nullptr);

  QObject *child = parent->findChild<QObject*>(String_val(child_name_val));

  if(0 == child)
	caml_failwith("Child not found");

  obj_val = caml_alloc(sizeof(QObject*), Abstract_tag);
  Ctype_field(QObject, obj_val, 0) = child;
  CAMLreturn(obj_val);
}

// Abstract (cppobj) -> string -> Abstract (cppobj)
extern "C" value caml_qml_property_child_named(value parent_object_val, value child_name_val) {
  CAMLparam2(parent_object_val, child_name_val);
  CAMLlocal1(obj_val);

  QObject *parent = ((QObject*) Field(parent_object_val, 0));
  Q_ASSERT(parent != nullptr);

  QVariant property_object = parent->property(String_val(child_name_val));
  if(!property_object.isValid())
	caml_failwith("Child not found");

  QObject *child = qvariant_cast<QObject *>(property_object);
  obj_val = caml_alloc(sizeof(QObject*), Abstract_tag);
  Ctype_field(QObject, obj_val, 0) = child;
  CAMLreturn(obj_val);
}

// QQuickWindow.t -> string -> cppobj option
extern "C" value caml_quick_window_find_child(value window_val, value name_val) {
#warning "not tested"
  CAMLparam1(window_val);
  CAMLlocal1(object_val);

  QQuickWindow *window = ((QQuickWindow*) Field(window_val, 0));
  Q_ASSERT(window != nullptr);
  QObject* qobject = window->findChild<QObject*>(String_val(name_val));
  if(0 == qobject)
    CAMLreturn(Val_none);
  else
    CAMLreturn(Val_some(object_val));
}

bool lablqml_check_locks = false;

extern "C" value caml_lablqml_set_check_locks(value b) {
  CAMLparam1(b);
  lablqml_check_locks = Bool_val(b);
  CAMLreturn(Val_unit);
}

/*
extern "C" value caml_set_caml_object(value _cppobj,value _camlobj) {
  CAMLparam2(_cppobj, _camlobj);
  QObject *o = ((QObject*) Field(_cppobj,0));

  thi\
  caml_register_global_root(&_camlobj);
  o->setProperty(CAMLOBJ_PROPERTY, QVariant::fromValue(_camlobj) );
  CAMLreturn(Val_unit);
}
*/
