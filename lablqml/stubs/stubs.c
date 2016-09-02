#include "lablqml.h"

#include <QtQml/QQmlEngine>

void registerContext(const QString& name, QQmlContext* v) {
  CAMLparam0();
  CAMLlocal3(_name,_view,_ans);
  static value *closure = nullptr;
  if (closure == nullptr) {
    closure = caml_named_value("register_view");
  }
  Q_ASSERT(closure!=nullptr);
  _name = caml_copy_string(name.toStdString().c_str());
  _view = caml_alloc_small(1, Abstract_tag);
  (*((QQmlContext **) &Field(_view, 0))) = v;
  caml_leave_blocking_section ();
  _ans = caml_callback2(*closure, _name, _view); // should be a unit
  caml_enter_blocking_section();
  Q_UNUSED(_ans);
  CAMLreturn0;
}

// ctx:t -> name:string -> cppobj -> unit
extern "C" value caml_setContextProperty(value _ctx, value _name, value _cppObj) {
  CAMLparam3(_ctx,_name,_cppObj);

  Q_ASSERT( Tag_val(_ctx) == Abstract_tag );
  Q_ASSERT( Tag_val(_cppObj) == Abstract_tag || Tag_val(_cppObj) == Custom_tag );
  QQmlContext *ctx = ((QQmlContext*) Field(_ctx,0));
  QString name = QString(String_val(_name));
  QObject *o =  Tag_val(_cppObj) == Custom_tag?
    (*(QObject**) (Data_custom_val(_cppObj))) : ((QObject*) Field(_cppObj,0));
  ctx->setContextProperty(name, o);
  //qDebug() << "setted property " << name << " to " << o;
  CAMLreturn(Val_unit);
}

// string -> QQmlEngine.t -> unit
extern "C" value caml_QQmlEngine_registerContext(value _name, value _engine) {
  CAMLparam2(_name,_engine);

  Q_ASSERT( true );
  QQmlEngine  *engine = ((QQmlEngine*) Field(_engine,0));
  Q_ASSERT(engine != nullptr);
  QQmlContext *ctx = engine->rootContext();
  const QString &name = QString::fromLocal8Bit(String_val(_name));
  registerContext(name, ctx);
  CAMLreturn(Val_unit);
}

// string -> QQmlEngine.t -> unit
extern "C" value caml_QQmlEngine_addImportPath(value _path, value _engine) {
  CAMLparam2(_path,_engine);

  QQmlEngine  *engine = ((QQmlEngine*) Field(_engine,0));
  Q_ASSERT(engine != nullptr);
  const QString &path = QString::fromLocal8Bit(String_val(_path));
  engine->addImportPath(path);
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
