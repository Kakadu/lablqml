#include "stubs.h"
static value Val_some(value v) {   
  CAMLparam1( v );
  CAMLlocal1( some );
  some = caml_alloc(1, 0);
  Store_field( some, 0, v );
  CAMLreturn( some );
}

// string array -> QGuiApplication.t * QQmlEngine.t
extern "C" value caml_create_QGuiApplication(value _argv) {
  CAMLparam1(_argv);
  CAMLlocal3(_ans,_1,_0);

  int argc = Wosize_val(_argv);
  qDebug() << "argc = " << argc;
  char **copy = new char*[argc+1];
  for (int i = 0; i < argc; i++) {
    //int l = string_length(Field(_argv,i));
	char *item = String_val(Field(_argv,i)); 
	//qDebug() << "Item: " << item;
    copy[i] = strdup(item);
  }
  copy[argc] = NULL;
  int* r_argc = new int(argc);

  QGuiApplication *app = new QGuiApplication(*r_argc, copy);
  QQmlEngine* engine = new QQmlEngine();
  //QQmlComponent *comp = new QQmlComponent(engine);

  QQmlContext *ctxt = engine->rootContext();
  registerContext(QString("rootContext"), ctxt);

  _0 = caml_alloc_small(1, Abstract_tag);
  (*((QGuiApplication **) &Field(_0, 0))) = app;
  _1 = caml_alloc_small(1, Abstract_tag);
  (*((QQmlEngine **) &Field(_1, 0))) = engine;

  _ans = caml_alloc(2,0);
  Store_field(_ans, 0, _0);
  Store_field(_ans, 1, _1);
  CAMLreturn(_ans);
}

// string -> QQmlEngine.t -> QQuickWindow.t option
extern "C" value caml_loadQml(value _path, value _engine) {
  CAMLparam2(_path, _engine);
  CAMLlocal1(_ans);

  QQmlEngine *engine = (QQmlEngine*) (Field(_engine,0));
  QQmlComponent *comp = new QQmlComponent(engine, QUrl(String_val(_path)) );
  QObject *topLevel = comp->create(engine->rootContext() );
  if (comp->isError()) {
    qDebug() << comp->errors();
    CAMLreturn(Val_none);
  }
  QQuickWindow *window = qobject_cast<QQuickWindow*>(topLevel);

  _ans = caml_alloc_small(1, Abstract_tag);
  (*((QQuickWindow **) &Field(_ans, 0))) = window;

  CAMLreturn(Val_some(_ans));
}

extern "C" value caml_QGuiApplication_exec(value _app) {
  CAMLparam1(_app);
  QGuiApplication *app = (QGuiApplication*) (Field(_app,0));
  app->exec();
  CAMLreturn(Val_unit);
}
extern "C" value caml_QQuickWindow_showMaximized(value _w) {
  CAMLparam1(_w);
  QQuickWindow *w = (QQuickWindow*) (Field(_w,0));
  w->showMaximized();
  CAMLreturn(Val_unit);
}
