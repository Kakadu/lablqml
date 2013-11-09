#include "stubs.h"
static value Val_some(value v) {
  CAMLparam1( v );
  CAMLlocal1( some );
  some = caml_alloc(1, 0);
  Store_field( some, 0, v );
  CAMLreturn( some );
}
#define ARGC_N_ARGV(_argv,copy)\
  int argc = Wosize_val(_argv);\
  char **copy = new char*[argc+1];\
  for (int i = 0; i < argc; i++) {\
	char *item = String_val(Field(_argv,i));\
    copy[i] = strdup(item);\
  }\
  copy[argc] = NULL;


// string array -> QGuiApplication.t * QQmlEngine.t
extern "C" value caml_create_QGuiApplication(value _argv) {
  CAMLparam1(_argv);
  CAMLlocal3(_ans,_app,_engine);

  ARGC_N_ARGV(_argv, copy)

  QGuiApplication *app = new QGuiApplication(argc, copy);
  QQmlEngine* engine = new QQmlEngine();

  QQmlContext *ctxt = engine->rootContext();
  registerContext(QString("rootContext"), ctxt);

  _app = caml_alloc_small(1, Abstract_tag);
  (*((QGuiApplication **) &Field(_app, 0))) = app;
  _engine = caml_alloc_small(1, Abstract_tag);
  (*((QQmlEngine **) &Field(_engine, 0))) = engine;

  _ans = caml_alloc(2,0);
  Store_field(_ans, 0, _app);
  Store_field(_ans, 1, _engine);
  CAMLreturn(_ans);
}

// string -> QQmlEngine.t -> QQuickWindow.t option
extern "C" value caml_loadQml(value _path, value _engine) {
  CAMLparam2(_path, _engine);
  CAMLlocal1(_ans);

  QQmlEngine *engine = (QQmlEngine*) (Field(_engine,0));
  QUrl source(String_val(_path));
  QQmlComponent *comp = new QQmlComponent(engine, source);
  QObject *topLevel = comp->create(engine->rootContext() );
  if (comp->isError()) {
    qDebug() << comp->errors();
    CAMLreturn(Val_none);
  }
  Q_ASSERT(topLevel != 0);
  //qDebug() << "Classname = " << topLevel->metaObject()->className();

  QQuickWindow *window = qobject_cast<QQuickWindow*>(topLevel);
  if (window!=nullptr) {
    // we have loaded window from QtQuick.Controls probably
    _ans = caml_alloc_small(1, Abstract_tag);
    (*((QQuickWindow **) &Field(_ans, 0))) = window;
    CAMLreturn(Val_some(_ans));
  } else {
    // if it is not QtQuick.Controls.ApplicationWindow it seems to be some QQuickItem
    QQuickItem *item = dynamic_cast<QQuickItem*>(topLevel);
    Q_ASSERT(item != nullptr);
    QQuickView *view = new QQuickView(); // QQuickVew is subclass of QQuickWindow
    view->setContent(source, comp, item);

    _ans = caml_alloc_small(1, Abstract_tag);
    (*((QQuickView **) &Field(_ans, 0))) = view;
    CAMLreturn(Val_some(_ans));
  }
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

// argv -> (unit -> unit) -> string -> unit
extern "C" value caml_run_QQmlApplicationEngine(value _argv, value _cb, value _qmlpath) {
  CAMLparam3(_argv, _cb, _qmlpath);
  CAMLlocal2(_ctx, _cb_res);

  ARGC_N_ARGV(_argv, copy);
  QApplication app(argc, copy);
  QQmlApplicationEngine engine;
  QQmlContext *ctxt = engine.rootContext();

  registerContext(QString("rootContext"), ctxt);
  /*
  _ctx = caml_alloc_small(1, Abstract_tag);
  (*((QQmlContext **) &Field(_ctx, 0))) = ctxt; */
  _cb_res = caml_callback(_cb, Val_unit);
  Q_ASSERT(_cb_res == Val_unit);

  engine.load(QString(String_val(_qmlpath)));
  QList<QObject*> xs = engine.rootObjects();
  if (xs.count() == 0) {
    Q_ASSERT_X(false, "Creating C++ runtime", "Your QML file seems buggy");
  }
  QQuickWindow *window = qobject_cast<QQuickWindow*>(xs.at(0) );
  window->showMaximized();
  app.exec();
  CAMLreturn(Val_unit);
}
