#include "stubs.h"

/*  QGuiApplication for any GUI application
 *  QApplication inherits QGuiApplication is for QWidget-based apps.
 *  We use first one.
 */
#include <QtGui/QGuiApplication>
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlApplicationEngine>
#include <QtQuick/QQuickView>
#include <QtQuick/QQuickWindow>
#include <QtQuick/QQuickItem>

static value Val_some(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);

  some = caml_alloc(1, 0);
  Store_field(some, 0, v);

  CAMLreturn(some);
}

#define ARGC_N_ARGV(_argv,copy)\
  int argc_val = Wosize_val(_argv);\
  char **copy = new char*[argc_val];\
  for (int i = 0; i < argc_val; ++i) {\
	char *item = String_val(Field(_argv,i));\
    copy[i] = strdup(item);\
  }\
  int *argc = new int(argc_val);

// string array -> QGuiApplication.t * QQmlEngine.t
extern "C" value caml_create_QGuiApplication(value _argv) {
  CAMLparam1(_argv);
  CAMLlocal3(_ans,_app,_engine);
  caml_enter_blocking_section();

  ARGC_N_ARGV(_argv, copy)
  // we need allocate argc because QApplication(int& argc,...)
  QGuiApplication *app = new QGuiApplication(*argc, copy);
  QQmlEngine* engine = new QQmlEngine();

  QObject::connect(engine, &QQmlEngine::quit,
                   [=]() {
                       app->quit();
                   }
      );

  QQmlContext *ctxt = engine->rootContext();
  registerContext(QString("rootContext"), ctxt);

  _app = caml_alloc_small(1, Abstract_tag);
  (*((QGuiApplication **) &Field(_app, 0))) = app;
  _engine = caml_alloc_small(1, Abstract_tag);
  (*((QQmlEngine **) &Field(_engine, 0))) = engine;

  _ans = caml_alloc(2,0);
  Store_field(_ans, 0, _app);
  Store_field(_ans, 1, _engine);
  caml_leave_blocking_section();
  CAMLreturn(_ans);
}

// string -> QQmlEngine.t -> QQuickWindow.t option
extern "C" value caml_loadQml(value _path, value _engine) {
  CAMLparam2(_path, _engine);
  CAMLlocal1(_ans);
  caml_enter_blocking_section();

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
  } else {
    // if it is not QtQuick.Controls.ApplicationWindow it seems to be some QQuickItem
    QQuickItem *item = dynamic_cast<QQuickItem*>(topLevel);
    Q_ASSERT(item != nullptr);
    QQuickView *view = new QQuickView(); // QQuickVew is subclass of QQuickWindow
    view->setContent(source, comp, item);

    _ans = caml_alloc_small(1, Abstract_tag);
    (*((QQuickView **) &Field(_ans, 0))) = view;
  }
  caml_leave_blocking_section();
  CAMLreturn(Val_some(_ans));
}

extern "C" value caml_QGuiApplication_exec(value _app) {
  CAMLparam1(_app);
  QGuiApplication *app = (QGuiApplication*) (Field(_app,0));
  //qDebug() << "App exec. ENTER blocking section" << __FILE__ ;
  caml_enter_blocking_section();
  app->exec();
  caml_leave_blocking_section();
  qDebug() << "quittting";
  CAMLreturn(Val_unit);
}
// QQuickWindow.t -> unit
extern "C" value caml_QQuickWindow_showMaximized(value _w) {
  CAMLparam1(_w);
  caml_enter_blocking_section();
  QQuickWindow *w = (QQuickWindow*) (Field(_w,0));
  Q_ASSERT_X(w != NULL, __func__, "Trying to show window which is NULL");
  w->showMaximized();
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}
// QQuickWindow.t -> unit
extern "C" value caml_QQuickWindow_show(value _w) {
  CAMLparam1(_w);
  caml_enter_blocking_section();
  QQuickWindow *w = (QQuickWindow*) (Field(_w,0));
  Q_ASSERT_X(w != NULL, __func__, "Trying to show window which is NULL");
  w->show();
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}
// QQuickWindow.t -> unit
extern "C" value caml_QQuickWindow_showFullScreen(value _w) {
  CAMLparam1(_w);
  caml_enter_blocking_section();
  QQuickWindow *w = (QQuickWindow*) (Field(_w,0));
  Q_ASSERT_X(w != NULL, __func__, "Trying to show window which is NULL");
  w->showFullScreen();
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}

#define debug_enter_blocking \
qDebug() << "___________ ENTER blocking section in " << __FILE__ << " +" << __LINE__;

#define debug_leave_blocking \
qDebug() << "___________ LEAVE blocking section in " << __FILE__ << " +" << __LINE__;

// argv -> (unit -> unit) -> string -> unit
extern "C" value caml_run_QQmlApplicationEngine(value _argv, value _cb, value _qmlpath) {
  CAMLparam3(_argv, _cb, _qmlpath);
  CAMLlocal2(_ctx, _cb_res);
  //qDebug() << "App exec. inside caml_run_QQmlApplicationEngine. "<<__FILE__<< ", line " << __LINE__ ;
  caml_enter_blocking_section();

  ARGC_N_ARGV(_argv, copy);
  QGuiApplication app(*argc, copy);
  QQmlApplicationEngine engine;
  QQmlContext *ctxt = engine.rootContext();
  QObject::connect(&engine, SIGNAL(quit()), &app, SLOT(quit()));

  registerContext(QString("rootContext"), ctxt);
  /*
  _ctx = caml_alloc_small(1, Abstract_tag);
  (*((QQmlContext **) &Field(_ctx, 0))) = ctxt; */
  //debug_leave_blocking;
  caml_leave_blocking_section();
  _cb_res = caml_callback(_cb, Val_unit);
  //debug_enter_blocking;
  caml_enter_blocking_section();
  Q_ASSERT(_cb_res == Val_unit);

  engine.load(QUrl(QString(String_val(_qmlpath))));
  QList<QObject*> xs = engine.rootObjects();
  if (xs.count() == 0) {
    Q_ASSERT_X(false, "Creating C++ runtime", "Your QML file seems buggy");
  }
  QQuickWindow *window = qobject_cast<QQuickWindow*>(xs.at(0) );
  window->showMaximized();
  //qDebug() << "executing app.exec()";
  app.exec();
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}
