#include "../stubs.h"

#include <QtGui/QGuiApplication>
#include <QtQuick/qquickview.h>
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlComponent>

void doCaml() {
  CAMLparam0();
  static value *closure = nullptr;
  if (closure == nullptr) {
    closure = caml_named_value("doCaml");
  }
  Q_ASSERT(closure!=nullptr);
  caml_callback(*closure, Val_unit); // should be a unit
  CAMLreturn0;
}

int main(int argc, char ** argv) {
    caml_main(argv);
    QGuiApplication app(argc, argv);

    QQmlEngine engine;
    QQmlComponent component(&engine);

    QQmlContext *ctxt = engine.rootContext();
    registerContext(QString("rootContext"), ctxt);
    doCaml();

    component.loadUrl(QUrl("Root.qml"));
    QObject *topLevel = component.create(ctxt);
    if (component.isError()) {
      qDebug() << component.errors();
      return 1;
    }
    QQuickWindow *window = qobject_cast<QQuickWindow*>(topLevel);
    // allow using Qt.quit() from QML
    QObject::connect(&engine,SIGNAL(quit()),&app,SLOT(quit()) );
	window->setTitle( QObject::tr("QOCamlBrowser: Demo app for lablqt&mocml") );
    window->showMaximized();
    return app.exec();
}
