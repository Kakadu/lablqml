#include "stubs.h"

#include <QtGui/QGuiApplication>
#include <QtWidgets/QApplication>
#include <QtQuick/qquickview.h>
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlComponent>

void doCaml() {
    CAMLparam0();
    static value *closure = nullptr;
    if (closure == nullptr) {
      closure = caml_named_value("doCamlInitialization");
    }
    Q_ASSERT(closure!=nullptr);
    caml_callback(*closure, Val_unit); // should be a unit
    CAMLreturn0;
}

int main(int argc, char ** argv) {
    caml_main(argv);
    QApplication app(argc, argv);

    QQmlEngine engine;
    QQmlComponent component(&engine);

    QQmlContext *ctxt = engine.rootContext();
    registerContext(QString("rootContext"), ctxt);
    doCaml();

    component.loadUrl(QUrl("Root.qml"));
    QObject *topLevel = component.create();
    QQuickWindow *window = qobject_cast<QQuickWindow *>(topLevel);

    window->show();

    return app.exec();
}
