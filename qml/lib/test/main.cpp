#include "../stubs.h"

#include <QtGui/QGuiApplication>
#include <QtQuick/qquickitem.h>
#include <QtQuick/qquickview.h>

void doCaml() {
  static value *closure = nullptr;
  if (closure == nullptr) {
    closure = caml_named_value("doCaml");
  }
  Q_ASSERT(closure!=nullptr);
  caml_callback(*closure, Val_unit); // should be a unit
}

int main(int argc, char ** argv) {
    caml_main(argv);
    //caml_startup(argv);
    QGuiApplication app(argc, argv);
    QQuickView view;
    view.setResizeMode(QQuickView::SizeRootObjectToView);

    QQmlContext *ctxt = view.rootContext();
    registerContext(QString("rootContext"), ctxt);
    doCaml();
    view.setSource(QUrl::fromLocalFile(QString("view.qml")));
    view.show();

    return app.exec();
}
