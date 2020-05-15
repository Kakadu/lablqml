#include "lablqml.h"

#include <QtGui/QGuiApplication>
#include <QtCore/QDebug>
#include <QtQuick/qquickview.h>

void doCaml() {
  CAMLparam0();
  static value *closure = nullptr;
  if (closure == nullptr) {
    closure = const_cast<value*>(caml_named_value("doCaml"));
  }
  Q_ASSERT(closure!=nullptr);
  caml_acquire_runtime_system();
  caml_callback(*closure, Val_unit); // should be a unit
  caml_release_runtime_system();

  CAMLreturn0;
}

int main(int argc, char ** argv) {
    caml_main(argv);
    caml_release_runtime_system();

    QGuiApplication app(argc, argv);
    QQuickView view;
    view.setResizeMode(QQuickView::SizeRootObjectToView);

    QQmlContext *ctxt = view.rootContext();
    registerContext(QString("rootContext"), ctxt);
    doCaml();
    view.setSource(QUrl::fromLocalFile(QString("Root.qml")));
    view.show();

    return app.exec();
}
