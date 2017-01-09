#include <lablqml.h> // for register_context

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
}
#include <QtGui/QGuiApplication>
#include <QtQuick/QQuickView>

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
    QQuickView view;
    view.setResizeMode(QQuickView::SizeRootObjectToView);

    QQmlContext *ctxt = view.rootContext();
    registerContext(QString("rootContext"), ctxt);
    doCaml();
    view.setSource(QUrl::fromLocalFile(QString("Root.qml")));
    view.show();

    return app.exec();
}
