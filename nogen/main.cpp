#include "stubs.h"

#include <QtGui/QGuiApplication>
#include <QtQuick/qquickview.h>
#include "MyDynamicQObject.h"

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
    /*
    QQmlContext *ctxt = view.rootContext();
    registerContext(QString("rootContext"), ctxt);
    doCaml();

    QVariant v = ctxt->contextProperty(QString("controller"));
    Q_ASSERT(v.isValid());
    QObject* obj = v.value<QObject*>();
*/

    MyDynamicQObject *my = new MyDynamicQObject();
    //my->tr("");

    //qDebug() << "classname = " << my->metaObject()->className();
    //MyDynamicQObject * lmyClass = qobject_cast<MyDynamicQObject *>(obj);
    QMetaObject::invokeMethod(my, "slot1",  Qt::DirectConnection);
    return app.exec();

    view.setSource(QUrl::fromLocalFile(QString("Root.qml")));
    view.show();
    qDebug() << "app starts";
    return app.exec();
}
