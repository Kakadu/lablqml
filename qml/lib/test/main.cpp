#include "../stubs.h"

#include <QtGui/QGuiApplication>
#include <QtQuick/qquickitem.h>
#include <QtQuick/qquickview.h>

int main(int argc, char ** argv)
{
    caml_main(argv);
    //caml_startup(argv);
    QGuiApplication app(argc, argv);
    QQuickView view;
    view.setResizeMode(QQuickView::SizeRootObjectToView);

    QQmlContext *ctxt = view.rootContext();
    registerContext(QString("myModel"), ctxt);

    //view.setSource(QUrl("qrc:view.qml"));
    //view.show();

    return app.exec();
}
