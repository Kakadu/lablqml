#include <QtGui/QApplication>
#include <QDeclarativeContext>
#include "gamemap.h"
#include <Qt3DQuick/qdeclarativeview3d.h>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    QDeclarativeView3D viewer;
    GameMap map;
    viewer.rootContext() -> setContextProperty("gameMap",&map);
    //viewer.setResizeMode(QDeclarativeView::SizeRootObjectToView);
    viewer.setSource(QUrl("qml/3d/main.qml"));
    viewer.show();
    return app.exec();
}
