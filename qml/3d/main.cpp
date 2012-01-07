#include <QtGui/QApplication>
#include <QDeclarativeContext>
#include "gamemap.h"
#include <QtDeclarative/QDeclarativeView>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    QDeclarativeView viewer;
    GameMap map;
    viewer.rootContext() -> setContextProperty("gameMap",&map);
    //viewer.setResizeMode(QDeclarativeView::SizeRootObjectToView);
    viewer.setSource(QUrl("qml/3d/main.qml"));
    viewer.show();
    return app.exec();
}
