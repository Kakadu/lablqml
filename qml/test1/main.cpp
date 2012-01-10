#include <QtGui/QApplication>
#include <QDeclarativeContext>
#include "gamemap.h"
#include <QtDeclarative/QDeclarativeView>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    caml_main(argv);
    QDeclarativeView viewer;
    GameMap map;
    viewer.rootContext() -> setContextProperty("gameMap",&map);
    viewer.setSource(QUrl("qml/main.qml"));
    viewer.show();
    return app.exec();
}
