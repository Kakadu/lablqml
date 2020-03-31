#include "mysingleton.h"

#include <QtCore/QDebug>

MySingleton::MySingleton(QObject *parent) : QObject(parent)
{
    qDebug() << Q_FUNC_INFO;
}
