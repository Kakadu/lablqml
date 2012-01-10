#ifndef GAMEMAP_H
#define GAMEMAP_H

#include <QObject>
#include <QDebug>
#include "kamlo.h"

class GameMap : public QObject
{
    Q_OBJECT
    QString _title;

public:
    explicit GameMap(QObject *parent = 0);

    Q_INVOKABLE int sizex();

    Q_INVOKABLE int sizey() { return 600; }
    Q_PROPERTY(QString title WRITE setTitle READ title NOTIFY titleChanged)

    QString title() {
        return _title;
    }
    void setTitle(QString& s) {
        _title = s;
    }

signals:
    void titleChanged ();

public slots:

};

#endif // GAMEMAP_H
