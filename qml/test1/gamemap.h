#ifndef GAMEMAP_H
#define GAMEMAP_H

#include <QObject>

class GameMap : public QObject
{
    Q_OBJECT
    QString _title;

public:
    explicit GameMap(QObject *parent = 0);
    Q_INVOKABLE int sizex() { return 400; }
    Q_INVOKABLE int sizey() { return 600; }
    Q_PROPERTY(QString title WRITE setTitle READ title);
    QString title() {
        return _title;
    }
    void setTitle(QString& s) {
        _title = s;
    }

signals:

public slots:

};

#endif // GAMEMAP_H
