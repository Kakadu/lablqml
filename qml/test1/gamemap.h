#ifndef GAMEMAP_H
#define GAMEMAP_H

#include <QObject>
#include "kamlo.h"

class GameMap : public QObject
{
    Q_OBJECT
    QString _title;

public:
    explicit GameMap(QObject *parent = 0);
    Q_INVOKABLE int sizex() {
        CAMLparam0();
        CAMLlocal1(ans);
        static value* closure_f = NULL;
        if (closure_f == NULL)
            closure_f = caml_named_value("test function");

        ans = caml_callback(*closure_f, Val_unit);
        int _ans = Int_val(ans);
        return _ans;
    }
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
