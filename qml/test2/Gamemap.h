#ifndef Gamemap_H
#define Gamemap_H

#include <QObject>
#include <QDebug>
#include <kamlo.h>

class Gamemap : public QObject {
  Q_OBJECT
public:
public:
  Q_PROPERTY(QString title WRITE setTitle READ title NOTIFY titleChanged)
  QString title() {
    value *closure = caml_named_value("prop_Gamemap_title_get");
    Q_ASSERT_X(closure!=NULL,"Gamemap::title","ocaml's closure `prop_Gamemap_title_get` not found");
    value _ans = caml_callback(*closure, Val_unit);
    return QString(String_val(_ans));
  }
  void setTitle(QString x0) {
    value *closure = caml_named_value("prop_Gamemap_setTitle_set");
    Q_ASSERT_X(closure!=NULL,"Gamemap::setTitle","ocaml's closure `prop_Gamemap_setTitle_set` not found");
    value *args = new value[1];
    args[0] = caml_copy_string(x0.toLocal8Bit().data() );
    // delete args or not?
    caml_callbackN(*closure, 1, args);
  }
signals:
  void titleChanged();
public:
  Q_PROPERTY(int width WRITE setWidth READ width NOTIFY widthChanged)
  int width() {
    value *closure = caml_named_value("prop_Gamemap_width_get");
    Q_ASSERT_X(closure!=NULL,"Gamemap::width","ocaml's closure `prop_Gamemap_width_get` not found");
    int _ans = caml_callback(*closure, Val_unit);
    return Int_val(_ans);
  }
  void setWidth(int x0) {
    value *closure = caml_named_value("prop_Gamemap_setWidth_set");
    Q_ASSERT_X(closure!=NULL,"Gamemap::setWidth","ocaml's closure `prop_Gamemap_setWidth_set` not found");
    value *args = new value[1];
    args[0] = Val_int (x0);
    // delete args or not?
    caml_callbackN(*closure, 1, args);
  }
signals:
  void widthChanged();
public:
  explicit Gamemap(QObject *parent = 0) : QObject(parent) {}
};
#endif

