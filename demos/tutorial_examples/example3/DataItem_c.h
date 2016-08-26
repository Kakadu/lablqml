/*
 * Generated at 2013-05-31 19:13:44.040798+04:00
 */
#ifndef DataItem_c_H
#define DataItem_c_H
#include "kamlo.h"
#include <QtCore/QDebug>
#include <QtCore/QObject>

class DataItem: public QObject {
  Q_OBJECT
  value _camlobjHolder;
public:
  DataItem();
  void storeCAMLobj(value x) {
    if (_camlobjHolder != 0) {
       //maybe unregister global root?
    }
    _camlobjHolder = x;
    register_global_root(&_camlobjHolder);
  }
public:
  Q_PROPERTY(QString author WRITE setName READ author NOTIFY nameChanged)
  Q_INVOKABLE QString author();
  Q_INVOKABLE bool setName(QString);
signals:
  void nameChanged(QString);
public:
  void emit_nameChanged(QString arg1) {
    emit nameChanged(arg1);
  }

public:
  Q_PROPERTY(QString title WRITE setTitle READ title NOTIFY titleChanged)
  Q_INVOKABLE QString title();
  Q_INVOKABLE bool setTitle(QString);
signals:
  void titleChanged(QString);
public:
  void emit_titleChanged(QString arg1) {
    emit titleChanged(arg1);
  }

};
#endif // DataItem_H
