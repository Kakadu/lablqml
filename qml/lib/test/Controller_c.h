/*
 * Generated at 2013-03-31 00:17:18.872570
 */
#ifndef Controller_c_H
#define Controller_c_H
#include "kamlo.h"
#include <QtCore/QDebug>
#include <QtCore/QObject>

class Controller: public QObject {
  Q_OBJECT
  value _camlobjHolder = 0;
public:
  Controller();
  void storeCAMLobj(value x) {
    if (_camlobjHolder != 0) {
       //maybe unregister global root?
    }
    _camlobjHolder = x;
    register_global_root(&_camlobjHolder);
  }
  Q_INVOKABLE void onItemSelected(int,int);
public:
  Q_PROPERTY(bool hasData  READ isHasData NOTIFY hasDataChanged)
  Q_INVOKABLE bool isHasData();
signals:
  void hasDataChanged(bool);
public:
  void emit_hasDataChanged(bool arg1) {
    emit hasDataChanged(arg1);
  }

public:
  Q_PROPERTY(QString descr  READ getDescr NOTIFY descChanged)
  Q_INVOKABLE QString getDescr();
signals:
  void descChanged(QString);
public:
  void emit_descChanged(QString arg1) {
    emit descChanged(arg1);
  }

};
#endif // Controller_H
