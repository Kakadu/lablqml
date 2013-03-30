/*
 * Generated at 2013-03-30 14:13:59.889395
 */
#ifndef Controller_c_H
#define Controller_c_H
#include "kamlo.h"
#include <QtCore/QDebug>
#include <QtCore/QObject>

class Controller: public QObject {
  Q_OBJECT
public:
  Controller();
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
