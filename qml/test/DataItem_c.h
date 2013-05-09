/*
 * Generated at 2013-05-09 12:56:37.803365+04:00
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
  Q_INVOKABLE QString name();
  Q_INVOKABLE QString sort();
};
#endif // DataItem_H
