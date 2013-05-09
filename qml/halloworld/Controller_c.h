/*
 * Generated at 2013-05-09 20:17:43.137060+04:00
 */
#ifndef Controller_c_H
#define Controller_c_H
#include "kamlo.h"
#include <QtCore/QDebug>
#include <QtCore/QObject>

class Controller: public QObject {
  Q_OBJECT
  value _camlobjHolder;
public:
  Controller();
  void storeCAMLobj(value x) {
    if (_camlobjHolder != 0) {
       //maybe unregister global root?
    }
    _camlobjHolder = x;
    register_global_root(&_camlobjHolder);
  }
  Q_INVOKABLE void onMouseClicked();
};
#endif // Controller_H
