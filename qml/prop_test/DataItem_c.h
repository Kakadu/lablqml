/*
 * Generated at 2013-11-06 23:46:53.841758+04:00
 */
#ifndef DataItem_c_H
#define DataItem_c_H
#include "kamlo.h"
#include <QtCore/QDebug>
#include <QtCore/QObject>
#include <QtWidgets/QGraphicsSceneMouseEvent>
#include <QtGui/QKeyEvent>

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
  Q_PROPERTY(int cellX  READ cellX NOTIFY cellXChanged)
  Q_INVOKABLE int cellX();
signals:
  void cellXChanged(int cellX);
public:
  void emit_cellXChanged(int cellX) {
    emit cellXChanged(cellX);
  }

};
#endif // DataItem_H
