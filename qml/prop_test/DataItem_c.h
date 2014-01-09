/*
 * Generated at 0-9 13:38:17
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

public:
  Q_PROPERTY(QString text  READ text NOTIFY textChanged)
  Q_INVOKABLE QString text();
signals:
  void textChanged(QString text);
public:
  void emit_textChanged(QString text) {
    emit textChanged(text);
  }

};
#endif // DataItem_H
