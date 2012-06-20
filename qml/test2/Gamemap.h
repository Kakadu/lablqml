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
  QString title();
  void setTitle(QString x0);
signals:
  void titleChanged();
public:
  Q_PROPERTY(int width WRITE setWidth READ width NOTIFY widthChanged)
  int width();
  void setWidth(int x0);
signals:
  void widthChanged();
public:
  explicit Gamemap(QObject *parent = 0) : QObject(parent) {}
};
#endif

