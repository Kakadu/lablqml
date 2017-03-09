#pragma once

#include <QtCore/QObject>
#include <QMutex>
#include <QQmlProperty>
#include <QString>
#include <QtCore/QDebug>
#include <lablqml.h>

class PropertyBinding : public QObject {
  Q_OBJECT
  value *ocaml_function;

  public:
  QQmlProperty property;
  mutable QMutex mutex;

  PropertyBinding(QObject *, QString name, value ocaml_function);
  ~PropertyBinding();

  public slots:
    void valueChanged();
};
