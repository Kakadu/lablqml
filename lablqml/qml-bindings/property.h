#pragma once

#include <QtCore/QObject>
#include <QQmlProperty>
#include <QString>
#include <QtCore/QDebug>
#include "stubs.h"

class PropertyBinding : public QObject {
  Q_OBJECT
  value *ocaml_function;
  QQmlProperty property;

  public:
  PropertyBinding(QObject *, QString name, value ocaml_function);
    ~PropertyBinding();

  public slots:
    void valueChanged();
    void valueChanged(QString);
};
