#pragma once

#include <QtCore/QObject>
#include <QQmlProperty>
#include <QString>
#include <QtCore/QDebug>
#include <lablqml.h>

class PropertyBinding : public QObject {
  Q_OBJECT
  value *ocaml_function;

  public:
  QQmlProperty property;

  PropertyBinding(QObject *, QString name, value ocaml_function);
  ~PropertyBinding();

  public slots:
    void valueChanged();
};
