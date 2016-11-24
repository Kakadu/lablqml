#pragma once

#include <QtCore/QObject>
#include <QString>
#include <QtCore/QDebug>
#include "stubs.h"

class QmlString : public QObject {
  Q_OBJECT
  value *ocaml_function;

  public:
    QmlString(value ocaml_function);
    ~QmlString();

  public slots:
    void valueChanged(QString);
};
