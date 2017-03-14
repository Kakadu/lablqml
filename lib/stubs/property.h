#pragma once

#include <QtCore/QObject>
#include <QString>
#include <QVariant>
#include <QMutex>
#include <QThread>
#include <QQmlProperty>
#include <QQmlPropertyValueSource>
#include <QtCore/QDebug>
#include <lablqml.h>

class PropertyBinding : public QThread {
  Q_OBJECT

  value *ocaml_function;

  public:
  QQmlProperty property;

  QMutex mutex;

  PropertyBinding(QObject *, QString name, value ocaml_function);
  ~PropertyBinding();

  signals:
    void ocamlChanged(QVariant);

  public slots:
    void qtChanged();
    void fromOCaml(QVariant);
    QVariant read();
};
