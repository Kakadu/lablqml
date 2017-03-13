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

class PropertyBinding : public QThread, public QQmlPropertyValueSource {
  Q_OBJECT
//Q_INTERFACES(QQmlPropertyValueSource)

  value *ocaml_function;

  public:
  QQmlProperty property;

  QMutex mutex;

  PropertyBinding(QObject *, QString name, value ocaml_function);
  ~PropertyBinding();

  virtual void setTarget(const QQmlProperty &p) { property = p; }

  signals:
    void ocamlChanged(QVariant);

  public slots:
    void qtChanged();
};
