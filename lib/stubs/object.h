#pragma once

#include <QtCore/QObject>
#include <QString>
#include <QVariant>
#include <QQmlProperty>
#include <QMutex>
#include <QtCore/QDebug>
#include <lablqml.h>

class OCamlBinding : public QObject {
  Q_OBJECT

  value *ocaml_function;
  QQmlProperty property;

 public:
  OCamlBinding (QObject *obj, const QString &name, value func);
  ~OCamlBinding ();

 public slots:
    void valueChanged();
    bool write (QVariant);
};

class OCamlObject : public QObject {
  Q_OBJECT

 public:
  OCamlObject (QObject *parent = 0);
  ~OCamlObject();
 public slots:
   bool write (QQmlProperty property, QVariant value);
};
