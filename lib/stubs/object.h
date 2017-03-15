#pragma once

#include <QtCore/QObject>
#include <QString>
#include <QVariant>
#include <QtCore/QDebug>
#include <lablqml.h>

class OCamlObject : public QObject {
  Q_OBJECT
  Q_PROPERTY (QVariant mlvalue READ read WRITE write NOTIFY changed)

  value *ocaml_function;

  public:
  QVariant mlvalue;

  OCamlObject (QObject *parent = 0);
  ~OCamlObject ();
  void bind (value func);
  QVariant read ();
  void write (QVariant v);

 public slots:
  void fromOCaml (QVariant v);

 signals:
    void changed (QVariant);
};
