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

class PropertyBinding : public QObject {
  Q_OBJECT
  Q_PROPERTY (QVariant mlvalue READ read WRITE setMlValue NOTIFY mlvalueChanged)

  value *ocaml_function;

  public:
  QVariant mlvalue;

  PropertyBinding (QObject *parent = 0);
  ~PropertyBinding ();
  void bind (value func);
  QVariant read ();
  void setMlValue (QVariant v);

 public slots:
  void fromOCaml  (QVariant v);

 signals:
    void mlvalueChanged (QVariant);
};
