/*
 * Generated at 16 Jul, 2018 20:52:40
 */
#ifndef CONTROLLER_H
#define CONTROLLER_H

#include <QtCore/QDebug>
#include <QtCore/QObject>
#include <QtCore/QAbstractItemModel>

#ifdef __cplusplus
extern "C" {
#endif
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/threads.h>
#ifdef __cplusplus
}
#endif

class controller : public QObject {
  Q_OBJECT
  value _camlobjHolder; // store reference to OCaml value there
public:
  controller() : _camlobjHolder(0) { };
  void storeCAMLobj(value x) {
    if (_camlobjHolder != 0) {
       //maybe unregister global root?
    }
    _camlobjHolder = x;
    register_global_root(&_camlobjHolder);
  }

public:
  Q_PROPERTY(QString descr READ getdescr NOTIFY descrChanged)
  Q_INVOKABLE QString getdescr();
signals:
  void descrChanged(QString descr);
};
#endif /* CONTROLLER_H */

extern "C" value caml_create_controller(value _dummyUnitVal);
extern "C" value caml_store_value_in_controller(value _cppobj,value _camlobj);
