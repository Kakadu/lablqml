#pragma once

extern "C" {
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/fail.h>
}

#include <QtCore/QAbstractItemModel>
#include <QtCore/QString>
#include <QtQml/QQmlContext>

#define Ctype_of_val(T, V) (*((T **) Data_custom_val(V)))
#define Ctype_field(T, B, I) ((*((T **) &Field(B, I))))

extern void registerContext(const QString& name, QQmlContext* v);
extern QModelIndex make_qmodelindex4(int, int, void*, const QAbstractItemModel *);

#define Val_none Val_int(0)

static value Val_some(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);

  some = caml_alloc(1, 0);
  Store_field(some, 0, v);

  CAMLreturn(some);
}
