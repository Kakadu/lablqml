#ifndef STUBS_H
#define STUBS_H

extern "C" {
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/threads.h>
}

#include <QtCore/QAbstractItemModel>
#include <QtCore/QString>
#include <QtQml/QQmlContext>

#define Ctype_of_val(T, V) (*((T **) Data_custom_val(V)))

extern void registerContext(const QString& name, QQmlContext* v);
extern QModelIndex make_qmodelindex4(int, int, void*, const QAbstractItemModel *);

#define Val_none Val_int(0)

#endif // STUBS_H
