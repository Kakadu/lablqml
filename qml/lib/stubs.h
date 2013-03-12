#ifndef STUBS_H
#define STUBS_H

#include <QtQuick/qquickview.h>
#include <QtCore/QDebug>

extern "C" {
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/callback.h>
}

extern void registerView(const QString& name, QQmlContext* v);
#endif
