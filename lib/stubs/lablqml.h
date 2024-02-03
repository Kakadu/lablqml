#pragma once

#include <QtCore/QAbstractItemModel>
#include <QtCore/QString>
#include <QtCore/QDebug>
#include <QtQml/QQmlContext>


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

extern bool lablqml_check_locks;

#define DEBUG_ENTER_OCAML \
    qDebug() << "ENTER OCAML == release_runtime_lock " << __FILE__ << __LINE__;
#define DEBUG_LEAVE_OCAML \
    qDebug() << "LEAVE OCAML == acquire_runtime_lock" << __FILE__ << __LINE__;

#define LABLQML_USE_LOCKS 1
// #define DEBUG_ENTER_OCAML
// #define DEBUG_LEAVE_OCAML

#define LABLQML_LEAVE_OCAML \
  if (lablqml_check_locks) { \
    DEBUG_LEAVE_OCAML \
    if (LABLQML_USE_LOCKS) caml_acquire_runtime_system(); \
  }

#define LABLQML_ENTER_OCAML \
  if (lablqml_check_locks) { \
    DEBUG_ENTER_OCAML \
    if (LABLQML_USE_LOCKS) caml_release_runtime_system(); \
  }

#define LABLQML_MAYBE_TAKE_LOCK \
  bool __lablqml_need_release_ = (Caml_state_opt == NULL) ? true : false; \
  if (__lablqml_need_release_) caml_acquire_runtime_system();

#define LABLQML_MAYBE_RELEASE_LOCK if (__lablqml_need_release_) caml_release_runtime_system();