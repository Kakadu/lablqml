#include "lablqml.h"
#include "variant.h"

#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <assert.h>

#include "object.h"

#include <QMutexLocker>

OCamlObject::OCamlObject(QObject *parent)
: QObject(parent), ocaml_function(NULL), mlvalue()
{
}

void OCamlObject::bind(value func) {
  ocaml_function = (value*) malloc(sizeof(func));
  *ocaml_function = func;
  caml_register_global_root(ocaml_function);
}

OCamlObject::~OCamlObject() {
  if (ocaml_function != NULL) {
    caml_remove_global_root(ocaml_function);
    free(ocaml_function);
  }
}

void OCamlObject::write(QVariant v) {
  CAMLparam0();
  CAMLlocal1(variant_val);

  if (ocaml_function != NULL) {
    caml_leave_blocking_section();
    caml_callback(*(this->ocaml_function), Val_QVariant(variant_val, v));
    caml_enter_blocking_section();
  }

  mlvalue = v;

  CAMLreturn0;
}

QVariant OCamlObject::read() {
  return mlvalue;
}

void OCamlObject::fromOCaml(QVariant v) {
  qDebug()
    << "assign: " << objectName ()
    << " value:" << v;
  mlvalue = v;
}

extern "C" {

  void free_qml_ocaml_object(value s){
    //delete Ctype_of_val(OCamlObject, s);
  }

  static struct custom_operations ops = {
    "lablqml.qml.property",
    free_qml_ocaml_object,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
  };

  value caml_qml_ocaml_object(value qt_object_val, value func_val) {
    CAMLparam2(qt_object_val, func_val);
    CAMLlocal1(result_val);

    QObject *o = Ctype_field(QObject, qt_object_val, 0);
    Q_ASSERT(o != nullptr);

    qDebug () << "bound to: " << o->metaObject()->className();

    OCamlObject *binding = (OCamlObject *)o;
    binding->bind(func_val);

    result_val = caml_alloc_custom(&ops, sizeof(OCamlObject*), 0, 1);
    Ctype_of_val(OCamlObject, result_val) = binding;
    CAMLreturn(result_val);
  }

  value caml_qml_ocaml_object_value(value ocaml_object_val) {
    CAMLparam1(ocaml_object_val);
    CAMLlocal1(result_variant_val);

    OCamlObject *binding = Ctype_of_val(OCamlObject, ocaml_object_val);
    Q_ASSERT(binding != nullptr);
    QVariant ret = binding->read();

    CAMLreturn(Val_QVariant(result_variant_val, ret));
  }

  value caml_qml_ocaml_object_assign(value ocaml_object_val, value value_val) {
    CAMLparam2(ocaml_object_val, value_val);

    OCamlObject *binding = Ctype_of_val(OCamlObject, ocaml_object_val);
    Q_ASSERT(binding != nullptr);

    bool written = QMetaObject::invokeMethod
      (binding, "fromOCaml", Qt::QueuedConnection, Q_ARG(QVariant, QVariant_val(value_val)));
    if (written)
      CAMLreturn(Val_true);
    else
      CAMLreturn(Val_false);
  }

} // Extern C
