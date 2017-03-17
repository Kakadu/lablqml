#include "lablqml.h"
#include "variant.h"

#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <assert.h>

#include "object.h"

extern "C" void ocamlobject_callback (value *ocaml_function, QVariant v);

OCamlBinding::OCamlBinding(QObject *obj, const QString &name, value *func):
QObject(obj), ocaml_function(func), property(obj, name)
{
  property.connectNotifySignal(this, SLOT(valueChanged()));
}

OCamlBinding::~OCamlBinding(){
  if (ocaml_function != NULL) {
    caml_acquire_runtime_system();
    caml_remove_global_root(ocaml_function);
    caml_release_runtime_system();
    free(ocaml_function);
  }
  qDebug () << "destroyed object for " << property.name();
}

bool OCamlBinding::write(QVariant v) {
  return property.write(v);
}

void OCamlBinding::valueChanged () {
  if (ocaml_function != NULL) {
    caml_acquire_runtime_system();
    ocamlobject_callback (ocaml_function, property.read ());
    caml_release_runtime_system();
  }
}

OCamlObject::OCamlObject(QObject *parent):
QObject(parent)
{
  caml_c_thread_register();
}

OCamlObject::~OCamlObject(){
  caml_c_thread_unregister();
  qDebug () << "object destroyed";
}

bool OCamlObject::write(QQmlProperty property, QVariant v) {
  return property.write(v);
}

extern "C" {

  void free_ocaml_binding_object(value v){
    QMetaObject::invokeMethod (Ctype_of_val(OCamlBinding, v), "deleteLater", Qt::QueuedConnection);
  }

  static struct custom_operations ops = {
    "lablqml.qml.property",
    free_ocaml_binding_object,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
  };

  value ocamlobject_binding(value create, value qt_object_val, value property_name_val, value func_val) {
    CAMLparam4(create, qt_object_val, property_name_val, func_val);
    CAMLlocal1(result_val);

    QObject *o = Ctype_field(QObject, qt_object_val, 0);
    Q_ASSERT(o != nullptr);

    if (!Bool_val(create)) {
      QVariant property_object = o->property(String_val(property_name_val));
      if (!property_object.isValid())
	caml_failwith("Property not found");
    }else{
      caml_failwith("Implicit creation not supported yet");
    }

    value *ocaml_function = (value*) malloc(sizeof(func_val));
    *ocaml_function = func_val;
    caml_register_global_root(ocaml_function);

    OCamlBinding *binding = new OCamlBinding ((OCamlObject *)o, String_val(property_name_val), ocaml_function);

    result_val = caml_alloc_custom(&ops, sizeof(OCamlBinding*), 0, 1);
    Ctype_of_val(OCamlBinding, result_val) = binding;

    CAMLreturn(result_val);
  }

  // Warning: synchronously reading into Qt from another thread can cause corruption
  //value caml_qml_ocaml_object_value(value ocaml_object_val) {
  //  CAMLparam1(ocaml_object_val);
  //  CAMLlocal1(result_variant_val);
  //
  //  OCamlObject *binding = Ctype_of_val(OCamlObject, ocaml_object_val);
  //  Q_ASSERT(binding != nullptr);
  //  QVariant ret = binding->read(); // Synchronous read..
  //
  //  CAMLreturn(Val_QVariant(result_variant_val, ret));
  //}

  value ocamlobject_write(value binding_val, value value_val) {
    CAMLparam2(binding_val, value_val);

    auto *b = Ctype_of_val(OCamlBinding, binding_val);
    Q_ASSERT(b != nullptr);

    bool written = QMetaObject::invokeMethod
      (b, "write", Qt::QueuedConnection,
       Q_ARG(QVariant, QVariant_val(value_val)));

    CAMLreturn(written? Val_true: Val_false);
  }

  void ocamlobject_callback (value *ocaml_function, QVariant v) {
    CAMLparam0();
    CAMLlocal1(variant_val);
    int x = caml_c_thread_register();
    if(x==1) qDebug () << "an unregistered thread" << x;
    caml_callback(*ocaml_function, Val_QVariant(variant_val, v));
    CAMLreturn0;
  }

} // Extern C
