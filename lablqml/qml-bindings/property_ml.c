#include "stubs.h"

#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <assert.h>

#include <qml-bindings/object.h>
#include <qml-bindings/property.h>

PropertyBinding::PropertyBinding(QObject *o, QString name, value func)
:QObject(), ocaml_function(), property(o, name)
{
  ocaml_function = (value*) malloc(sizeof(func));
  *ocaml_function = func;
  caml_register_global_root(ocaml_function);

  property.connectNotifySignal(this, SLOT(valueChanged()));
  qDebug() << "property:" << property.read();
}

PropertyBinding::~PropertyBinding(){
  free(ocaml_function);
}

void PropertyBinding::valueChanged() {
  qDebug() << "changed";
}

void PropertyBinding::valueChanged(QString v) {
  qDebug() << "changed" << v;
  caml_leave_blocking_section();

    //[&this, &v]() {
    CAMLparam0();
    CAMLlocal1(caml_value);
    caml_value = caml_copy_string(v.toLocal8Bit().data());
    caml_callback(*(this->ocaml_function), caml_value);
    CAMLreturn0;
    //}();

  caml_enter_blocking_section();
}

extern "C" {

void free_qml_property_binding(value s){
  delete Ctype_of_val(PropertyBinding, s);
}

static struct custom_operations qml_property_ops = {
  "lablqml.qml.property",
  free_qml_property_binding,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

value caml_qml_property_binding(value qt_object_val, value property_name_val, value func_val) {
  CAMLparam3(qt_object_val, property_name_val, func_val);
  CAMLlocal1(result_val);

  QObject *o = Ctype_field(QObject, qt_object_val, 0);
  Q_ASSERT(o != nullptr);
  PropertyBinding *p = new PropertyBinding(o, String_val(property_name_val), func_val);

  result_val = caml_alloc_custom(&qml_property_ops, sizeof(PropertyBinding*), 0, 1);
  Ctype_of_val(PropertyBinding, result_val) = p;
  CAMLreturn(result_val);
}

} // Extern C
