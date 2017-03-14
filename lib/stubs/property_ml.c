#include "lablqml.h"
#include "variant.h"

#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <assert.h>

#include "property.h"

#include <QMutexLocker>

PropertyBinding::PropertyBinding(QObject *o, QString name, value func)
: QThread(), ocaml_function(), property(o, name)
{
  ocaml_function = (value*) malloc(sizeof(func));
  *ocaml_function = func;
  caml_register_global_root(ocaml_function);

  property.connectNotifySignal(this, SLOT(qtChanged()));
}

PropertyBinding::~PropertyBinding(){
  free(ocaml_function);
}

QVariant PropertyBinding::read() {
  QMutexLocker locker(&mutex);
  return property.read();
}

void PropertyBinding::qtChanged() {
  CAMLparam0();
  CAMLlocal1(variant_val);

  caml_leave_blocking_section();
  caml_callback(*(this->ocaml_function), Val_QVariant(variant_val, property.read()));
  caml_enter_blocking_section();

  CAMLreturn0;
}

void PropertyBinding::fromOCaml(QVariant v) {
    QMutexLocker locker(&mutex);

    if (property.object()->thread () != QThread::currentThread())
      qDebug()
	<< "assign: " << property.name ()
	<< " object thread:" << property.object()->thread ()
	<< " current thread:" << QThread::currentThread();
    property.write(v);
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

  value caml_qml_property_binding(value create, value qt_object_val, value property_name_val, value func_val) {
    CAMLparam4(create, qt_object_val, property_name_val, func_val);
    CAMLlocal1(result_val);

    QObject *o = Ctype_field(QObject, qt_object_val, 0);
    Q_ASSERT(o != nullptr);

    if (!Bool_val(create)) {
      QVariant property_object = o->property(String_val(property_name_val));
      if (!property_object.isValid())
	caml_failwith("Property not found");
    }

    PropertyBinding *p = new PropertyBinding(o, String_val(property_name_val), func_val);
    p->start();

    result_val = caml_alloc_custom(&qml_property_ops, sizeof(PropertyBinding*), 0, 1);
    Ctype_of_val(PropertyBinding, result_val) = p;
    CAMLreturn(result_val);
  }

  value caml_qml_property_binding_value(value property_binding_val) {
    CAMLparam1(property_binding_val);
    CAMLlocal1(result_variant_val);

    PropertyBinding *binding = Ctype_of_val(PropertyBinding, property_binding_val);
    Q_ASSERT(binding != nullptr);
    QVariant ret = binding->read();

    CAMLreturn(Val_QVariant(result_variant_val, ret));
  }

  value caml_qml_property_binding_assign(value property_binding_val, value value_val) {
    CAMLparam2(property_binding_val, value_val);

    PropertyBinding *binding = Ctype_of_val(PropertyBinding, property_binding_val);
    Q_ASSERT(binding != nullptr);

    bool written = QMetaObject::invokeMethod
      (binding, "fromOCaml", Qt::QueuedConnection, Q_ARG(QVariant, QVariant_val(value_val)));
    if (written)
      CAMLreturn(Val_true);
    else
      CAMLreturn(Val_false);
  }

} // Extern C
