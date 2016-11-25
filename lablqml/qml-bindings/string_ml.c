#include "stubs.h"

#include <qml-bindings/object.h>
#include <qml-bindings/property.h>
#include <qml-bindings/string.h>

QmlString::QmlString(value func)
 :QObject(), ocaml_function()
{
  ocaml_function = (value*) malloc(sizeof(func));
  *ocaml_function = func;
  caml_register_global_root(ocaml_function);
}

QmlString::~QmlString(){
  free(ocaml_function);
}

void QmlString::valueChanged(QString v) {
  qDebug() << "changed" << v;
  caml_leave_blocking_section();

  //  [&this, &v]() {
    CAMLparam0();
    CAMLlocal1(caml_value);
    caml_value = caml_copy_string(v.toLocal8Bit().data());
    caml_callback(*(this->ocaml_function), caml_value);
    CAMLreturn0;
    //}();

  caml_enter_blocking_section();
}

extern "C" {

#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <assert.h>

void free_qml_string(value s){
  delete Ctype_of_val(QmlString, s);
}

static struct custom_operations qml_string_ops = {
  "lablqml.qml.string",
  free_qml_string,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

  //qml_engine.rootObjects().first()->
  //  QObject *parent = Ctype_of_val(QObject, parent_object) -> findChild<QObject*> (String_val(child_name));
  //assert (parent!=0);

value caml_qml_string_binding(value qt_object, value property_name, value func) {
  CAMLparam3(qt_object, property_name, func);
  CAMLlocal1(result);

  QObject *parent = Ctype_of_val(QObject, qt_object);
  Q_ASSERT(parent != nullptr);
  QmlString *str = new QmlString(func);
  QQmlProperty property (parent, String_val(property_name));
  property.connectNotifySignal(str, SLOT(valueChanged(QString)));

  result = caml_alloc_custom(&qml_string_ops, sizeof(QmlString*), 0, 1);
  Ctype_of_val(QmlString, result) = str;

  CAMLreturn(result);
}
  /*
value caml_QQmlPropertyMap_value(value _map, value _propName) {
    CAMLparam2(_map, _propName);
    CAMLlocal1(_ans);

    CamlPropertyMap *map = (*(CamlPropertyMap**) (Data_custom_val(_map)));
    Q_ASSERT_X(map != NULL, __func__, "Trying to use QQmlPropertyMap object which is NULL");

    const QVariant& ans = map->value(QString( String_val(_propName) ));

    _ans = Val_QVariant(_ans, ans);
    CAMLreturn(_ans);
}

value caml_QQmlPropertyMap_insert(value _map, value _propName, value _variant) {
    CAMLparam3(_map, _propName, _variant);

    // copy and paste from the generated file for QAbstractModel subclass
    // TODO: move this conversion to the lablqml
    QVariant newval;
    if (Is_block(_variant)) {
        if (caml_hash_variant("bool") == Field(_variant,0) )
            // without cast it will create Qvariant of int
            newval = QVariant::fromValue( (bool)Bool_val(Field(_variant,1)) );
        else if (caml_hash_variant("string") == Field(_variant,0) )
            newval = QVariant::fromValue(QString(String_val(Field(_variant,1))));
        else if (caml_hash_variant("int") == Field(_variant,0) )
            newval = QVariant::fromValue(Int_val(Field(_variant,1)));
        else if (caml_hash_variant("float") == Field(_variant,0) )
            newval = QVariant::fromValue(Double_val(Field(_variant,1)));
        else if (caml_hash_variant("qobject") == Field(_variant,0) )
            newval = QVariant::fromValue((QObject*) (Field(Field(_variant,1),0)));
        else
            Q_ASSERT_X(false, "While converting OCaml value to QVariant",
                       "Unknown variant tag");
    } else { // empty QVariant
        newval = QVariant();
    }

    CamlPropertyMap *map = (*(CamlPropertyMap**) (Data_custom_val(_map)));
    Q_ASSERT_X(map != NULL, __func__, "Trying to use QQmlPropertyMap object which is NULL");
    map->insert( QString(String_val(_propName)), newval);

    CAMLreturn(Val_unit);
}
  */
} // Extern C
