#include "lablqml.h"

#include "variant.h"
#include "CamlPropertyMap.h"

#include <QtCore/QDebug>
#include <QtQml/QQmlEngine>

extern "C" {

#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>

/* Custom CamlPropertyMap block freeing operations */

void free_qml_propertymap(value camlmap){
  CamlPropertyMap *map = (*(CamlPropertyMap**) (Data_custom_val(camlmap))); 
  free(map->_saved_callback);
  delete map;
}

static struct custom_operations camlpropertymap_ops = {
  "lablqml.qml.propertymap",
  free_qml_propertymap,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

value caml_create_QQmlPropertyMap(value _func, value _unit) {
    CAMLparam2(_func, _unit);
    CAMLlocal1(_ans);

    value *fv = (value*) malloc(sizeof(_func));
    *fv = _func;
    caml_register_global_root(fv);
    
    CamlPropertyMap *propMap = new CamlPropertyMap();
    _ans = caml_alloc_custom(&camlpropertymap_ops, sizeof(CamlPropertyMap*), 0, 1);
    (*((CamlPropertyMap **) Data_custom_val(_ans))) = propMap;
    propMap->saveCallback(fv);

    QObject::connect(propMap, &CamlPropertyMap::valueChanged,
                     [fv](const QString& propName, const QVariant& var) {
                       caml_leave_blocking_section();

                       [&fv, &propName, &var]() {
                         CAMLparam0();
                         CAMLlocal2(_nameArg, _variantArg);
                         _nameArg = caml_copy_string( propName.toLocal8Bit().data() );
                         caml_callback2(*fv, _nameArg, Val_QVariant(_variantArg, var) );
                         CAMLreturn0;
                       }();

                       caml_enter_blocking_section();
                     } );

    CAMLreturn(_ans);
}

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

}
