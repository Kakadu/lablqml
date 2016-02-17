#include "stubs.h"

#include <QQmlPropertyMap>

extern "C" value caml_create_QQmlPropertyMap(value _unit) {
    CAMLparam1(_unit);
    CAMLlocal1(_ans);
    caml_enter_blocking_section();

    QQmlPropertyMap *propMap = new QQmlPropertyMap();
    _ans = caml_alloc_small(1, Abstract_tag);
    (*((QQmlPropertyMap **) &Field(_ans, 0))) = propMap;

    caml_leave_blocking_section();
    CAMLreturn(_ans);
}

extern "C" value caml_QQmlPropertyMap_insert(value _map, value _propName, value _variant) {
    CAMLparam3(_map, _propName, _variant);

    QQmlPropertyMap *map = (QQmlPropertyMap*) (Field(_map,0));
    Q_ASSERT_X(map != NULL, __func__, "Trying to use QQmlPropertyMap object which is NULL");

    // copy and paste from the generated file for QAbstractModel subclass
    // TODO: move this conversion to the lablqml
    QVariant newval;
    if (Is_block(_variant)) {
        if (caml_hash_variant("string") == Field(_variant,0) )
            newval = QVariant::fromValue(QString(String_val(Field(_variant,1))));
        else if (caml_hash_variant("int") == Field(_variant,0) )
            newval = QVariant::fromValue(Int_val(Field(_variant,1)));
        else if (caml_hash_variant("qobject") == Field(_variant,0) )
            newval = QVariant::fromValue((QObject*) (Field(Field(_variant,1),0)));
        else Q_ASSERT_X(false,"While converting OCaml value to QVariant","Unknown variant tag");
    } else { // empty QVariant
        newval = QVariant();
    }

    map->insert( QString(String_val(_propName)), newval);

    CAMLreturn(Val_unit);
}
