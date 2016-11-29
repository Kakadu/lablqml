#include "variant.h"

extern "C" {
// converts QVariant to OCaml QVariant.t. Should be ported to the lablqml
/*
// I intentionally skipped CAMLlocalN because intuition says that they are
// not needed because it is done in caller function
*/
value Val_QVariant(value _dest, const QVariant& var) {
    CAMLparam1(_dest);
    CAMLlocal1(_var);

    if (!var.isValid()) {
        _dest = hash_variant("empty");
    } else {
        const int ut = var.userType();

        switch (ut) {
        case QMetaType::Bool:
            _dest = caml_alloc(2, 0);
            Store_field(_dest, 0, hash_variant("bool"));
            Store_field(_dest, 1, Val_bool(var.toBool()));
            break;
        case QMetaType::QString:
            _dest = caml_alloc(2, 0);
            Store_field(_dest, 0, hash_variant("string"));
            Store_field(_dest, 1, caml_copy_string(var.value<QString>().toLocal8Bit().data()));
            break;
        case QMetaType::Int:
            _dest = caml_alloc(2, 0);
            Store_field(_dest, 0, hash_variant("int"));
            Store_field(_dest, 1, Val_int(var.value<int>()));
            break;
        case QMetaType::Float:
        case QMetaType::Double:
            _dest = caml_alloc(2, 0);
            Store_field(_dest, 0, hash_variant("float"));
            Store_field(_dest, 1, caml_copy_double(var.toFloat()));
            break;
        case QMetaType::User:
        case QMetaType::QObjectStar:
            {
              QObject *vvv = var.value<QObject*>();
              _var = caml_alloc_small(1,Abstract_tag);
              (*((QObject **) &Field(_var, 0))) = vvv;
              _dest = caml_alloc(2,0);
              Store_field(_dest, 0, hash_variant("qobject"));
              Store_field(_dest, 1, _var);
            }
            break;
        default:
            QString msg("Type is not supported:");
            msg += QString("userType() == %1").arg(ut);
            Q_ASSERT_X(false, __func__, msg.toLocal8Bit().data() );
        }
    }
    CAMLreturn(_dest);
}

}
