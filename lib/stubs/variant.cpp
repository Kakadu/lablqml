#include "variant.h"

extern "C" {

  QVariant QVariant_val(value variant_val) {
    CAMLparam1(variant_val);
    QVariant v;
    if (Is_block(variant_val)) {
        if (caml_hash_variant("bool") == Field(variant_val, 0))
          // without cast it will create Qvariant of int
	  v = QVariant::fromValue( (bool) Bool_val(Field(variant_val, 1)) );
        else if (caml_hash_variant("string") == Field(variant_val, 0) )
	  v = QVariant::fromValue(QString(String_val(Field(variant_val, 1))));
        else if (caml_hash_variant("int") == Field(variant_val, 0) )
	  v = QVariant::fromValue(Int_val(Field(variant_val, 1)));
        else if (caml_hash_variant("float") == Field(variant_val, 0) )
	  v = QVariant::fromValue(Double_val(Field(variant_val, 1)));
        else if (caml_hash_variant("qobject") == Field(variant_val, 0) )
	  v = QVariant::fromValue((QObject*) (Field(Field(variant_val, 1),0)));
        else
            Q_ASSERT_X(false, "While converting OCaml value to QVariant",
                       "Unknown variant tag");
    }
    CAMLreturnT (QVariant, v);
  }

  // converts QVariant to OCaml QVariant.t. Should be ported to the lablqml
  /*
  // I intentionally skipped CAMLlocalN because intuition says that they are
  // not needed because it is done in caller function
  */
  value Val_QVariant(value _dest, const QVariant& var) {
    CAMLparam1(_dest);
    CAMLlocal1(_var);

    if (!var.isValid()) {
        _dest =caml_hash_variant("empty");
    } else {
        const int ut = var.userType();

        switch (ut) {
        case QMetaType::Bool:
            _dest = caml_alloc(2, 0);
            Store_field(_dest, 0,caml_hash_variant("bool"));
            Store_field(_dest, 1, Val_bool(var.toBool()));
            break;
        case QMetaType::QString:
            _dest = caml_alloc(2, 0);
            Store_field(_dest, 0,caml_hash_variant("string"));
            Store_field(_dest, 1, caml_copy_string(var.value<QString>().toLocal8Bit().data()));
            break;
        case QMetaType::Int:
            _dest = caml_alloc(2, 0);
            Store_field(_dest, 0,caml_hash_variant("int"));
            Store_field(_dest, 1, Val_int(var.value<int>()));
            break;
        case QMetaType::Float:
        case QMetaType::Double:
            _dest = caml_alloc(2, 0);
            Store_field(_dest, 0,caml_hash_variant("float"));
            Store_field(_dest, 1, caml_copy_double(var.toFloat()));
            break;
        case QMetaType::User:
        case QMetaType::QObjectStar:
            {
              QObject *vvv = var.value<QObject*>();
              _var = caml_alloc_small(1,Abstract_tag);
              (*((QObject **) &Field(_var, 0))) = vvv;
              _dest = caml_alloc(2,0);
              Store_field(_dest, 0,caml_hash_variant("qobject"));
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
