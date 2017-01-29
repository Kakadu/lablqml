#include <lablqml.h>
#include <QVariant>

extern "C" {
  QVariant QVariant_val(value variant_val);
  value Val_QVariant(value _dest, const QVariant& var);
}
