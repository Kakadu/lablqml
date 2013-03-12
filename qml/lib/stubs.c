#include "stubs.h"


void registerView(const QString& name, QQmlContext* v) {
  qDebug() << "void registerView(....)";
  //CAMLparam0();
  static value *closure = nullptr;
  if (closure == nullptr) {
    closure = caml_named_value("register_view");
  }
  Q_ASSERT(closure!=nullptr);
  static value _name;
  static value _view;
  _name = caml_copy_string(name.toStdString().c_str());
  _view = caml_alloc_small(1, Abstract_tag);
  (*((QQmlContext **) &Field(_view, 0))) = v;
  static value _ans;
  _ans = caml_callback2(*closure, _name, _view); // should be a unit
  Q_UNUSED(_ans);
  //CAMLreturn0;
}

extern "C" value caml_foo(value x) {
    CAMLparam1(x);

    CAMLreturn(Val_unit);
}
