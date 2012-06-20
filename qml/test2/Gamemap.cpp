#include "Gamemap.h"
QString Gamemap::title() {
  CAMLparam0();
  CAMLlocal1(_ans);
  value *closure = caml_named_value("prop_Gamemap_title_get_string");
  Q_ASSERT_X(closure!=NULL, "Gamemap::title",
             "ocaml's closure `prop_Gamemap_title_get_string` not found");
  _ans = caml_callback(*closure, Val_unit);
  return QString(String_val(_ans));
}
void Gamemap::setTitle(QString x0) {
  CAMLparam0();
  CAMLlocal1(_ans);
  value *closure = caml_named_value("prop_Gamemap_title_set_string");
  Q_ASSERT_X(closure!=NULL, "Gamemap::setTitle",
             "ocaml's closure `prop_Gamemap_title_set_string` not found");
  value *args = new value[1];
  args[0] = caml_copy_string(x0.toLocal8Bit().data() );
  // delete args or not?
  caml_callbackN(*closure, 1, args);
  CAMLreturn0;
}
int Gamemap::width() {
  CAMLparam0();
  CAMLlocal1(_ans);
  value *closure = caml_named_value("prop_Gamemap_width_get_int");
  Q_ASSERT_X(closure!=NULL, "Gamemap::width",
             "ocaml's closure `prop_Gamemap_width_get_int` not found");
  _ans = caml_callback(*closure, Val_unit);
  return Int_val(_ans);
}
void Gamemap::setWidth(int x0) {
  CAMLparam0();
  CAMLlocal1(_ans);
  value *closure = caml_named_value("prop_Gamemap_width_set_int");
  Q_ASSERT_X(closure!=NULL, "Gamemap::setWidth",
             "ocaml's closure `prop_Gamemap_width_set_int` not found");
  value *args = new value[1];
  args[0] = Val_int (x0);
  // delete args or not?
  caml_callbackN(*closure, 1, args);
  CAMLreturn0;
}
