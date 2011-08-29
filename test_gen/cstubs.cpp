#include <Qt/QtOpenGL>
#include "headers.h"
extern "C" {
value make_root_widget() {
  CAMLparam0();
  CAMLlocal1(_ans);
  QWidget *ans = new QWidget();
  _ans = (value)ans;
  CAMLreturn(_ans);
}

value ml_QApplication (value argv) {
  CAMLparam1(argv);
  CAMLlocal1(_ans);
  int argc = Wosize_val(argv);
  char **copy = new char*[argc];
  for (int i = 0; i < argc; i++) {
    int l = string_length(Field(argv,i));
    copy[i] = strcpy (new char[l+1], String_val(Field(argv,i)));
  }
  _ans = (value)(new QApplication (argc, copy));
  printf("QApplication created\n");
  CAMLreturn(_ans);
}

value ml_QApplication_exec (value self) {
  CAMLparam1(self);
  return Val_int (((QApplication*)self)->exec());
}

}  // extern "C"
