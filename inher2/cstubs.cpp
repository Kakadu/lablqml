#include <Qt/QtGui>
#include <QDebug>
#include "headers.h"

extern "C" {
CAMLprim
value ml_qapp_create (value argv) {
  CAMLparam1(argv);
  CAMLlocal1(_ans);
  int argc = Wosize_val(argv);
  char **copy = new char*[argc+1];
  for (int i = 0; i < argc; i++) {
    int l = string_length(Field(argv,i));
    copy[i] = strdup(String_val(Field(argv,i)));
  }
  copy[argc]=NULL;
  qDebug() << "printing args in constructor";
  for (int i=0; i<argc; ++i) {
    qDebug() << i << ".`" << copy[i] << "`";
  }
  qDebug() << "end of arg list in constructor";
  QApplication *app = new QApplication (argc, copy);
  _ans = caml_alloc(1, Abstract_tag);
  (*((QApplication **) &Field(_ans, 0))) = app;
  printf ("QApplication created : %p\n", (void*)app);
  
  CAMLreturn(_ans);
}
CAMLprim
value ml_qapp_exec (value unit) {
  CAMLparam1(unit);
  printf ("qApp = %p\n", qApp); // qApp is a macros for accessing QCoreApplication::instance()
  printf("preparing before qApp->exec(). arguments are:\n");
  QStringList const& argslist = qApp->arguments();
  foreach (QString const& s, argslist) {
        qDebug() << s ;
    //printf("%s\n", s.toLocal8Bit().data() );
  }
  printf("end of arguments list\n");
  int ret = qApp->exec();
  CAMLreturn(Val_int(ret));
}

}  // extern "C"
