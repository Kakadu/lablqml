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
    copy[i] = strcpy (new char[l+1], String_val(Field(argv,i)));
  }
  copy[argc] = NULL;
  QApplication *app = new QApplication (argc, copy);
  _ans = caml_alloc(1, Abstract_tag);
  (*((QApplication **) &Field(_ans, 0))) = app;
  printf ("QApplication created : %p\n", (void*)app);
  CAMLreturn(_ans);
}
CAMLprim
value ml_qapp_exec (value self) {
  CAMLparam1(self);
  QApplication *app = QApplication_val(self);
  printf ("app=%p gApp=%p\n", app, QApplication::instance());
  printf("preparing before app.exec(). arguments are:\n");
  QStringList argslist = app->arguments();
  foreach (const QString s, argslist) {
    //    qDebug() << s ;
    printf("%s\n", s.toLocal8Bit().data() );
  }
  printf("end of arguments list\n");
  CAMLreturn( Val_int (app->exec()) );
}
  //============================ settting and getting caml object from QObject
  #define CAMLOBJ_PROPERTY "_camlobj"
  value takeCamlObj(const QObject* o) {
    QVariant ans = o -> property(CAMLOBJ_PROPERTY);
    if (ans.isValid())
      return ans.toLongLong();
    else
      return 0;
  }
  /*
  CAMLprim // [`qobject] obj -> 'a option
  value hasCamlObj(value cppobj) {
    CAMLparam1(cppobj);
    CAMLlocal1(ans);
    QObject *o = QObject_val(cppobj);
    long answer = takeCamlObj(o);
    if (answer != 0) {
      setAbstrClass(ans,QObject,(QObject*)answer);
      CAMLreturn( Some_val(ans) );
    }
    else
      CAMLreturn(Val_none);
  }
  */

  CAMLprim
  value setCamlObj(value cppobj, value camlobj) {
    CAMLparam2(cppobj, camlobj);
    printf("setting camlobj = %lld, of cppobj_abstr = %lld\n", camlobj, cppobj);
    QObject *o = QObject_val(cppobj);
    printf("real cpp object = %p\n", (void*)o);
    printf("qobject's classname = %s\n", o->metaObject()->className() );
    o->setProperty(CAMLOBJ_PROPERTY, (qlonglong)camlobj);
    caml_register_global_root(&camlobj);
    CAMLreturn(Val_unit);
  }
  /*
  CAMLprim // [`qobject ] obj -> string option 
  value getClassName(value cppobj) {
    CAMLparam1(cppobj);
    QObject * qobj = QObject_val(cppobj);
    if (qobj == NULL)
      CAMLreturn(Val_none);
    else 
      CAMLreturn(Some_val(caml_copy_string(qobj -> metaObject() -> className() ) ) );
  }
  CAMLprim
  value getNullObject(value x) {
    CAMLparam1(x);
    CAMLlocal1(ans);
    setAbstrClass(ans,QObject,NULL);
    CAMLreturn(ans);
  }

  CAMLprim
  value myQKeyEvent(value x) {
    CAMLparam1(x);
    CAMLlocal1(ans);
    QKeyEvent* event = new QKeyEvent(QKeyEvent::KeyPress, Qt::Key_Escape,Qt::NoModifier);
    setAbstrClass(ans,QKeyEvent, event);
    CAMLreturn(ans);    
  }
  */
}  // extern "C"
