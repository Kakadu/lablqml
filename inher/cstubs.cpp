#include <Qt/QtGui>
#include "headers.h"

extern "C" {

value ml_qapp_create (value argv) {
  CAMLparam1(argv);
  CAMLlocal1(_ans);
  int argc = Wosize_val(argv);
  char **copy = new char*[argc];
  for (int i = 0; i < argc; i++) {
    int l = string_length(Field(argv,i));
    copy[i] = strcpy (new char[l+1], String_val(Field(argv,i)));
  }
  QApplication *app = new QApplication (argc, copy);
  printf("QApplication created: %x\n", app);
  setAbstrClass(_ans,QApplication,app);
  CAMLreturn(_ans);
}

value ml_qapp_exec (value self) {
  CAMLparam1(self);
  QApplication *app = QApplication_val(self);
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

  CAMLprim
  value setCamlObj(value cppobj, value camlobj) {
    CAMLparam2(cppobj, camlobj);
    printf("setting camlobj = %x, of cppobj_abstr = %x\n", camlobj, cppobj);
    QObject *o = QObject_val(cppobj);
    printf("real cpp object = %x\n", o);
    printf("qobject's classname = %s\n", o->metaObject()->className() );
    o->setProperty(CAMLOBJ_PROPERTY, (qlonglong)camlobj);
    caml_register_global_root(&camlobj);
    CAMLreturn(Val_unit);
  }
  
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
}  // extern "C"
