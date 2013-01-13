#include <QtWidgets/QtWidgets>
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
  copy[argc] = NULL;
  int* r_argc = new int(argc);
  QApplication *app = new QApplication (*r_argc, copy);
  _ans = caml_alloc(1, Abstract_tag);
  (*((QApplication **) &Field(_ans, 0))) = app;
  printf ("QApplication created : %p\n", (void*)app);
  CAMLreturn(_ans);
}
CAMLprim
value ml_qapp_exec (value unit) {
  CAMLparam1(unit);
  printf("preparing before app.exec(). arguments are:\n");
  QStringList argslist = QApplication::arguments();
  foreach (const QString s, argslist) {
    printf("%s\n", s.toLocal8Bit().data() );
  }
  printf("end of arguments list\n");
  CAMLreturn( Val_int (QApplication::exec()) );
}

CAMLprim // TODO: think about casting from abstract tags
value ml_QObject_connect (value sender, value signal, value receiver, value member) {
  CAMLparam4(sender,signal,receiver,member);
  char *sg = String_val(signal);
  char *sl = String_val(member);
  int len1 = strlen(sg), len2 = strlen(sl);
  char cc1[len1+2];
  char cc2[len2+2];
// printf("signal = %s, slot = %s\n", sg, sl);
  cc1[len1+1] = cc2[len2+1] = '\0';
  cc1[0] = '2'; cc2[0] = '1';
  int i=1;
  for ( ;i<=len1; ++i) cc1[i] = sg[i-1];
  for (i=1;i<=len2; ++i) cc2[i] = sl[i-1];

// QString s1 = QString("2%1").arg(QString(sg)),
// s2 = QString("1%1").arg(QString(sl));
// printf("signal = %s, slot = %s\n", s1.toLocal8Bit().data(), s2.toLocal8Bit().data() );
// const char *loc1 = s1.toLocal8Bit().data();
// const char *loc2 = s2.toLocal8Bit().data();
  printf ("trying to connect %s -> %s\n", cc1, cc2);
  CAMLreturn(
    Val_bool
      (QObject::connect(QObject_val(sender),
                        cc1,
                        QObject_val(receiver),
                        cc2) ) );
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
      setAbstrClass<QObject>(ans,(QObject*)answer);
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
    o->setProperty(CAMLOBJ_PROPERTY, (qlonglong)camlobj);
    caml_register_global_root(&camlobj);
    CAMLreturn(Val_unit);
  }
  
  CAMLprim // [`qobject ] obj -> string option 
  value getClassName(value cppobj) {
    CAMLparam1(cppobj);
    QObject * qobj = QObject_val(cppobj);
    printf("getClassName of %p\n", qobj);
    if (qobj == NULL)
      CAMLreturn(Val_none);
    else { 
      const char* name = qobj -> metaObject() -> className();
      printf("name = %s\n", name);
      CAMLreturn(Some_of_val(caml_copy_string(name) ) );
    }
  }
/*
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
