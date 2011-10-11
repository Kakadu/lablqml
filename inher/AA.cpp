#include "A.h"
#include "headers.h"
#include <caml/mlvalues.h>
#include "WrapperClass.h"
#include <stdio.h>

class AA : public A, public OCamlBindingObject {
public:
  void foo() {
    CAMLparam0();
    CAMLlocal1(meth);
    printf("inside AA::foo\n"); fflush(stdout);
    meth = caml_get_public_method( _camlobj, caml_hash_variant("foo"));
    if (meth==0)
      printf ("total fail\n");
    caml_callback(meth, _camlobj);
    printf ("exit from AA::foo\n");
    CAMLreturn0;
  }
  void virtual boo() {
    CAMLparam0();
    CAMLlocal1(meth);
    printf ("inside AA::boo\n");
    meth = caml_get_public_method( _camlobj, caml_hash_variant("boo"));
    caml_callback(meth, _camlobj);
    printf ("exit from AA::boo\n");
    CAMLreturn0;
  }
};

extern "C" {
value create_AA(value unit_x) {
  CAMLparam1(unit_x);
  AA* aa = new AA;
  printf("new AA created: this = %x\n", aa);
  ((OCamlBindingObject*)aa) -> setCamlObj(0);
  CAMLreturn((value)aa);
}
value set_caml_obj(value cpp_obj, value caml_obj) {
  AA* aa_obj;
  CAMLparam2(cpp_obj, caml_obj);
  aa_obj = (AA*)cpp_obj;

  /*
  OCamlBindingObject *o = (OCamlBindingObject*) cpp_obj;
  o -> setCamlObj(caml_obj);
  */

  OCamlBindingObject* o = dynamic_cast<OCamlBindingObject*>(aa_obj);
  o->setCamlObj(caml_obj);

  printf ("Setting caml_obj %x to c++ obj %x\n", caml_obj, cpp_obj);
  CAMLreturn(Val_unit);
}

}



