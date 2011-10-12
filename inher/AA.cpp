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
    printf("calling callback of meth = %x\n",meth);
    caml_callback(meth, _camlobj);
    printf ("exit from AA::foo\n");
    CAMLreturn0;
  }
  void virtual boo() {
    CAMLparam0();
    CAMLlocal1(meth);
    printf ("inside AA::boo, camlobj = %x\n", _camlobj);
    meth = caml_get_public_method( _camlobj, caml_hash_variant("boo"));
    if (meth==0)
      printf ("total fail\n");
    printf("calling callback of meth = %x\n",meth);
    caml_callback(meth, _camlobj);
    printf ("exit from AA::boo\n");
    CAMLreturn0;
  }
};

extern "C" {
value create_AA(value camlobj) {
  CAMLparam1(camlobj);
  AA* aa = new AA;
  aa -> setCamlObj(camlobj);
  CAMLreturn((value)aa);
}

}



