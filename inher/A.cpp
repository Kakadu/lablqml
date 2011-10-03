#include "A.h"

A::A() {

}

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

extern "C" {
  value call_A_foo(value obj) {
    CAMLparam1(obj);
    A* a = (A*) obj;
    int ans = a->foo();
    CAMLreturn(Val_int(ans) );
  }

}
