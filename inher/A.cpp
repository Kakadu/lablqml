#include "A.h"
#include <stdio.h>

A::A() {
  printf("inside A constructor, this = %x\n", this);
}

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

extern "C" {
  value call_A_foo(value obj) {
    CAMLparam1(obj);
    printf ("inside call_A_foo, obj = %x\n", obj);
    A* a = (A*) obj;
    a->foo();
    printf("let's return from call_A_foo\n");
    CAMLreturn(Val_unit);
  }

}
