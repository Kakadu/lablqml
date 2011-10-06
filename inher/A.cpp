#include "A.h"
#include <stdio.h>

A::A() {
  printf("inside A constructor\n");
}

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

extern "C" {
  value call_A_foo(value obj) {
    CAMLparam1(obj);
    A* a = (A*) obj;
    a->foo();
    printf("let's return from call_A_foo\n");
    CAMLreturn(Val_unit);
  }

}
