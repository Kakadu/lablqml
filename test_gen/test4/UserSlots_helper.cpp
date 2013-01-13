#include "UserSlots.h"
extern "C" {
  #include "headers.h"
  value createUserSlots(value x) {
    CAMLparam1(x);
    CAMLreturn((value)(new UserSlots()));
  }
}

