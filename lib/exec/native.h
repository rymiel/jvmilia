#pragma once
#include <caml/mlvalues.h>

extern "C" {

// external fib : int -> int = "fib_native"
CAMLprim value fib_native(value n);

// external load_library : string -> int64 = "load_library_native"
CAMLprim value load_library_native(value name);

// external load_method : int64 -> string -> int64 = "load_method_native"
CAMLprim value load_method_native(value library, value name);

// external make_native_interface : unit -> int64 =
// "make_native_interface_native"
CAMLprim value make_native_interface_native(value unit);

// external free_native_interface : int64 -> unit =
// "free_native_interface_native"
CAMLprim value free_native_interface_native(value handle);

// external execute_native_noargs_void : int64 -> string -> int64 -> unit =
// "execute_native_noargs_void_native"
CAMLprim value execute_native_noargs_void_native(value interface, value cls,
                                                 value handle);
}
