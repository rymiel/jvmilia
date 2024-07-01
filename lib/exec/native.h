#pragma once
#include <caml/mlvalues.h>

extern "C" {

namespace jvmilia {

// external load_library : string -> int64 = "load_library_native"
CAMLprim value load_library_native(value name);

// external load_method : int64 -> string -> int64 = "load_method_native"
CAMLprim value load_method_native(value library, value name);

// external make_native_interface : unit -> int64 = "make_native_interface_native"
CAMLprim value make_native_interface_native(value unit);

// external free_native_interface : int64 -> unit =  "free_native_interface_native"
CAMLprim value free_native_interface_native(value handle);

// external get_registered_fnptr : int64 -> string -> string -> string -> int64 option = "get_registered_fnptr_native"
CAMLprim value get_registered_fnptr_native(value interface, value class_name, value method, value signature);

// external execute_native_auto : int64 -> Basic.evalue list -> Vtype.vtype list -> Vtype.vtype -> int64 -> Basic.evalue
// = "execute_native_auto_native"
CAMLprim value execute_native_auto_native(value interface, value params, value param_types, value ret_type,
                                          value fn_ptr);

} // namespace jvmilia
}
