#pragma once
#include <caml/mlvalues.h>

extern "C" {
namespace jvmilia {

extern bool silent;
int log_printf(const char* format, ...) __attribute__((format(printf, 1, 2)));

CAMLprim value load_library_native(value name);
CAMLprim value load_method_native(value library, value name);
CAMLprim value make_native_interface_native(value interface_data);
CAMLprim value free_native_interface_native(value handle);
CAMLprim value get_registered_fnptr_native(value interface, value class_name, value method, value signature);
CAMLprim value execute_native_auto_native(value interface, value params, value param_types, value ret_type,
                                          value fn_ptr);

} // namespace jvmilia
}
