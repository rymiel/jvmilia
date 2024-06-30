external load_library : string -> int64 = "load_library_native"
external load_method : int64 -> string -> int64 = "load_method_native"
external make_native_interface : unit -> int64 = "make_native_interface_native"
external free_native_interface : int64 -> unit = "free_native_interface_native"

external execute_native_noargs_void : int64 -> string -> int64 -> unit
  = "execute_native_noargs_void_native"

external get_registered_fnptr : int64 -> string -> string -> string -> int64 option
  = "get_registered_fnptr_native"
