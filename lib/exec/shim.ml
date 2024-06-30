external load_library : string -> int = "load_library_native"
external load_method : int -> string -> int = "load_method_native"
external make_native_interface : unit -> int = "make_native_interface_native"
external free_native_interface : int -> unit = "free_native_interface_native"

external execute_native_noargs_void : int -> string -> int -> unit
  = "execute_native_noargs_void_native"

external execute_native_auto :
  int ->
  Basic.evalue list ->
  Vtype.vtype list ->
  Vtype.vtype ->
  int ->
  Basic.evalue = "execute_native_auto_native"

external get_registered_fnptr : int -> string -> string -> string -> int option
  = "get_registered_fnptr_native"
