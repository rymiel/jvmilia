external load_library : string -> int = "load_library_native"
external load_method : int -> string -> int = "load_method_native"

type native_interface = {
  find_class : string -> Basic.eclass;
  dummy : int;
      (* get_static_method : string -> string -> string -> Java.jclass * Java.jmethod; *)
}

external make_native_interface : native_interface -> int
  = "make_native_interface_native"

external free_native_interface : int -> unit = "free_native_interface_native"

external execute_native_auto :
  int ->
  Basic.evalue list ->
  Vtype.vtype list ->
  Vtype.vtype ->
  int ->
  Basic.evalue = "execute_native_auto_native"

external get_registered_fnptr : int -> string -> string -> string -> int option
  = "get_registered_fnptr_native"
