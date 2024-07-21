external load_library : string -> int = "load_library_native"
external load_method : int -> string -> int = "load_method_native"

type native_interface = {
  find_class : string -> Basic.evalue;
  get_static_method : string -> string -> string -> Java.jmethod;
  class_name : Basic.evalue -> string;
  make_string : string -> Basic.evalue;
  invoke_method : Java.jmethod -> Basic.evalue list -> Basic.evalue;
  get_virtual_method : string -> string -> string -> Java.jmethod;
  make_object_array : int -> string -> Basic.evalue -> Basic.evalue;
  set_object_array : Basic.evalue -> int -> Basic.evalue -> unit;
  object_type_name : Basic.evalue -> string;
  object_instance_field : Basic.evalue -> string -> Basic.evalue;
  make_class_direct : string -> Basic.evalue;
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