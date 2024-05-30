type class_access_flags = {
  is_public : bool;
  is_final : bool;
  is_super : bool;
  is_interface : bool;
  is_abstract : bool;
  is_synthetic : bool;
  is_annotation : bool;
  is_enum : bool;
  is_module : bool;
}

let class_access_flags_of_int (num : int) : class_access_flags =
  {
    is_public = Int.logand num 0x0001 <> 0;
    is_final = Int.logand num 0x0010 <> 0;
    is_super = Int.logand num 0x0020 <> 0;
    is_interface = Int.logand num 0x0200 <> 0;
    is_abstract = Int.logand num 0x0400 <> 0;
    is_synthetic = Int.logand num 0x1000 <> 0;
    is_annotation = Int.logand num 0x2000 <> 0;
    is_enum = Int.logand num 0x4000 <> 0;
    is_module = Int.logand num 0x8000 <> 0;
  }

type method_access_flags = {
  is_public : bool;
  is_private : bool;
  is_protected : bool;
  is_static : bool;
  is_final : bool;
  is_synchronized : bool;
  is_bridge : bool;
  is_varargs : bool;
  is_native : bool;
  is_abstract : bool;
  is_strict : bool;
  is_synthetic : bool;
}

let method_access_flags_of_int (num : int) : method_access_flags =
  {
    is_public = Int.logand num 0x0001 <> 0;
    is_private = Int.logand num 0x0002 <> 0;
    is_protected = Int.logand num 0x0004 <> 0;
    is_static = Int.logand num 0x0008 <> 0;
    is_final = Int.logand num 0x0010 <> 0;
    is_synchronized = Int.logand num 0x0020 <> 0;
    is_bridge = Int.logand num 0x0040 <> 0;
    is_varargs = Int.logand num 0x0080 <> 0;
    is_native = Int.logand num 0x0100 <> 0;
    is_abstract = Int.logand num 0x0400 <> 0;
    is_strict = Int.logand num 0x0800 <> 0;
    is_synthetic = Int.logand num 0x1000 <> 0;
  }

type field_access_flags = {
  is_public : bool;
  is_private : bool;
  is_protected : bool;
  is_static : bool;
  is_final : bool;
  is_volatile : bool;
  is_transient : bool;
  is_synthetic : bool;
  is_enum : bool;
}

let field_access_flags_of_int (num : int) : field_access_flags =
  {
    is_public = Int.logand num 0x0001 <> 0;
    is_private = Int.logand num 0x0002 <> 0;
    is_protected = Int.logand num 0x0004 <> 0;
    is_static = Int.logand num 0x0008 <> 0;
    is_final = Int.logand num 0x0010 <> 0;
    is_volatile = Int.logand num 0x0040 <> 0;
    is_transient = Int.logand num 0x0080 <> 0;
    is_synthetic = Int.logand num 0x1000 <> 0;
    is_enum = Int.logand num 0x4000 <> 0;
  }

type class_desc = { name : string }
type name_and_type_desc = { name : string; desc : string }
type method_desc = { cls : string; name : string; desc : string }

type exception_handler = {
  starti : int;
  endi : int;
  target : int;
  class_name : string option;
}

type jloader = Bootstrap | UserDefined of string
type loadable_constant = Integer of int32 | String of string
type loadable_constant2 = Long of int64

let string_of_loadable_constant (c : loadable_constant) : string =
  match c with
  | Integer i -> Printf.sprintf "int %ld" i
  | String s -> Printf.sprintf "string %S" s

let string_of_loadable_constant2 (c : loadable_constant2) : string =
  match c with Long l -> Printf.sprintf "int %Ld" l
