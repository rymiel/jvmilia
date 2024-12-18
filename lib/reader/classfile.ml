open Shared
open Attr
open Constpool
open Java

let read_exception_table_entry (pool : const_pool) (r : Io.reader) :
    exception_handler =
  let start_pc = Io.read_u2 r in
  let end_pc = Io.read_u2 r in
  let handler_pc = Io.read_u2 r in
  let catch_type_index = Io.read_u2 r in
  let catch_type =
    if catch_type_index = 0 then None
    else Some (const_pool_class pool catch_type_index).name
  in
  {
    starti = start_pc;
    endi = end_pc;
    target = handler_pc;
    class_name = catch_type;
  }

let read_stack_map_vtype (pool : const_pool) (r : Io.reader) : Type.vtype =
  let tag = Io.read_u1 r in
  match tag with
  | 0 -> Top
  | 1 -> Int
  | 2 -> Float
  | 3 -> Double
  | 4 -> Long
  | 5 -> Null
  | 6 -> UninitializedThis
  | 7 ->
      let cls = Io.read_u2 r |> const_pool_class pool in
      let name =
        Type.parse_class_internal_name cls.name |> Type.vtype_of_dtype
      in
      name
  | 8 ->
      let offset = Io.read_u2 r in
      UninitializedOffset offset
  | _ -> failwith (Printf.sprintf "Unimplemented stack_map vtype tag %d" tag)

let read_stack_map_frame (pool : const_pool) (r : Io.reader) : delta_frame =
  let tag = Io.read_u1 r in
  match tag with
  | i when i >= 0 && i <= 63 -> (i, Same)
  | i when i >= 64 && i <= 127 ->
      let t = read_stack_map_vtype pool r in
      (i - 64, SameLocals1StackItem t)
  | i when i = 247 ->
      let delta = Io.read_u2 r in
      let t = read_stack_map_vtype pool r in
      (delta, SameLocals1StackItem t)
  | i when i >= 248 && i <= 250 ->
      let count = 251 - i in
      let delta = Io.read_u2 r in
      (delta, Chop count)
  | i when i = 251 ->
      let delta = Io.read_u2 r in
      (delta, Same)
  | i when i >= 252 && i <= 254 ->
      let count = i - 251 in
      let delta = Io.read_u2 r in
      let locals = Io.read_list_sized r count (read_stack_map_vtype pool) in
      (delta, Append locals)
  | i when i = 255 ->
      let delta = Io.read_u2 r in
      let locals = Io.read_list r (read_stack_map_vtype pool) in
      let stack = Io.read_list r (read_stack_map_vtype pool) in
      (delta, FullFrame { locals; stack })
  | _ -> failwith (Printf.sprintf "Unimplemented stack_map_frame tag %d" tag)

let read_stack_map_table_attribute (pool : const_pool) (r : Io.reader) :
    delta_frame list =
  let out = Io.read_list r (read_stack_map_frame pool) in
  let () = Io.assert_end_of_file r in
  out

let read_bootstrap_method (pool : const_pool) (r : Io.reader) : bootstrap_method
    =
  let ref = Io.read_u2 r |> const_pool_method_handle pool in
  let args =
    Io.read_list r (fun r -> Io.read_u2 r |> const_pool_loadable_constant pool)
  in
  { ref; args }

let read_bootstrap_methods (pool : const_pool) (r : Io.reader) :
    bootstrap_method list =
  let out = Io.read_list r (read_bootstrap_method pool) in
  let () = Io.assert_end_of_file r in
  out

let rec read_code_attribute (pool : const_pool) (r : Io.reader) : code_attribute
    =
  let max_stack = Io.read_u2 r in
  let frame_size = Io.read_u2 r in
  let length = Int32.to_int (Io.read_u4 r) in
  let code_bytes = Bytes.create length in
  let () = Io.really_read r code_bytes length in
  let code_reader = Io.bytes_reader code_bytes in
  let code = Code.read_code pool code_reader in
  let handlers = Io.read_list r (read_exception_table_entry pool) in
  let attributes = Io.read_list r (read_attribute pool) in
  let () = Io.assert_end_of_file r in
  { max_stack; frame_size; code; handlers; attributes }

and read_attribute (pool : const_pool) (r : Io.reader) : attribute =
  let name = const_pool_utf8 pool (Io.read_u2 r) in
  let length = Int32.to_int (Io.read_u4 r) in
  let bytes = Bytes.create length in
  let () = Io.really_read r bytes length in
  let bytes_reader = Io.bytes_reader bytes in
  match name with
  | "Code" ->
      let code = read_code_attribute pool bytes_reader in
      Code code
  | "StackMapTable" ->
      let table = read_stack_map_table_attribute pool bytes_reader in
      StackMapTable table
  | "BootstrapMethods" ->
      let methods = read_bootstrap_methods pool bytes_reader in
      BootstrapMethods methods
  | _ -> Unknown (name, bytes)

let read_field_info (pool : const_pool) (cls : string) (r : Io.reader) : jfield
    =
  let access_flags = field_access_flags_of_int (Io.read_u2 r) in
  let name = const_pool_utf8 pool (Io.read_u2 r) in
  let desc = const_pool_utf8 pool (Io.read_u2 r) in
  let attributes = Io.read_list r (read_attribute pool) in
  let field_type = Type.parse_field_descriptor desc in
  { access_flags; name; desc; attributes; field_type; cls }

let read_method_info (pool : const_pool) (cls : string) (r : Io.reader) :
    jmethod =
  let access_flags = method_access_flags_of_int (Io.read_u2 r) in
  let name = const_pool_utf8 pool (Io.read_u2 r) in
  let desc = const_pool_utf8 pool (Io.read_u2 r) in
  let attributes = Io.read_list r (read_attribute pool) in
  let arg_types, ret_type = Type.parse_method_descriptor desc in
  let nargs = List.length arg_types in
  { access_flags; name; desc; attributes; nargs; arg_types; ret_type; cls }

type class_file = {
  major_version : int;
  minor_version : int;
  const_pool : const_pool;
  access_flags : class_access_flags;
  this_class : string;
  super_class : string option;
  interfaces : string list;
  fields : jfield list;
  methods : jmethod list;
  attributes : attribute list;
}

let convert_class_file (file : class_file) (loader : jloader) : jclass =
  {
    name = file.this_class;
    access_flags = file.access_flags;
    superclass = file.super_class;
    superinterfaces = file.interfaces;
    methods = file.methods;
    fields = file.fields;
    attributes = file.attributes;
    loader;
  }

let read_class_file (ch : in_channel) : class_file =
  let r = Io.ch_reader ch in
  let magic = Io.read_u4 r in
  if magic <> 0xCAFEBABEl then
    failwith
      (Printf.sprintf "Invalid magic: %s, expected 0xCAFEBABE" (Io.hex_u4 magic));
  let minor_version = Io.read_u2 r in
  let major_version = Io.read_u2 r in
  let const_pool = read_const_pool r in
  let access_flags = Io.read_u2 r |> class_access_flags_of_int in
  let this_class = (Io.read_u2 r |> const_pool_class const_pool).name in
  let super_class_index = Io.read_u2 r in
  let super_class =
    if super_class_index = 0 then None
    else Some (const_pool_class const_pool super_class_index).name
  in
  let interfaces_indices = Io.read_list r Io.read_u2 in
  let interfaces =
    List.map (fun x -> (const_pool_class const_pool x).name) interfaces_indices
  in
  let fields = Io.read_list r (read_field_info const_pool this_class) in
  let methods = Io.read_list r (read_method_info const_pool this_class) in
  let attributes = Io.read_list r (read_attribute const_pool) in
  let () = Io.assert_end_of_file r in
  {
    major_version;
    minor_version;
    const_pool;
    access_flags;
    this_class;
    super_class;
    interfaces;
    fields;
    methods;
    attributes;
  }
