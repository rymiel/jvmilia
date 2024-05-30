open Io
open Shared

type cp_info =
  | Utf8Info of string
  | ClassInfo of { name_idx : int }
  | MethodRefInfo of { class_idx : int; nat_idx : int }
  | NameAndTypeInfo of { name_idx : int; desc_idx : int }

let read_cp_utf8 (r : Io.reader) =
  let bytes = read_list r read_u1 in
  let s =
    (* suboptimal *)
    String.init (List.length bytes) (fun i -> Char.chr (List.nth bytes i))
  in
  Utf8Info s

let read_cp_class (r : Io.reader) = ClassInfo { name_idx = read_u2 r }

let read_cp_method_ref (r : Io.reader) =
  let cls = read_u2 r in
  let nat = read_u2 r in
  MethodRefInfo { class_idx = cls; nat_idx = nat }

let read_cp_name_and_type (r : Io.reader) =
  NameAndTypeInfo { name_idx = read_u2 r; desc_idx = read_u2 r }

let read_const_pool_info (r : Io.reader) : cp_info =
  let tag = read_u1 r in
  match tag with
  | 1 -> read_cp_utf8 r
  | 7 -> read_cp_class r
  | 10 -> read_cp_method_ref r
  | 12 -> read_cp_name_and_type r
  | i -> failwith (Printf.sprintf "Invalid constant pool tag %i" i)

type cp_class = { name : string }
type cp_name_and_type = { name : string; desc : string }

type cp_entry =
  | Utf8 of string
  | Class of cp_class
  | MethodRef of { cls : cp_class; nat : cp_name_and_type }
  | NameAndType of cp_name_and_type

let cp_entry_name (info : cp_entry) : string =
  match info with
  | Utf8 _ -> "CONSTANT_Utf8"
  | Class _ -> "CONSTANT_Class"
  | MethodRef _ -> "CONSTANT_MethodRef"
  | NameAndType _ -> "CONSTANT_NameAndType"

let rec resolve_cp_info (pool : cp_info array) (info : cp_info) : cp_entry =
  match info with
  | Utf8Info x -> Utf8 x
  | ClassInfo x -> Class { name = resolve_utf8 pool x.name_idx }
  | MethodRefInfo x ->
      MethodRef
        {
          cls =
            (match resolve_cp_info pool pool.(x.class_idx - 1) with
            | Class x -> x
            | y ->
                failwith
                  (Printf.sprintf
                     "MethodRef.cls: Expected CONSTANT_Class, got %s"
                     (cp_entry_name y)));
          nat =
            (match resolve_cp_info pool pool.(x.nat_idx - 1) with
            | NameAndType x -> x
            | y ->
                failwith
                  (Printf.sprintf
                     "MethodRef.nat: Expected CONSTANT_NameAndType, got %s"
                     (cp_entry_name y)));
        }
  | NameAndTypeInfo x ->
      NameAndType
        {
          name = resolve_utf8 pool x.name_idx;
          desc = resolve_utf8 pool x.desc_idx;
        }

and resolve_utf8 (pool : cp_info array) idx : string =
  match resolve_cp_info pool pool.(idx - 1) with
  | Utf8 x -> x
  | y ->
      failwith
        (Printf.sprintf "Expected CONSTANT_Utf8, got %s" (cp_entry_name y))

let resolve_const_pool (cp : cp_info list) : cp_entry list =
  let pool = Array.of_list cp in
  List.map (resolve_cp_info pool) cp

let const_pool_class (cp : cp_entry list) (index : int) : cp_class =
  match List.nth cp index with
  | Class x -> x
  | _ -> failwith "Expected Class in constant pool"

let const_pool_utf8 (cp : cp_entry list) (index : int) : string =
  match List.nth cp index with
  | Utf8 x -> x
  | _ -> failwith "Expected Utf8 in constant pool"

type exception_table_entry = {
  start_pc : int;
  end_pc : int;
  handler_pc : int;
  catch_type : cp_class option;
}

let read_exception_table_entry (const_pool : cp_entry list) (r : Io.reader) :
    exception_table_entry =
  let start_pc = Io.read_u2 r in
  let end_pc = Io.read_u2 r in
  let handler_pc = Io.read_u2 r in
  let catch_type_index = Io.read_u2 r in
  let catch_type =
    if catch_type_index = 0 then None
    else Some (const_pool_class const_pool (catch_type_index - 1))
  in
  { start_pc; end_pc; handler_pc; catch_type }

type code_attribute = {
  max_stack : int;
  max_locals : int;
  code : bytes;
  exception_table : exception_table_entry list;
  attributes : attribute list;
}

and attribute = Unknown of string * bytes | Code of code_attribute

let rec read_code_attribute (const_pool : cp_entry list) (r : Io.reader) :
    code_attribute =
  let max_stack = Io.read_u2 r in
  let max_locals = Io.read_u2 r in
  let length = Int32.to_int (Io.read_u4 r) in
  let bytes = Bytes.create length in
  let () = Io.really_read r bytes length in
  let exception_table =
    Io.read_list r (read_exception_table_entry const_pool)
  in
  let attributes = Io.read_list r (read_attribute const_pool) in
  { max_stack; max_locals; code = bytes; exception_table; attributes }

and read_attribute (const_pool : cp_entry list) (r : Io.reader) : attribute =
  let name = const_pool_utf8 const_pool (Io.read_u2 r - 1) in
  let length = Int32.to_int (Io.read_u4 r) in
  let bytes = Bytes.create length in
  let () = Io.really_read r bytes length in
  Unknown (name, bytes)

type field_info = {
  access_flags : field_access_flags;
  name : string;
  descriptor : string;
  attributes : attribute list;
}

let read_field_info (const_pool : cp_entry list) (r : Io.reader) : field_info =
  let access_flags = field_access_flags_of_int (Io.read_u2 r) in
  let name = const_pool_utf8 const_pool (Io.read_u2 r - 1) in
  let descriptor = const_pool_utf8 const_pool (Io.read_u2 r - 1) in
  let attributes = Io.read_list r (read_attribute const_pool) in
  { access_flags; name; descriptor; attributes }

type method_info = {
  access_flags : method_access_flags;
  name : string;
  descriptor : string;
  attributes : attribute list;
}

let read_method_info (const_pool : cp_entry list) (r : Io.reader) : method_info
    =
  let access_flags = method_access_flags_of_int (Io.read_u2 r) in
  let name = const_pool_utf8 const_pool (Io.read_u2 r - 1) in
  let descriptor = const_pool_utf8 const_pool (Io.read_u2 r - 1) in
  let attributes = Io.read_list r (read_attribute const_pool) in
  { access_flags; name; descriptor; attributes }

type class_file = {
  major_version : int;
  minor_version : int;
  const_pool : cp_entry list;
  access_flags : class_access_flags;
  this_class : cp_class;
  super_class : cp_class option;
  interfaces : cp_class list;
  fields : field_info list;
  methods : method_info list;
  attributes : attribute list;
}

let read_class_file (ch : in_channel) : class_file =
  let r = Io.ch_reader ch in
  let magic = Io.read_u4 r in
  if magic <> 0xCAFEBABEl then
    failwith
      (Printf.sprintf "Invalid magic: %s, expected 0xCAFEBABE" (Io.hex_u4 magic));
  let minor_version = Io.read_u2 r in
  let major_version = Io.read_u2 r in
  let const_pool_raw = Io.read_list0 r read_const_pool_info in
  let const_pool = resolve_const_pool const_pool_raw in
  let access_flags = class_access_flags_of_int (Io.read_u2 r) in
  let this_class = const_pool_class const_pool (Io.read_u2 r - 1) in
  let super_class_index = Io.read_u2 r in
  let super_class =
    if super_class_index = 0 then None
    else Some (const_pool_class const_pool (super_class_index - 1))
  in
  let interfaces_indices = Io.read_list r Io.read_u2 in
  let interfaces =
    List.map (fun x -> const_pool_class const_pool (x - 1)) interfaces_indices
  in
  let fields = Io.read_list r (read_field_info const_pool) in
  let methods = Io.read_list r (read_method_info const_pool) in
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
