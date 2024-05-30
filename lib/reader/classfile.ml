open Io
open Shared

type cp_info =
  | Utf8Info of string
  | ClassInfo of { name_idx : int }
  | MethodRefInfo of { class_idx : int; nat_idx : int }
  | NameAndTypeInfo of { name_idx : int; desc_idx : int }

let read_cp_utf8 (ch : in_channel) =
  let bytes = read_list ch read_u1 in
  let s =
    (* suboptimal *)
    String.init (List.length bytes) (fun i -> Char.chr (List.nth bytes i))
  in
  Utf8Info s

let read_cp_class (ch : in_channel) = ClassInfo { name_idx = read_u2 ch }

let read_cp_method_ref (ch : in_channel) =
  let cls = read_u2 ch in
  let nat = read_u2 ch in
  MethodRefInfo { class_idx = cls; nat_idx = nat }

let read_cp_name_and_type (ch : in_channel) =
  NameAndTypeInfo { name_idx = read_u2 ch; desc_idx = read_u2 ch }

let read_const_pool_info (ch : in_channel) : cp_info =
  let tag = read_u1 ch in
  match tag with
  | 1 -> read_cp_utf8 ch
  | 7 -> read_cp_class ch
  | 10 -> read_cp_method_ref ch
  | 12 -> read_cp_name_and_type ch
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

type attribute = Unknown of string * bytes

let read_attribute (const_pool : cp_entry list) (ch : in_channel) : attribute =
  let name = const_pool_utf8 const_pool (Io.read_u2 ch - 1) in
  let length = Int32.to_int (Io.read_u4 ch) in
  let bytes = Bytes.create length in
  let () = Stdlib.really_input ch bytes 0 length in
  Unknown (name, bytes)

type field_info = {
  access_flags : field_access_flags;
  name : string;
  descriptor : string;
  attributes : attribute list;
}

let read_field_info (const_pool : cp_entry list) (ch : in_channel) : field_info
    =
  let access_flags = field_access_flags_of_int (Io.read_u2 ch) in
  let name = const_pool_utf8 const_pool (Io.read_u2 ch - 1) in
  let descriptor = const_pool_utf8 const_pool (Io.read_u2 ch - 1) in
  let attributes = Io.read_list ch (read_attribute const_pool) in
  { access_flags; name; descriptor; attributes }

type method_info = {
  access_flags : method_access_flags;
  name : string;
  descriptor : string;
  attributes : attribute list;
}

let read_method_info (const_pool : cp_entry list) (ch : in_channel) :
    method_info =
  let access_flags = method_access_flags_of_int (Io.read_u2 ch) in
  let name = const_pool_utf8 const_pool (Io.read_u2 ch - 1) in
  let descriptor = const_pool_utf8 const_pool (Io.read_u2 ch - 1) in
  let attributes = Io.read_list ch (read_attribute const_pool) in
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
  let magic = Io.read_u4 ch in
  if magic <> 0xCAFEBABEl then
    failwith
      (Printf.sprintf "Invalid magic: %s, expected 0xCAFEBABE" (Io.hex_u4 magic));
  let minor_version = Io.read_u2 ch in
  let major_version = Io.read_u2 ch in
  let const_pool_raw = Io.read_list0 ch read_const_pool_info in
  let const_pool = resolve_const_pool const_pool_raw in
  let access_flags = class_access_flags_of_int (Io.read_u2 ch) in
  let this_class = const_pool_class const_pool (Io.read_u2 ch - 1) in
  let super_class_index = Io.read_u2 ch in
  let super_class =
    if super_class_index = 0 then None
    else Some (const_pool_class const_pool (super_class_index - 1))
  in
  let interfaces_indices = Io.read_list ch Io.read_u2 in
  let interfaces =
    List.map (fun x -> const_pool_class const_pool (x - 1)) interfaces_indices
  in
  let fields = Io.read_list ch (read_field_info const_pool) in
  let methods = Io.read_list ch (read_method_info const_pool) in
  let attributes = Io.read_list ch (read_attribute const_pool) in
  let () = Io.assert_end_of_file ch in
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
