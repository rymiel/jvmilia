type cp_info =
  | Utf8Info of string
  | ClassInfo of { name_idx : int }
  | MethodRefInfo of { class_idx : int; nat_idx : int }
  | NameAndTypeInfo of { name_idx : int; desc_idx : int }

let read_cp_utf8 (r : Io.reader) =
  let bytes = Io.read_list r Io.read_u1 in
  let s =
    (* suboptimal *)
    String.init (List.length bytes) (fun i -> Char.chr (List.nth bytes i))
  in
  Utf8Info s

let read_cp_class (r : Io.reader) = ClassInfo { name_idx = Io.read_u2 r }

let read_cp_method_ref (r : Io.reader) =
  let cls = Io.read_u2 r in
  let nat = Io.read_u2 r in
  MethodRefInfo { class_idx = cls; nat_idx = nat }

let read_cp_name_and_type (r : Io.reader) =
  NameAndTypeInfo { name_idx = Io.read_u2 r; desc_idx = Io.read_u2 r }

let read_const_pool_info (r : Io.reader) : cp_info =
  let tag = Io.read_u1 r in
  match tag with
  | 1 -> read_cp_utf8 r
  | 7 -> read_cp_class r
  | 10 -> read_cp_method_ref r
  | 12 -> read_cp_name_and_type r
  | i -> failwith (Printf.sprintf "Invalid constant pool tag %i" i)

type cp_entry =
  | Utf8 of string
  | Class of Shared.class_desc
  | MethodRef of Shared.method_desc
  | NameAndType of Shared.name_and_type_desc

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
      let cls =
        match resolve_cp_info pool pool.(x.class_idx - 1) with
        | Class x -> x
        | y ->
            failwith
              (Printf.sprintf "MethodRef.cls: Expected CONSTANT_Class, got %s"
                 (cp_entry_name y))
      in
      let nat =
        match resolve_cp_info pool pool.(x.nat_idx - 1) with
        | NameAndType x -> x
        | y ->
            failwith
              (Printf.sprintf
                 "MethodRef.nat: Expected CONSTANT_NameAndType, got %s"
                 (cp_entry_name y))
      in
      MethodRef { cls = cls.name; name = nat.name; desc = nat.desc }
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

let const_pool_class (cp : cp_entry list) (index : int) : Shared.class_desc =
  match List.nth cp index with
  | Class x -> x
  | _ -> failwith "Expected Class in constant pool"

let const_pool_utf8 (cp : cp_entry list) (index : int) : string =
  match List.nth cp index with
  | Utf8 x -> x
  | _ -> failwith "Expected Utf8 in constant pool"
