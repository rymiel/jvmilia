type cp_info =
  | InvalidInfo  (** takes up the "gap" after Long or Double *)
  | Utf8Info of string
  | IntegerInfo of int32
  | LongInfo of int64
  | ClassInfo of { name_idx : int }
  | StringInfo of { str_idx : int }
  | MethodRefInfo of { cls_idx : int; nat_idx : int }
  | NameAndTypeInfo of { name_idx : int; desc_idx : int }

let read_cp_utf8 (r : Io.reader) =
  let bytes = Io.read_list r Io.read_u1 in
  let s =
    (* suboptimal *)
    String.init (List.length bytes) (fun i -> Char.chr (List.nth bytes i))
  in
  Utf8Info s

let read_cp_integer (r : Io.reader) =
  let value = Io.read_u4 r in
  IntegerInfo value

let read_cp_long (r : Io.reader) =
  let value = Io.read_u8 r in
  LongInfo value

let read_cp_class (r : Io.reader) =
  let name_idx = Io.read_u2 r in
  ClassInfo { name_idx }

let read_cp_string (r : Io.reader) =
  let str_idx = Io.read_u2 r in
  StringInfo { str_idx }

let read_cp_method_ref (r : Io.reader) =
  let cls_idx = Io.read_u2 r in
  let nat_idx = Io.read_u2 r in
  MethodRefInfo { cls_idx; nat_idx }

let read_cp_name_and_type (r : Io.reader) =
  let name_idx = Io.read_u2 r in
  let desc_idx = Io.read_u2 r in
  NameAndTypeInfo { name_idx; desc_idx }

let read_const_pool_info (r : Io.reader) : cp_info =
  let tag = Io.read_u1 r in
  match tag with
  | 1 -> read_cp_utf8 r
  | 3 -> read_cp_integer r
  | 5 -> read_cp_long r
  | 7 -> read_cp_class r
  | 8 -> read_cp_string r
  | 10 -> read_cp_method_ref r
  | 12 -> read_cp_name_and_type r
  | i -> failwith (Printf.sprintf "Invalid constant pool tag %i" i)

let cp_info_size (x : cp_info) : int =
  match x with LongInfo _ -> 2 | _ -> 1 (* TODO: double *)

type cp_entry =
  | Invalid  (** takes up the "gap" after Long or Double *)
  | Utf8 of string
  | Integer of int32
  | Long of int64
  | Class of Shared.class_desc
  | String of string
  | MethodRef of Shared.method_desc
  | NameAndType of Shared.name_and_type_desc

let cp_entry_name (info : cp_entry) : string =
  match info with
  | Invalid -> "--"
  | Utf8 _ -> "CONSTANT_Utf8"
  | Integer _ -> "CONSTANT_Integer"
  | Long _ -> "CONSTANT_Long"
  | Class _ -> "CONSTANT_Class"
  | String _ -> "CONSTANT_String"
  | MethodRef _ -> "CONSTANT_MethodRef"
  | NameAndType _ -> "CONSTANT_NameAndType"

type const_pool = cp_entry array

let rec resolve_cp_info (pool : cp_info array) (info : cp_info) : cp_entry =
  match info with
  | InvalidInfo -> Invalid
  | Utf8Info x -> Utf8 x
  | IntegerInfo x -> Integer x
  | LongInfo x -> Long x
  | ClassInfo x -> Class { name = resolve_utf8 pool x.name_idx }
  | StringInfo x -> String (resolve_utf8 pool x.str_idx)
  | MethodRefInfo x ->
      let cls = resolve_class pool x.cls_idx in
      let nat = resolve_nat pool x.nat_idx in
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

and resolve_nat (pool : cp_info array) idx : Shared.name_and_type_desc =
  match resolve_cp_info pool pool.(idx - 1) with
  | NameAndType x -> x
  | y ->
      failwith
        (Printf.sprintf "Expected CONSTANT_NameAndType, got %s"
           (cp_entry_name y))

and resolve_class (pool : cp_info array) idx : Shared.class_desc =
  match resolve_cp_info pool pool.(idx - 1) with
  | Class x -> x
  | y ->
      failwith
        (Printf.sprintf "Expected CONSTANT_Class, got %s" (cp_entry_name y))

let read_const_pool (r : Io.reader) : const_pool =
  let len = Io.read_u2 r - 1 in
  let arr = Array.make len InvalidInfo in
  let i = ref 0 in
  while !i < len do
    let info = read_const_pool_info r in
    Array.set arr !i info;
    i := !i + cp_info_size info
  done;
  Array.map (resolve_cp_info arr) arr

let const_pool_class (cp : const_pool) (index : int) : Shared.class_desc =
  match Array.get cp (index - 1) with
  | Class x -> x
  | _ -> failwith "Expected Class in constant pool"

let const_pool_method (cp : const_pool) (index : int) : Shared.method_desc =
  match Array.get cp (index - 1) with
  | MethodRef x -> x
  | _ -> failwith "Expected MethodRef in constant pool"

let const_pool_utf8 (cp : const_pool) (index : int) : string =
  match Array.get cp (index - 1) with
  | Utf8 x -> x
  | _ -> failwith "Expected Utf8 in constant pool"

let const_pool_loadable_constant (cp : const_pool) (index : int) :
    Shared.loadable_constant =
  let entry = Array.get cp (index - 1) in
  match entry with
  | Integer x -> Integer x
  | String s -> String s
  | _ ->
      failwith
        (Printf.sprintf "%s is not a loadable constant" (cp_entry_name entry))
