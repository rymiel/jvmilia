type cp_info =
  | InvalidInfo  (** takes up the "gap" after Long or Double *)
  | Utf8Info of string
  | IntegerInfo of int32
  | FloatInfo of float
  | LongInfo of int64
  | DoubleInfo of float
  | ClassInfo of { name_idx : int }
  | StringInfo of { str_idx : int }
  | FieldRefInfo of { cls_idx : int; nat_idx : int }
  | MethodRefInfo of { cls_idx : int; nat_idx : int }
  | InterfaceMethodRefInfo of { cls_idx : int; nat_idx : int }
  | NameAndTypeInfo of { name_idx : int; desc_idx : int }
  | MethodHandleInfo of { ref_kind : int; ref_idx : int }
  | MethodTypeInfo of { desc_idx : int }
  | InvokeDynamicInfo of { bootstrap_idx : int; nat_idx : int }

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

let read_cp_float (r : Io.reader) =
  let value = Io.read_u4 r in
  let f = Int32.float_of_bits value in
  FloatInfo f

let read_cp_long (r : Io.reader) =
  let value = Io.read_u8 r in
  LongInfo value

let read_cp_double (r : Io.reader) =
  let value = Io.read_u8 r in
  let f = Int64.float_of_bits value in
  DoubleInfo f

let read_cp_class (r : Io.reader) =
  let name_idx = Io.read_u2 r in
  ClassInfo { name_idx }

let read_cp_string (r : Io.reader) =
  let str_idx = Io.read_u2 r in
  StringInfo { str_idx }

let read_cp_field_ref (r : Io.reader) =
  let cls_idx = Io.read_u2 r in
  let nat_idx = Io.read_u2 r in
  FieldRefInfo { cls_idx; nat_idx }

let read_cp_method_ref (r : Io.reader) =
  let cls_idx = Io.read_u2 r in
  let nat_idx = Io.read_u2 r in
  MethodRefInfo { cls_idx; nat_idx }

let read_cp_interface_method_ref (r : Io.reader) =
  let cls_idx = Io.read_u2 r in
  let nat_idx = Io.read_u2 r in
  InterfaceMethodRefInfo { cls_idx; nat_idx }

let read_cp_name_and_type (r : Io.reader) =
  let name_idx = Io.read_u2 r in
  let desc_idx = Io.read_u2 r in
  NameAndTypeInfo { name_idx; desc_idx }

let read_cp_method_handle (r : Io.reader) =
  let ref_kind = Io.read_u1 r in
  let ref_idx = Io.read_u2 r in
  MethodHandleInfo { ref_kind; ref_idx }

let read_cp_method_type (r : Io.reader) =
  let desc_idx = Io.read_u2 r in
  MethodTypeInfo { desc_idx }

let read_cp_invoke_dynamic (r : Io.reader) =
  let bootstrap_idx = Io.read_u2 r in
  let nat_idx = Io.read_u2 r in
  InvokeDynamicInfo { bootstrap_idx; nat_idx }

let read_const_pool_info (r : Io.reader) : cp_info =
  let tag = Io.read_u1 r in
  match tag with
  | 1 -> read_cp_utf8 r
  | 3 -> read_cp_integer r
  | 4 -> read_cp_float r
  | 5 -> read_cp_long r
  | 6 -> read_cp_double r
  | 7 -> read_cp_class r
  | 8 -> read_cp_string r
  | 9 -> read_cp_field_ref r
  | 10 -> read_cp_method_ref r
  | 11 -> read_cp_interface_method_ref r
  | 12 -> read_cp_name_and_type r
  | 15 -> read_cp_method_handle r
  | 16 -> read_cp_method_type r
  | 18 -> read_cp_invoke_dynamic r
  | i -> failwith (Printf.sprintf "Invalid constant pool tag %i" i)

let cp_info_size (x : cp_info) : int =
  match x with LongInfo _ | DoubleInfo _ -> 2 | _ -> 1

type cp_entry =
  | Invalid  (** takes up the "gap" after Long or Double *)
  | Utf8 of string
  | Integer of int32
  | Float of float
  | Long of int64
  | Double of float
  | Class of Shared.class_desc
  | String of string
  | FieldRef of Shared.field_desc
  | MethodRef of Shared.method_desc
  | InterfaceMethodRef of Shared.method_desc
  | NameAndType of Shared.name_and_type_desc
  | MethodHandle of Shared.method_handle_desc
  | MethodType of Shared.method_type_desc
  | InvokeDynamic of Shared.dynamic_desc

let cp_entry_name (info : cp_entry) : string =
  match info with
  | Invalid -> "--"
  | Utf8 _ -> "CONSTANT_Utf8"
  | Integer _ -> "CONSTANT_Integer"
  | Float _ -> "CONSTANT_Float"
  | Long _ -> "CONSTANT_Long"
  | Double _ -> "CONSTANT_Double"
  | Class _ -> "CONSTANT_Class"
  | String _ -> "CONSTANT_String"
  | FieldRef _ -> "CONSTANT_FieldRef"
  | MethodRef _ -> "CONSTANT_MethodRef"
  | InterfaceMethodRef _ -> "CONSTANT_InterfaceMethodRef"
  | NameAndType _ -> "CONSTANT_NameAndType"
  | MethodHandle _ -> "CONSTANT_MethodHandle"
  | MethodType _ -> "CONSTANT_MethodType"
  | InvokeDynamic _ -> "CONSTANT_Invokedynamic"

type const_pool = cp_entry array

let rec resolve_cp_info (pool : cp_info array) (info : cp_info) : cp_entry =
  match info with
  | InvalidInfo -> Invalid
  | Utf8Info x -> Utf8 x
  | IntegerInfo x -> Integer x
  | FloatInfo x -> Float x
  | LongInfo x -> Long x
  | DoubleInfo x -> Double x
  | ClassInfo x -> Class { name = resolve_utf8 pool x.name_idx }
  | StringInfo x -> String (resolve_utf8 pool x.str_idx)
  | FieldRefInfo x ->
      let cls = resolve_class pool x.cls_idx in
      let nat = resolve_nat pool x.nat_idx in
      FieldRef { cls = cls.name; name = nat.name; desc = nat.desc }
  | MethodRefInfo x ->
      let cls = resolve_class pool x.cls_idx in
      let nat = resolve_nat pool x.nat_idx in
      MethodRef { cls = cls.name; name = nat.name; desc = nat.desc }
  | InterfaceMethodRefInfo x ->
      let cls = resolve_class pool x.cls_idx in
      let nat = resolve_nat pool x.nat_idx in
      InterfaceMethodRef { cls = cls.name; name = nat.name; desc = nat.desc }
  | NameAndTypeInfo x ->
      NameAndType
        {
          name = resolve_utf8 pool x.name_idx;
          desc = resolve_utf8 pool x.desc_idx;
        }
  | MethodHandleInfo x ->
      MethodHandle
        {
          kind = x.ref_kind;
          ref =
            (match x.ref_kind with
            | 1 | 2 | 3 | 4 -> resolve_field
            | 5 | 8 -> resolve_method
            | 6 | 7 -> resolve_method_or_interface_method
            | 9 -> resolve_inteface_method
            | _ -> failwith "Invalid method handle ref kind")
              pool x.ref_idx;
        }
  | MethodTypeInfo x -> MethodType { desc = resolve_utf8 pool x.desc_idx }
  | InvokeDynamicInfo x ->
      let nat = resolve_nat pool x.nat_idx in
      InvokeDynamic
        { bootstrap_idx = x.bootstrap_idx; name = nat.name; desc = nat.desc }

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

and resolve_field (pool : cp_info array) idx : Shared.field_desc =
  match resolve_cp_info pool pool.(idx - 1) with
  | FieldRef x -> x
  | y ->
      failwith
        (Printf.sprintf "Expected CONSTANT_Fieldref, got %s" (cp_entry_name y))

and resolve_method (pool : cp_info array) idx : Shared.method_desc =
  match resolve_cp_info pool pool.(idx - 1) with
  | MethodRef x -> x
  | y ->
      failwith
        (Printf.sprintf "Expected CONSTANT_Methodref, got %s" (cp_entry_name y))

and resolve_inteface_method (pool : cp_info array) idx : Shared.method_desc =
  match resolve_cp_info pool pool.(idx - 1) with
  | InterfaceMethodRef x -> x
  | y ->
      failwith
        (Printf.sprintf "Expected CONSTANT_InterfaceMethodref, got %s"
           (cp_entry_name y))

and resolve_method_or_interface_method (pool : cp_info array) idx :
    Shared.method_desc =
  match resolve_cp_info pool pool.(idx - 1) with
  | MethodRef x -> x
  | InterfaceMethodRef x -> x
  | y ->
      failwith
        (Printf.sprintf
           "Expected CONSTANT_Methodref or CONSTANT_InterfaceMethodref, got %s"
           (cp_entry_name y))

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

let const_pool_method_or_interface_method (cp : const_pool) (index : int) :
    Shared.method_desc =
  match Array.get cp (index - 1) with
  | MethodRef x -> x
  | InterfaceMethodRef x -> x
  | _ -> failwith "Expected MethodRef or InterfaceMethodRef in constant pool"

let const_pool_interface_method (cp : const_pool) (index : int) :
    Shared.method_desc =
  match Array.get cp (index - 1) with
  | InterfaceMethodRef x -> x
  | _ -> failwith "Expected InterfaceMethodRef in constant pool"

let const_pool_field (cp : const_pool) (index : int) : Shared.field_desc =
  match Array.get cp (index - 1) with
  | FieldRef x -> x
  | _ -> failwith "Expected FieldRef in constant pool"

let const_pool_utf8 (cp : const_pool) (index : int) : string =
  match Array.get cp (index - 1) with
  | Utf8 x -> x
  | _ -> failwith "Expected Utf8 in constant pool"

let const_pool_loadable_constant (cp : const_pool) (index : int) :
    Shared.loadable_constant =
  let entry = Array.get cp (index - 1) in
  match entry with
  | Integer x -> Integer x
  | Float x -> Float x
  | String s -> String s
  | Class s -> Class s.name
  | MethodType s -> MethodType s.desc
  | MethodHandle m -> MethodHandle m
  | _ ->
      failwith
        (Printf.sprintf "%s is not a loadable constant" (cp_entry_name entry))

let const_pool_loadable_constant2 (cp : const_pool) (index : int) :
    Shared.loadable_constant2 =
  let entry = Array.get cp (index - 1) in
  match entry with
  | Long l -> Long l
  | Double d -> Double d
  | _ ->
      failwith
        (Printf.sprintf "%s is not a loadable constant 2" (cp_entry_name entry))

let const_pool_invokedynamic (cp : const_pool) (index : int) :
    Shared.dynamic_desc =
  match Array.get cp (index - 1) with
  | InvokeDynamic x -> x
  | _ -> failwith "Expected InvokeDynamic in constant pool"

let const_pool_method_handle (cp : const_pool) (index : int) :
    Shared.method_handle_desc =
  match Array.get cp (index - 1) with
  | MethodHandle x -> x
  | _ -> failwith "Expected MethodHandle in constant pool"
