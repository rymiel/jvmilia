open Java
module StringMap = Map.Make (String)

(* WARNING: most types here should not be reordered because native code relies on the order of things *)
type evalue =
  | Void
  | Null
  | Object of eobjectvalue
  | Int of int32
  | Array of earrayvalue
  | Long of int64
  | ByteArray of bytes
  | Float of float

and eobjectvalue = { cls : eclass; mutable fields : evalue StringMap.t }
and earrayvalue = { ty : Type.arraytype; arr : evalue array }
and eclass = { raw : jclass; mutable static : evalue StringMap.t }

type exec_frame = {
  locals : evalue array;
  mutable stack : evalue list;
  mutable pc : int;
  mutable nextpc : int;
  mutable retval : evalue option;
}

let string_type_value (v : eobjectvalue) : string =
  match StringMap.find "value" v.fields with
  | ByteArray x -> Bytes.to_string x
  | _ -> assert false

let string_of_evalue (value : evalue) : string =
  match value with
  | Void -> "void"
  | Null -> "null"
  | Object v -> Printf.sprintf "%x:%s" (Obj.magic value) v.cls.raw.name
  | Int v -> Printf.sprintf "int %ld" v
  | Long v -> Printf.sprintf "long %Ld" v
  | Array v ->
      Printf.sprintf "%x:array %s[%d]" (Obj.magic value)
        (Type.string_of_arraytype v.ty)
        (Array.length v.arr)
  | ByteArray v ->
      Printf.sprintf "%x:array byte[%d]" (Obj.magic value) (Bytes.length v)
  | Float v -> Printf.sprintf "float %f" v

let rec string_of_evalue_detailed (value : evalue) : string =
  match value with
  | Void -> "void"
  | Null -> "null"
  | Object v -> (
      match v.cls.raw.name with
      | "java/lang/String" ->
          Printf.sprintf "%x:%S" (Obj.magic value) (string_type_value v)
      | name ->
          Printf.sprintf "%x:%s {%s}" (Obj.magic value) name
            (String.concat ", "
               (List.map
                  (fun (k, v) ->
                    Printf.sprintf "%s=%s" k (string_of_evalue_detailed v))
                  (StringMap.to_list v.fields))))
  | Int v -> Printf.sprintf "int %ld" v
  | Long v -> Printf.sprintf "long %Ld" v
  | Array v ->
      Printf.sprintf "%x:array %s[%d] {%s}" (Obj.magic value)
        (Type.string_of_arraytype v.ty)
        (Array.length v.arr)
        (String.concat ", "
           (Array.to_list (Array.map string_of_evalue_detailed v.arr)))
  | ByteArray v ->
      Printf.sprintf "%x:array byte[%d] {%s}" (Obj.magic value) (Bytes.length v)
        (Bytes.escaped v |> Bytes.to_string)
  | Float v -> Printf.sprintf "float %f" v

let string_of_frame (f : exec_frame) : string =
  let locals_s =
    List.mapi
      (fun i s -> Printf.sprintf "%d=%s" i (string_of_evalue s))
      (Array.to_list f.locals)
  in
  let stack_s = List.map string_of_evalue f.stack in
  Printf.sprintf "%d:{locals=[%s]; stack=<[%s]}" f.pc
    (String.concat ", " locals_s)
    (String.concat ", " stack_s)
