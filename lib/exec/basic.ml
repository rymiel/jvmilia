open Java
module StringMap = Map.Make (String)

type evalue =
  | Void
  | Null
  | Class of eclassvalue
  | Int of int
  | Array of earrayvalue

and eclassvalue = { cls : eclass; mutable fields : evalue StringMap.t }
and earrayvalue = { ty : Vtype.arraytype; arr : evalue array }
and eclass = { raw : jclass; mutable static : evalue StringMap.t }

type exec_frame = {
  locals : evalue array;
  mutable stack : evalue list;
  mutable pc : int;
  mutable nextpc : int;
  mutable retval : evalue option;
}

let string_of_evalue (value : evalue) : string =
  let base =
    match value with
    | Void -> "void"
    | Null -> "null"
    | Class v -> v.cls.raw.name
    | Int v -> Printf.sprintf "int %d" v
    | Array v ->
        Printf.sprintf "array %s[%d]"
          (Vtype.string_of_arraytype v.ty)
          (Array.length v.arr)
  in
  Printf.sprintf "%x:%s" (Obj.magic value) base

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
