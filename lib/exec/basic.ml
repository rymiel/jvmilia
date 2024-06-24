open Java
module StringMap = Map.Make (String)

type evalue = Void | Null | Class of eclassvalue
and eclassvalue = { cls : eclass }
and eclass = { raw : jclass; mutable static : evalue StringMap.t }

type exec_frame = {
  locals : evalue array;
  mutable stack : evalue list;
  mutable pc : int;
  mutable nextpc : int;
  mutable retval : evalue option;
}

let string_of_evalue (value : evalue) : string =
  match value with
  | Void -> "void"
  | Null -> "null"
  | Class v -> Printf.sprintf "class %s" v.cls.raw.name

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
