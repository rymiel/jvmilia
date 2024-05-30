open Shared

module StringMap = Map.Make (String)

type exception_handler = {
  starti : int;
  endi : int;
  target : int;
  class_name : string option;
}

type method_desc = { cls : string; name : string; desc : string }

type instrbody =
  | Aload_0
  | Aload_1
  | Aload_2
  | Aload_3
  | Aload of int
  | Invokespecial of method_desc
  | Return
  | Iconst_m1
  | Iconst_0
  | Iconst_1
  | Iconst_2
  | Iconst_3
  | Iconst_4
  | Iconst_5
  | Istore_0
  | Istore_1
  | Istore_2
  | Istore_3
  | Istore of int
  | Iload_0
  | Iload_1
  | Iload_2
  | Iload_3
  | Iload of int
  | Iadd
  | Ireturn
  | If_acmpeq of int
  | If_acmpne of int
  | Goto of int

let string_of_instr (i : instrbody) : string =
  let inner = function
    | Aload i -> ("aload", string_of_int i)
    | Aload_0 -> ("aload_0", "")
    | Aload_1 -> ("aload_1", "")
    | Aload_2 -> ("aload_2", "")
    | Aload_3 -> ("aload_3", "")
    | Invokespecial i ->
        ("invokespecial", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Return -> ("return", "")
    | Iconst_m1 -> ("iconst_m1", "")
    | Iconst_0 -> ("iconst_0", "")
    | Iconst_1 -> ("iconst_1", "")
    | Iconst_2 -> ("iconst_2", "")
    | Iconst_3 -> ("iconst_3", "")
    | Iconst_4 -> ("iconst_4", "")
    | Iconst_5 -> ("iconst_5", "")
    | Istore_0 -> ("istore_0", "")
    | Istore_1 -> ("istore_1", "")
    | Istore_2 -> ("istore_2", "")
    | Istore_3 -> ("istore_3", "")
    | Istore i -> ("istore", string_of_int i)
    | Iload_0 -> ("iload_0", "")
    | Iload_1 -> ("iload_1", "")
    | Iload_2 -> ("iload_2", "")
    | Iload_3 -> ("iload_3", "")
    | Iload i -> ("iload", string_of_int i)
    | Iadd -> ("iadd", "")
    | Ireturn -> ("ireturn", "")
    | If_acmpeq i -> ("if_acmpeq", string_of_int i)
    | If_acmpne i -> ("if_acmpne", string_of_int i)
    | Goto i -> ("goto", string_of_int i)
  in
  let mnemonic, args = inner i in
  Printf.sprintf "%-13s %s" mnemonic args

type jinstruction = int * instrbody

type jloader = Bootstrap | UserDefined of string
type flags = { is_this_uninit : bool }

(** verification type *)
type vtype =
  | Top
  | OneWord
  | Int
  | Float
  | Reference
  | Uninitialized
  | UninitializedThis
  | UninitializedOffset of int
  | TwoWord
  | Long
  | Double
  | Class of string * jloader
  | Array of arraytype
  | Null
  | Void (* unspecified in the spec *)

and arraytype = T of vtype | Byte | Char | Short | Boolean
and frame = { locals : vtype list; stack : vtype list; flags : flags }
and jstack_map = int * frame

and code_attribute = {
  frame_size : int;
  max_stack : int;
  handlers : exception_handler list;
  code : jinstruction list;
  stack_map_desc : delta_frame list;
}

(* TODO: i dont think this is circular anymore, investigate *)
and jattribute = Code of code_attribute | Placeholder

and jmethod = {
  name : string;
  access_flags : method_access_flags;
  desc : string;
  attributes : jattribute list;
}

and jclass = {
  name : string;
  access_flags : class_access_flags;
  superclass : string option;
  superinterfaces : string list;
  methods : jmethod list;
  mutable loader : jloader option;
}

and frame_desc = Same | SameLocals1StackItem of vtype
and delta_frame = int * frame_desc

type bootstrap_loader = {
  known : jclass StringMap.t ref;
  load : string -> jclass;
}

let rec string_of_vtype (t : vtype) : string =
  match t with
  | Top -> "~top"
  | OneWord -> "~oneword"
  | Int -> "int"
  | Float -> "float"
  | Reference -> "~reference"
  | Uninitialized -> "~uninitialized"
  | UninitializedThis -> "uninitializedthis"
  | UninitializedOffset i -> Printf.sprintf "uninitialized(%d)" i
  | TwoWord -> "~twoword"
  | Long -> "long"
  | Double -> "double"
  | Class (n, _) -> n
  | Array b -> Printf.sprintf "%s[]" (string_of_arraytype b)
  | Null -> "null"
  | Void -> "void"

and string_of_arraytype (t : arraytype) : string =
  match t with
  | T v -> string_of_vtype v
  | Byte -> "byte"
  | Char -> "char"
  | Short -> "short"
  | Boolean -> "boolean"

let string_of_stack (stack : vtype list) : string =
  let stack_s = List.map string_of_vtype stack in
  Printf.sprintf "[%s]>" (String.concat ", " stack_s)

let string_of_frame (f : frame) : string =
  let locals_s =
    List.mapi (fun i s -> Printf.sprintf "%d=%s" i (string_of_vtype s)) f.locals
  in
  let stack = string_of_stack f.stack in
  Printf.sprintf "{locals=[%s]; stack=%s}" (String.concat ", " locals_s) stack

type merged_code = Instruction of jinstruction | StackMap of jstack_map

type jenvironment = {
  cls : jclass;
  mth : jmethod;
  return : vtype;
  instructions : merged_code list;
  max_stack : int;
  exception_handlers : exception_handler list;
}

type vclass = string * jloader

let bootstrap_loader_ref : bootstrap_loader option ref = ref None

let initialize_bootstrap_loader (loader : string -> jclass) : unit =
  match !bootstrap_loader_ref with
  | Some _ -> failwith "Bootstrap loader has already been initialized"
  | None ->
      bootstrap_loader_ref :=
        Some { known = ref StringMap.empty; load = loader }

let bootstrap_loader_impl () : bootstrap_loader =
  match !bootstrap_loader_ref with
  | Some loader -> loader
  | None -> failwith "Bootstrap loader has not been initialized"

let bootstrap_loader = Bootstrap
