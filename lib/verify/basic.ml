open Shared
module StringMap = Map.Make (String)

type jloader = Bootstrap | UserDefined of string

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

type flags = { is_this_uninit : bool }
type frame = { locals : vtype list; stack : vtype list; flags : flags }
type jstack_map = int * frame
type frame_desc = Same | SameLocals1StackItem of vtype
type delta_frame = int * frame_desc

type code_attribute = {
  frame_size : int;
  max_stack : int;
  handlers : exception_handler list;
  code : Instr.instruction list;
  stack_map_desc : delta_frame list;
}

type jattribute = Code of code_attribute | Placeholder

type jmethod = {
  name : string;
  access_flags : method_access_flags;
  desc : string;
  attributes : jattribute list;
}

type jclass = {
  name : string;
  access_flags : class_access_flags;
  superclass : string option;
  superinterfaces : string list;
  methods : jmethod list;
  mutable loader : jloader option;
}

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

type merged_code = Instruction of Instr.instruction | StackMap of jstack_map

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
