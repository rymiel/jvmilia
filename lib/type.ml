open Shared

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

(** descriptor type *)
type dtype =
  | Byte
  | Char
  | Double
  | Float
  | Int
  | Long
  (* not called Class because it strangely collides with vtype *)
  | Object of string
  | Short
  | Boolean
  | Array of dtype
  | Void

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

let rec string_of_dtype (d : dtype) : string =
  match d with
  | Byte -> "byte"
  | Char -> "char"
  | Double -> "double"
  | Float -> "float"
  | Int -> "int"
  | Long -> "long"
  | Object n -> n
  | Short -> "short"
  | Boolean -> "boolean"
  | Array b -> Printf.sprintf "%s[]" (string_of_dtype b)
  | Void -> "void"

let rec arraytype_of_dtype (d : dtype) : arraytype =
  match d with
  | Byte -> Byte
  | Char -> Char
  | Double -> T Double
  | Float -> T Float
  | Int -> T Int
  | Long -> T Long
  | Object n -> T (Class (n, Bootstrap)) (* is this a good idea *)
  | Short -> Short
  | Boolean -> Boolean
  | Array x -> T (Array (arraytype_of_dtype x))
  | Void -> T Void

let vtype_of_dtype (d : dtype) : vtype =
  match arraytype_of_dtype d with
  | T x -> x
  | Byte | Char | Short | Boolean -> Int

let rec parse_dtype (s : string) (offset : int ref) : dtype =
  let c = String.get s !offset in
  incr offset;
  match c with
  | 'D' -> Double
  | 'F' -> Float
  | 'J' -> Long
  | 'I' -> Int
  | 'B' -> Byte
  | 'C' -> Char
  | 'S' -> Short
  | 'Z' -> Boolean
  | 'V' -> Void
  | '[' -> Array (parse_dtype s offset)
  | 'L' ->
      let end_index = String.index_from s !offset ';' in
      let count = end_index - !offset in
      let classname = String.sub s !offset count in
      offset := !offset + count + 1;
      Object classname
  | c -> failwith (Printf.sprintf "Invalid descriptor %c" c)

let rec parse_vtype (s : string) (offset : int ref) : vtype =
  match parse_arraytype s offset with
  | T x -> x
  | Byte | Char | Short | Boolean -> Int

and parse_arraytype (s : string) (offset : int ref) : arraytype =
  let c = String.get s !offset in
  incr offset;
  match c with
  | 'D' -> T Double
  | 'F' -> T Float
  | 'J' -> T Long
  | 'I' -> T Int
  | 'B' -> Byte
  | 'C' -> Char
  | 'S' -> Short
  | 'Z' -> Boolean
  | 'V' -> T Void
  | '[' -> T (Array (parse_arraytype s offset))
  | 'L' ->
      let count = ref 0 in
      while String.get s (!offset + !count) <> ';' do
        incr count
      done;
      let classname = String.sub s !offset !count in
      offset := !offset + !count + 1;

      T (Class (classname, Bootstrap))
  | c -> failwith (Printf.sprintf "Invalid descriptor %c" c)

let parse_class_internal_name (s : string) : vtype =
  if String.starts_with ~prefix:"[" s then (
    let offset = ref 0 in
    let t = parse_vtype s offset in
    assert (!offset = String.length s);
    match t with Array _ -> t | _ -> failwith "Not an array type?"
    (* kissing owner this a good idea *))
  else Class (s, Bootstrap)

let parse_method_descriptor (desc : string) : vtype list * vtype =
  let s = desc in
  assert (String.get s 0 = '(');
  let offset = ref 1 in
  let args = ref [] in
  while String.get s !offset <> ')' do
    let t = parse_vtype s offset in
    args := !args @ [ t ]
  done;
  incr offset;
  let ret = parse_vtype s offset in
  assert (!offset = String.length desc);
  (!args, ret)

let parse_method_descriptord (desc : string) : dtype list * dtype =
  let s = desc in
  assert (String.get s 0 = '(');
  let offset = ref 1 in
  let args = ref [] in
  while String.get s !offset <> ')' do
    let t = parse_dtype s offset in
    args := !args @ [ t ]
  done;
  incr offset;
  let ret = parse_dtype s offset in
  assert (!offset = String.length desc);
  (!args, ret)

let parse_field_descriptor (desc : string) : vtype =
  let offset = ref 0 in
  let t = parse_vtype desc offset in
  assert (!offset = String.length desc);
  t

let parse_field_descriptord (desc : string) : dtype =
  let offset = ref 0 in
  let t = parse_dtype desc offset in
  assert (!offset = String.length desc);
  t

let map_vtype_method ((args, ret) : dtype list * dtype) : vtype list * vtype =
  (List.map vtype_of_dtype args, vtype_of_dtype ret)

let (parse_vtype [@deprecated]) = parse_vtype
let (parse_arraytype [@deprecated]) = parse_arraytype
let (parse_method_descriptor [@deprecated]) = parse_method_descriptor
let (parse_field_descriptor [@deprecated]) = parse_field_descriptor

let size (t : vtype) : int =
  match t with
  | Top | OneWord | Int | Float | Reference | Uninitialized | UninitializedThis
  | Null | UninitializedOffset _ | Array _ | Class _ ->
      1
  | TwoWord | Long | Double -> 2
  | Void -> failwith "Void has no size"
