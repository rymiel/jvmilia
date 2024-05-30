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
