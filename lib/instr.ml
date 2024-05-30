open Shared

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
  | New of class_desc

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
    | New i -> ("new", i.name)
  in
  let mnemonic, args = inner i in
  Printf.sprintf "%-13s %s" mnemonic args

type instruction = int * instrbody
