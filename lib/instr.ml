open Shared

type instrbody =
  | Aload_0
  | Aload_1
  | Aload_2
  | Aload_3
  | Aload of int
  | Invokevirtual of method_desc
  | Invokespecial of method_desc
  | Invokestatic of method_desc
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
  | Dup
  | Ldc of loadable_constant
  | Ldc2_w of loadable_constant2
  | Areturn
  | Lconst_0
  | Lconst_1
  | Lstore_0
  | Lstore_1
  | Lstore_2
  | Lstore_3
  | Lload_0
  | Lload_1
  | Lload_2
  | Lload_3
  | Astore of int
  | Ifeq of int
  | Ifne of int
  | Iflt of int
  | Ifge of int
  | Ifgt of int
  | Ifle of int
  | Pop
  | Athrow
  | Lcmp
  | If_icmpeq of int
  | If_icmpne of int
  | If_icmplt of int
  | If_icmpge of int
  | If_icmpgt of int
  | If_icmple of int
  | Ladd

let string_of_instr (i : instrbody) : string =
  let inner = function
    | Aload i -> ("aload", string_of_int i)
    | Aload_0 -> ("aload_0", "")
    | Aload_1 -> ("aload_1", "")
    | Aload_2 -> ("aload_2", "")
    | Aload_3 -> ("aload_3", "")
    | Invokevirtual i ->
        ("invokevirtual", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Invokespecial i ->
        ("invokespecial", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Invokestatic i ->
        ("invokestatic", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
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
    | Dup -> ("dup", "")
    | Ldc i -> ("ldc", string_of_loadable_constant i)
    | Ldc2_w i -> ("ldc2_w", string_of_loadable_constant2 i)
    | Areturn -> ("areturn", "")
    | Lconst_0 -> ("lconst_0", "")
    | Lconst_1 -> ("lconst_1", "")
    | Lstore_0 -> ("lstore_0", "")
    | Lstore_1 -> ("lstore_1", "")
    | Lstore_2 -> ("lstore_2", "")
    | Lstore_3 -> ("lstore_3", "")
    | Lload_0 -> ("lload_0", "")
    | Lload_1 -> ("lload_1", "")
    | Lload_2 -> ("lload_2", "")
    | Lload_3 -> ("lload_3", "")
    | Astore i -> ("astore", string_of_int i)
    | Ifeq i -> ("ifeq", string_of_int i)
    | Ifne i -> ("ifne", string_of_int i)
    | Iflt i -> ("iflt", string_of_int i)
    | Ifge i -> ("ifge", string_of_int i)
    | Ifgt i -> ("ifgt", string_of_int i)
    | Ifle i -> ("ifle", string_of_int i)
    | Pop -> ("pop", "")
    | Athrow -> ("athrow", "")
    | Lcmp -> ("lcmp", "")
    | If_icmpeq i -> ("ifeq", string_of_int i)
    | If_icmpne i -> ("ifne", string_of_int i)
    | If_icmplt i -> ("iflt", string_of_int i)
    | If_icmpge i -> ("ifge", string_of_int i)
    | If_icmpgt i -> ("ifgt", string_of_int i)
    | If_icmple i -> ("ifle", string_of_int i)
    | Ladd -> ("ladd", "")
  in
  let mnemonic, args = inner i in
  Printf.sprintf "%-13s %s" mnemonic args

type instruction = int * instrbody
