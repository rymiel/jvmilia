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
  | Ldc_w of loadable_constant
  | Ldc2_w of loadable_constant2
  | Areturn
  | Lconst_0
  | Lconst_1
  | Lstore of int
  | Lstore_0
  | Lstore_1
  | Lstore_2
  | Lstore_3
  | Lload of int
  | Lload_0
  | Lload_1
  | Lload_2
  | Lload_3
  | Astore of int
  | Astore_0
  | Astore_1
  | Astore_2
  | Astore_3
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
  | Getfield of field_desc
  | Putfield of field_desc
  | Getstatic of field_desc
  | Putstatic of field_desc
  | Aconst_null
  | Ifnull of int
  | Ifnonnull of int
  | Checkcast of class_desc
  | Instanceof of class_desc
  | Monitorenter
  | Monitorexit
  | Invokeinterface of method_desc * int
  | Arraylength
  | Aaload
  | Iinc of int * int
  | Isub
  | Bipush of int
  | Sipush of int
  | Anewarray of class_desc
  | I2b
  | I2c
  | I2d
  | Ishl
  | Baload
  | Iand
  | Ior
  | Bastore
  | Newarray of Vtype.arraytype
  | Fload_0
  | Fload_1
  | Fload_2
  | Fload_3
  | Fload of int
  | Dload_0
  | Dload_1
  | Dload_2
  | Dload_3
  | Dload of int
  | F2d
  | Dmul
  | D2i
  | Ishr
  | Ixor
  | Imul
  | I2l
  | Lsub
  | Aastore
  | Lmul
  | Lshl
  | Dup2
  | L2i
  | Invokedynamic of Shared.dynamic_desc
  | Caload
  | Lookupswitch of int * (int * int) list
  | Tableswitch of int * (int * int) * int list
  | Castore
  | Idiv
  | Land
  | Ineg
  | Dadd
  | Dup_x1
  | Lor
  | Lreturn
  | Nop
  | Fconst_0
  | Fconst_1
  | Fconst_2
  | Dconst_0
  | Dconst_1
  | Iaload
  | Laload
  | Faload
  | Daload
  | Saload
  | Fstore of int
  | Dstore of int
  | Fstore_0
  | Fstore_1
  | Fstore_2
  | Fstore_3
  | Dstore_0
  | Dstore_1
  | Dstore_2
  | Dstore_3
  | Iastore
  | Lastore
  | Fastore
  | Dastore
  | Sastore
  | Pop2
  | Dup_x2
  | Dup2_x1
  | Dup2_x2
  | Swap
  | Fadd
  | Fsub
  | Dsub
  | Fmul
  | Ldiv
  | Fdiv
  | Ddiv
  | Irem
  | Lrem
  | Frem
  | Drem
  | Lneg
  | Fneg
  | Dneg
  | Lshr
  | Iushr
  | Lushr
  | Lxor
  | I2f
  | L2f
  | L2d
  | F2i
  | F2l
  | D2l
  | D2f
  | I2s
  | Fcmpl
  | Fcmpg
  | Dcmpl
  | Dcmpg

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
    | Invokeinterface (i, _) ->
        ("invokeinterface", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Return -> ("return", "")
    | Iconst_m1 -> ("iconst_m1", "")
    | Iconst_0 -> ("iconst_0", "")
    | Iconst_1 -> ("iconst_1", "")
    | Iconst_2 -> ("iconst_2", "")
    | Iconst_3 -> ("iconst_3", "")
    | Iconst_4 -> ("iconst_4", "")
    | Iconst_5 -> ("iconst_5", "")
    | Istore i -> ("istore", string_of_int i)
    | Istore_0 -> ("istore_0", "")
    | Istore_1 -> ("istore_1", "")
    | Istore_2 -> ("istore_2", "")
    | Istore_3 -> ("istore_3", "")
    | Iload i -> ("iload", string_of_int i)
    | Iload_0 -> ("iload_0", "")
    | Iload_1 -> ("iload_1", "")
    | Iload_2 -> ("iload_2", "")
    | Iload_3 -> ("iload_3", "")
    | Iadd -> ("iadd", "")
    | Ireturn -> ("ireturn", "")
    | If_acmpeq i -> ("if_acmpeq", string_of_int i)
    | If_acmpne i -> ("if_acmpne", string_of_int i)
    | Goto i -> ("goto", string_of_int i)
    | New i -> ("new", i.name)
    | Dup -> ("dup", "")
    | Ldc i -> ("ldc", string_of_loadable_constant i)
    | Ldc_w i -> ("ldc_w", string_of_loadable_constant i)
    | Ldc2_w i -> ("ldc2_w", string_of_loadable_constant2 i)
    | Areturn -> ("areturn", "")
    | Lconst_0 -> ("lconst_0", "")
    | Lconst_1 -> ("lconst_1", "")
    | Lstore i -> ("lstore", string_of_int i)
    | Lstore_0 -> ("lstore_0", "")
    | Lstore_1 -> ("lstore_1", "")
    | Lstore_2 -> ("lstore_2", "")
    | Lstore_3 -> ("lstore_3", "")
    | Lload i -> ("lload", string_of_int i)
    | Lload_0 -> ("lload_0", "")
    | Lload_1 -> ("lload_1", "")
    | Lload_2 -> ("lload_2", "")
    | Lload_3 -> ("lload_3", "")
    | Astore i -> ("astore", string_of_int i)
    | Astore_0 -> ("astore_0", "")
    | Astore_1 -> ("astore_1", "")
    | Astore_2 -> ("astore_2", "")
    | Astore_3 -> ("astore_3", "")
    | Ifeq i -> ("ifeq", string_of_int i)
    | Ifne i -> ("ifne", string_of_int i)
    | Iflt i -> ("iflt", string_of_int i)
    | Ifge i -> ("ifge", string_of_int i)
    | Ifgt i -> ("ifgt", string_of_int i)
    | Ifle i -> ("ifle", string_of_int i)
    | Pop -> ("pop", "")
    | Athrow -> ("athrow", "")
    | Lcmp -> ("lcmp", "")
    | If_icmpeq i -> ("if_icmpeq", string_of_int i)
    | If_icmpne i -> ("if_icmpne", string_of_int i)
    | If_icmplt i -> ("if_icmplt", string_of_int i)
    | If_icmpge i -> ("if_icmpge", string_of_int i)
    | If_icmpgt i -> ("if_icmpgt", string_of_int i)
    | If_icmple i -> ("if_icmple", string_of_int i)
    | Ladd -> ("ladd", "")
    | Getfield i -> ("getfield", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Putfield i -> ("putfield", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Getstatic i -> ("getstatic", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Putstatic i -> ("putstatic", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Aconst_null -> ("aconst_null", "")
    | Ifnull i -> ("ifnull", string_of_int i)
    | Ifnonnull i -> ("ifnonnull", string_of_int i)
    | Checkcast i -> ("checkcast", i.name)
    | Instanceof i -> ("instanceof", i.name)
    | Monitorenter -> ("monitorenter", "")
    | Monitorexit -> ("monitorexit", "")
    | Arraylength -> ("arraylength", "")
    | Aaload -> ("aaload", "")
    | Iinc (i, j) -> ("iinc", Printf.sprintf "%d %d" i j)
    | Isub -> ("isub", "")
    | Bipush i -> ("bipush", string_of_int i)
    | Sipush i -> ("sipush", string_of_int i)
    | Anewarray i -> ("anewarray", i.name)
    | I2b -> ("i2b", "")
    | I2c -> ("i2c", "")
    | I2d -> ("i2d", "")
    | Ishl -> ("ishl", "")
    | Baload -> ("baload", "")
    | Iand -> ("iand", "")
    | Ior -> ("ior", "")
    | Bastore -> ("bastore", "")
    | Newarray t -> ("newarray", Vtype.string_of_arraytype t)
    | Fload i -> ("fload", string_of_int i)
    | Fload_0 -> ("fload_0", "")
    | Fload_1 -> ("fload_1", "")
    | Fload_2 -> ("fload_2", "")
    | Fload_3 -> ("fload_3", "")
    | Dload i -> ("dload", string_of_int i)
    | Dload_0 -> ("dload_0", "")
    | Dload_1 -> ("dload_1", "")
    | Dload_2 -> ("dload_2", "")
    | Dload_3 -> ("dload_3", "")
    | F2d -> ("f2d", "")
    | Dmul -> ("dmul", "")
    | D2i -> ("d2i", "")
    | Ishr -> ("ishr", "")
    | Ixor -> ("ixor", "")
    | Imul -> ("imul", "")
    | I2l -> ("i2l", "")
    | Lsub -> ("lsub", "")
    | Aastore -> ("aastore", "")
    | Lmul -> ("lmul", "")
    | Lshl -> ("lshl", "")
    | Dup2 -> ("dup2", "")
    | L2i -> ("l2i", "")
    | Invokedynamic x ->
        ( "invokedynamic",
          Printf.sprintf "#%d %s %s" x.bootstrap_idx x.name x.desc )
    | Caload -> ("caload", "")
    | Lookupswitch (default, pairs) ->
        ( "lookupswitch",
          Printf.sprintf "{%sdefault: %d}"
            (pairs
            |> List.map (fun (k, v) ->
                   string_of_int k ^ ": " ^ string_of_int v ^ ", ")
            |> String.concat "")
            default )
    | Tableswitch (default, (low, high), targets) ->
        ( "tableswitch",
          Printf.sprintf "(%d-%d) {%sdefault: %d}" low high
            (targets
            |> List.map (fun v -> string_of_int v ^ ", ")
            |> String.concat "")
            default )
    | Castore -> ("castore", "")
    | Idiv -> ("idiv", "")
    | Land -> ("land", "")
    | Ineg -> ("ineg", "")
    | Dadd -> ("dadd", "")
    | Dup_x1 -> ("dup_x1", "")
    | Lor -> ("lor", "")
    | Lreturn -> ("lreturn", "")
    | Nop -> ("nop", "")
    | Fconst_0 -> ("fconst_0", "")
    | Fconst_1 -> ("fconst_1", "")
    | Fconst_2 -> ("fconst_2", "")
    | Dconst_0 -> ("dconst_0", "")
    | Dconst_1 -> ("dconst_1", "")
    | Iaload -> ("iaload", "")
    | Laload -> ("laload", "")
    | Faload -> ("faload", "")
    | Daload -> ("daload", "")
    | Saload -> ("daload", "")
    | Fstore i -> ("fstore", string_of_int i)
    | Dstore i -> ("dstore", string_of_int i)
    | Fstore_0 -> ("fstore_0", "")
    | Fstore_1 -> ("fstore_1", "")
    | Fstore_2 -> ("fstore_2", "")
    | Fstore_3 -> ("fstore_3", "")
    | Dstore_0 -> ("dstore_0", "")
    | Dstore_1 -> ("dstore_1", "")
    | Dstore_2 -> ("dstore_2", "")
    | Dstore_3 -> ("dstore_3", "")
    | Iastore -> ("iastore", "")
    | Lastore -> ("lastore", "")
    | Fastore -> ("fastore", "")
    | Dastore -> ("dastore", "")
    | Sastore -> ("sastore", "")
    | Pop2 -> ("pop2", "")
    | Dup_x2 -> ("dup_x2", "")
    | Dup2_x1 -> ("dup2_x1", "")
    | Dup2_x2 -> ("dup2_x2", "")
    | Swap -> ("swap", "")
    | Fadd -> ("fadd", "")
    | Fsub -> ("fsub", "")
    | Dsub -> ("dsub", "")
    | Fmul -> ("fmul", "")
    | Ldiv -> ("ldiv", "")
    | Fdiv -> ("fdiv", "")
    | Ddiv -> ("ddiv", "")
    | Irem -> ("irem", "")
    | Lrem -> ("lrem", "")
    | Frem -> ("frem", "")
    | Drem -> ("drem", "")
    | Lneg -> ("lneg", "")
    | Fneg -> ("fneg", "")
    | Dneg -> ("dneg", "")
    | Lshr -> ("lshr", "")
    | Iushr -> ("iushr", "")
    | Lushr -> ("lushr", "")
    | Lxor -> ("lxor", "")
    | I2f -> ("i2f", "")
    | L2f -> ("l2f", "")
    | L2d -> ("l2d", "")
    | F2i -> ("f2i", "")
    | F2l -> ("f2l", "")
    | D2l -> ("d2l", "")
    | D2f -> ("d2f", "")
    | I2s -> ("i2s", "")
    | Fcmpl -> ("fcmpl", "")
    | Fcmpg -> ("fcmpg", "")
    | Dcmpl -> ("dcmpl", "")
    | Dcmpg -> ("dcmpg", "")
  in
  let mnemonic, args = inner i in
  Printf.sprintf "%-13s %s" mnemonic args

type instruction = int * instrbody
