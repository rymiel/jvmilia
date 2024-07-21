open Shared

type arith_op = Add | Sub | Mul | Div | Rem

let string_of_arith_op (op : arith_op) : string =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Rem -> "rem"

type if_cond = Eq | Ne | Lt | Ge | Gt | Le

let string_of_if_cond (cond : if_cond) : string =
  match cond with
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt -> "lt"
  | Ge -> "ge"
  | Gt -> "gt"
  | Le -> "le"

type instrbody =
  (* constants *)
  | Nop
  | Aconst_null
  | Iconst of int32
  | Lconst of int64
  | Fconst of float
  | Dconst of float
  | Bipush of int32
  | Sipush of int32
  | Ldc of loadable_constant
  | Ldc2_w of loadable_constant2
  (* loads *)
  | Iload of int
  | Lload of int
  | Fload of int
  | Dload of int
  | Aload of int
  | Iaload
  | Laload
  | Faload
  | Daload
  | Aaload
  | Baload
  | Caload
  | Saload
  (* stores *)
  | Istore of int
  | Lstore of int
  | Fstore of int
  | Dstore of int
  | Astore of int
  | Iastore
  | Lastore
  | Fastore
  | Dastore
  | Aastore
  | Bastore
  | Castore
  | Sastore
  (* stack *)
  | Pop
  | Pop2
  | Dup
  | Dup_x1
  | Dup_x2
  | Dup2
  | Dup2_x1
  | Dup2_x2
  | Swap
  (* math *)
  | Iarith of arith_op
  | Larith of arith_op
  | Farith of arith_op
  | Darith of arith_op
  | Ineg
  | Lneg
  | Fneg
  | Dneg
  | Ishl
  | Lshl
  | Ishr
  | Lshr
  | Iushr
  | Lushr
  | Iand
  | Land
  | Ior
  | Lor
  | Ixor
  | Lxor
  | Iinc of int * int
  (* conversions *)
  | I2l
  | I2f
  | I2d
  | L2i
  | L2f
  | L2d
  | F2i
  | F2l
  | F2d
  | D2i
  | D2l
  | D2f
  | I2b
  | I2c
  | I2s
  (* comparisons *)
  | Lcmp
  | Fcmpl
  | Fcmpg
  | Dcmpl
  | Dcmpg
  | If of if_cond * int
  | If_icmp of if_cond * int
  | If_acmpeq of int
  | If_acmpne of int
  (* control *)
  | Goto of int
  | Jsr of int
  | Ret of int
  | Tableswitch of int * (int * int) * int list
  | Lookupswitch of int * (int * int) list
  | Ireturn
  | Lreturn
  | Freturn
  | Dreturn
  | Areturn
  | Return
  (* references *)
  | Getstatic of field_desc
  | Putstatic of field_desc
  | Getfield of field_desc
  | Putfield of field_desc
  | Invokevirtual of method_desc
  | Invokespecial of method_desc
  | Invokestatic of method_desc
  | Invokeinterface of method_desc * int
  | Invokedynamic of Shared.dynamic_desc
  | New of class_desc
  | Newarray of Vtype.arraytype
  | Anewarray of class_desc
  | Arraylength
  | Athrow
  | Checkcast of class_desc
  | Instanceof of class_desc
  | Monitorenter
  | Monitorexit
  (* extended *)
  | Multianewarray of class_desc * int
  | Ifnull of int
  | Ifnonnull of int

let string_of_instr (i : instrbody) : string =
  let inner = function
    | Nop -> ("nop", "")
    | Aconst_null -> ("aconst_null", "")
    | Iconst i -> ("iconst", Int32.to_string i)
    | Lconst i -> ("lconst", Int64.to_string i)
    | Fconst i -> ("fconst", string_of_float i)
    | Dconst i -> ("dconst", string_of_float i)
    | Bipush i -> ("bipush", Int32.to_string i)
    | Sipush i -> ("sipush", Int32.to_string i)
    | Ldc i -> ("ldc", string_of_loadable_constant i)
    | Ldc2_w i -> ("ldc2_w", string_of_loadable_constant2 i)
    | Iload i -> ("iload", string_of_int i)
    | Lload i -> ("lload", string_of_int i)
    | Fload i -> ("fload", string_of_int i)
    | Dload i -> ("dload", string_of_int i)
    | Aload i -> ("aload", string_of_int i)
    | Iaload -> ("iaload", "")
    | Laload -> ("laload", "")
    | Faload -> ("faload", "")
    | Daload -> ("daload", "")
    | Aaload -> ("aaload", "")
    | Baload -> ("baload", "")
    | Caload -> ("caload", "")
    | Saload -> ("saload", "")
    | Istore i -> ("istore", string_of_int i)
    | Lstore i -> ("lstore", string_of_int i)
    | Fstore i -> ("fstore", string_of_int i)
    | Dstore i -> ("dstore", string_of_int i)
    | Astore i -> ("astore", string_of_int i)
    | Iastore -> ("iastore", "")
    | Lastore -> ("lastore", "")
    | Fastore -> ("fastore", "")
    | Dastore -> ("dastore", "")
    | Aastore -> ("aastore", "")
    | Bastore -> ("bastore", "")
    | Castore -> ("castore", "")
    | Sastore -> ("sastore", "")
    | Pop -> ("pop", "")
    | Pop2 -> ("pop2", "")
    | Dup -> ("dup", "")
    | Dup_x1 -> ("dup_x1", "")
    | Dup_x2 -> ("dup_x2", "")
    | Dup2 -> ("dup2", "")
    | Dup2_x1 -> ("dup2_x1", "")
    | Dup2_x2 -> ("dup2_x2", "")
    | Swap -> ("swap", "")
    | Iarith op -> (Printf.sprintf "i%s" (string_of_arith_op op), "")
    | Larith op -> (Printf.sprintf "l%s" (string_of_arith_op op), "")
    | Farith op -> (Printf.sprintf "f%s" (string_of_arith_op op), "")
    | Darith op -> (Printf.sprintf "d%s" (string_of_arith_op op), "")
    | Ineg -> ("ineg", "")
    | Lneg -> ("lneg", "")
    | Fneg -> ("fneg", "")
    | Dneg -> ("dneg", "")
    | Ishl -> ("ishl", "")
    | Lshl -> ("lshl", "")
    | Ishr -> ("ishr", "")
    | Lshr -> ("lshr", "")
    | Iushr -> ("iushr", "")
    | Lushr -> ("lushr", "")
    | Iand -> ("iand", "")
    | Land -> ("land", "")
    | Ior -> ("ior", "")
    | Lor -> ("lor", "")
    | Ixor -> ("ixor", "")
    | Lxor -> ("lxor", "")
    | Iinc (i, j) -> ("iinc", Printf.sprintf "%d %d" i j)
    | I2l -> ("i2l", "")
    | I2f -> ("i2f", "")
    | I2d -> ("i2d", "")
    | L2i -> ("l2i", "")
    | L2f -> ("l2f", "")
    | L2d -> ("l2d", "")
    | F2i -> ("f2i", "")
    | F2l -> ("f2l", "")
    | F2d -> ("f2d", "")
    | D2i -> ("d2i", "")
    | D2l -> ("d2l", "")
    | D2f -> ("d2f", "")
    | I2b -> ("i2b", "")
    | I2c -> ("i2c", "")
    | I2s -> ("i2s", "")
    | Lcmp -> ("lcmp", "")
    | Fcmpl -> ("fcmpl", "")
    | Fcmpg -> ("fcmpg", "")
    | Dcmpl -> ("dcmpl", "")
    | Dcmpg -> ("dcmpg", "")
    | If (cond, i) ->
        (Printf.sprintf "if%s" (string_of_if_cond cond), string_of_int i)
    | If_icmp (cond, i) ->
        (Printf.sprintf "if_icmp%s" (string_of_if_cond cond), string_of_int i)
    | If_acmpeq i -> ("if_acmpeq", string_of_int i)
    | If_acmpne i -> ("if_acmpne", string_of_int i)
    | Goto i -> ("goto", string_of_int i)
    | Jsr i -> ("jsr", string_of_int i)
    | Ret i -> ("ret", string_of_int i)
    | Tableswitch (default, (low, high), targets) ->
        ( "tableswitch",
          Printf.sprintf "(%d-%d) {%sdefault: %d}" low high
            (targets
            |> List.map (fun v -> string_of_int v ^ ", ")
            |> String.concat "")
            default )
    | Lookupswitch (default, pairs) ->
        ( "lookupswitch",
          Printf.sprintf "{%sdefault: %d}"
            (pairs
            |> List.map (fun (k, v) ->
                   string_of_int k ^ ": " ^ string_of_int v ^ ", ")
            |> String.concat "")
            default )
    | Ireturn -> ("ireturn", "")
    | Lreturn -> ("lreturn", "")
    | Freturn -> ("freturn", "")
    | Dreturn -> ("dreturn", "")
    | Areturn -> ("areturn", "")
    | Return -> ("return", "")
    | Getstatic i -> ("getstatic", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Putstatic i -> ("putstatic", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Getfield i -> ("getfield", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Putfield i -> ("putfield", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Invokevirtual i ->
        ("invokevirtual", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Invokespecial i ->
        ("invokespecial", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Invokestatic i ->
        ("invokestatic", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Invokeinterface (i, _) ->
        ("invokeinterface", Printf.sprintf "%s.%s %s" i.cls i.name i.desc)
    | Invokedynamic x ->
        ( "invokedynamic",
          Printf.sprintf "#%d %s %s" x.bootstrap_idx x.name x.desc )
    | New i -> ("new", i.name)
    | Newarray t -> ("newarray", Vtype.string_of_arraytype t)
    | Anewarray i -> ("anewarray", i.name)
    | Arraylength -> ("arraylength", "")
    | Athrow -> ("athrow", "")
    | Checkcast i -> ("checkcast", i.name)
    | Instanceof i -> ("instanceof", i.name)
    | Monitorenter -> ("monitorenter", "")
    | Monitorexit -> ("monitorexit", "")
    | Multianewarray (c, i) -> ("dreturn", Printf.sprintf "%s %d" c.name i)
    | Ifnull i -> ("ifnull", string_of_int i)
    | Ifnonnull i -> ("ifnonnull", string_of_int i)
  in
  let mnemonic, args = inner i in
  Printf.sprintf "%-13s %s" mnemonic args

type instruction = int * instrbody

module IntMap = Map.Make (Int)

type mappedinstr = { body : instrbody; next : int }
type instrmap = mappedinstr IntMap.t

let map_instrs (instrlist : instruction list) : instrmap =
  let rev = List.rev instrlist in
  let fold (map, next) (addr, body) =
    (IntMap.add addr { body; next } map, addr)
  in
  (* -1 means "no next instruction" *)
  let result, _ = List.fold_left fold (IntMap.empty, -1) rev in
  result
