open Constpool
open Instr

let read_newarray_type (atype : int) : Type.dtype =
  match atype with
  | 4 -> Boolean
  | 5 -> Char
  | 6 -> Float
  | 7 -> Double
  | 8 -> Byte
  | 9 -> Short
  | 10 -> Int
  | 11 -> Long
  | _ -> failwith "Invalid array type"

let read_instr (pos : int) (opcode : int) (pool : const_pool) (r : Io.reader) :
    instrbody =
  let branchoffset () = pos + Io.read_i2 r in
  let modulo x y =
    let result = x mod y in
    if result >= 0 then result else result + y
  in
  let pad x = modulo ~-x 4 in
  match opcode with
  | 0x00 -> Nop
  | 0x01 -> Aconst_null
  | 0x02 -> Iconst (-1l)
  | 0x03 -> Iconst 0l
  | 0x04 -> Iconst 1l
  | 0x05 -> Iconst 2l
  | 0x06 -> Iconst 3l
  | 0x07 -> Iconst 4l
  | 0x08 -> Iconst 5l
  | 0x09 -> Lconst 0L
  | 0x0a -> Lconst 1L
  | 0x0b -> Fconst 0.0
  | 0x0c -> Fconst 1.0
  | 0x0d -> Fconst 2.0
  | 0x0e -> Dconst 0.0
  | 0x0f -> Dconst 1.0
  | 0x10 -> Bipush (Io.read_i1 r |> Int32.of_int)
  | 0x11 -> Sipush (Io.read_i2 r |> Int32.of_int)
  | 0x12 ->
      let const = Io.read_u1 r |> const_pool_loadable_constant pool in
      Ldc const
  | 0x13 ->
      let const = Io.read_u2 r |> const_pool_loadable_constant pool in
      Ldc const
  | 0x14 ->
      let const = Io.read_u2 r |> const_pool_loadable_constant2 pool in
      Ldc2_w const
  | 0x15 -> Iload (Io.read_u1 r)
  | 0x16 -> Lload (Io.read_u1 r)
  | 0x17 -> Fload (Io.read_u1 r)
  | 0x18 -> Dload (Io.read_u1 r)
  | 0x19 -> Aload (Io.read_u1 r)
  | 0x1a -> Iload 0
  | 0x1b -> Iload 1
  | 0x1c -> Iload 2
  | 0x1d -> Iload 3
  | 0x1e -> Lload 0
  | 0x1f -> Lload 1
  | 0x20 -> Lload 2
  | 0x21 -> Lload 3
  | 0x22 -> Fload 0
  | 0x23 -> Fload 1
  | 0x24 -> Fload 2
  | 0x25 -> Fload 3
  | 0x26 -> Dload 0
  | 0x27 -> Dload 1
  | 0x28 -> Dload 2
  | 0x29 -> Dload 3
  | 0x2a -> Aload 0
  | 0x2b -> Aload 1
  | 0x2c -> Aload 2
  | 0x2d -> Aload 3
  | 0x2e -> Iaload
  | 0x2f -> Laload
  | 0x30 -> Faload
  | 0x31 -> Daload
  | 0x32 -> Aaload
  | 0x33 -> Baload
  | 0x34 -> Caload
  | 0x35 -> Saload
  | 0x36 -> Istore (Io.read_u1 r)
  | 0x37 -> Lstore (Io.read_u1 r)
  | 0x38 -> Fstore (Io.read_u1 r)
  | 0x39 -> Dstore (Io.read_u1 r)
  | 0x3a -> Astore (Io.read_u1 r)
  | 0x3b -> Istore 0
  | 0x3c -> Istore 1
  | 0x3d -> Istore 2
  | 0x3e -> Istore 3
  | 0x3f -> Lstore 0
  | 0x40 -> Lstore 1
  | 0x41 -> Lstore 2
  | 0x42 -> Lstore 3
  | 0x43 -> Fstore 0
  | 0x44 -> Fstore 1
  | 0x45 -> Fstore 2
  | 0x46 -> Fstore 3
  | 0x47 -> Dstore 0
  | 0x48 -> Dstore 1
  | 0x49 -> Dstore 2
  | 0x4a -> Dstore 3
  | 0x4b -> Astore 0
  | 0x4c -> Astore 1
  | 0x4d -> Astore 2
  | 0x4e -> Astore 3
  | 0x4f -> Iastore
  | 0x50 -> Lastore
  | 0x51 -> Fastore
  | 0x52 -> Dastore
  | 0x53 -> Aastore
  | 0x54 -> Bastore
  | 0x55 -> Castore
  | 0x56 -> Sastore
  | 0x57 -> Pop
  | 0x58 -> Pop2
  | 0x59 -> Dup
  | 0x5a -> Dup_x1
  | 0x5b -> Dup_x2
  | 0x5c -> Dup2
  | 0x5d -> Dup2_x1
  | 0x5e -> Dup2_x2
  | 0x5f -> Swap
  | 0x60 -> Iarith Add
  | 0x61 -> Larith Add
  | 0x62 -> Farith Add
  | 0x63 -> Darith Add
  | 0x64 -> Iarith Sub
  | 0x65 -> Larith Sub
  | 0x66 -> Farith Sub
  | 0x67 -> Darith Sub
  | 0x68 -> Iarith Mul
  | 0x69 -> Larith Mul
  | 0x6a -> Farith Mul
  | 0x6b -> Darith Mul
  | 0x6c -> Iarith Div
  | 0x6d -> Larith Div
  | 0x6e -> Farith Div
  | 0x6f -> Darith Div
  | 0x70 -> Iarith Rem
  | 0x71 -> Larith Rem
  | 0x72 -> Farith Rem
  | 0x73 -> Darith Rem
  | 0x74 -> Ineg
  | 0x75 -> Lneg
  | 0x76 -> Fneg
  | 0x77 -> Dneg
  | 0x78 -> Ishl
  | 0x79 -> Lshl
  | 0x7a -> Ishr
  | 0x7b -> Lshr
  | 0x7c -> Iushr
  | 0x7d -> Lushr
  | 0x7e -> Iand
  | 0x7f -> Land
  | 0x80 -> Ior
  | 0x81 -> Lor
  | 0x82 -> Ixor
  | 0x83 -> Lxor
  | 0x84 ->
      let index = Io.read_u1 r in
      let const = Io.read_u1 r in
      Iinc (index, const)
  | 0x85 -> I2l
  | 0x86 -> I2f
  | 0x87 -> I2d
  | 0x88 -> L2i
  | 0x89 -> L2f
  | 0x8a -> L2d
  | 0x8b -> F2i
  | 0x8c -> F2l
  | 0x8d -> F2d
  | 0x8e -> D2i
  | 0x8f -> D2l
  | 0x90 -> D2f
  | 0x91 -> I2b
  | 0x92 -> I2c
  | 0x93 -> I2s
  | 0x94 -> Lcmp
  | 0x95 -> Fcmpl
  | 0x96 -> Fcmpg
  | 0x97 -> Dcmpl
  | 0x98 -> Dcmpg
  | 0x99 -> If (Eq, branchoffset ())
  | 0x9a -> If (Ne, branchoffset ())
  | 0x9b -> If (Lt, branchoffset ())
  | 0x9c -> If (Ge, branchoffset ())
  | 0x9d -> If (Gt, branchoffset ())
  | 0x9e -> If (Le, branchoffset ())
  | 0x9f -> If_icmp (Eq, branchoffset ())
  | 0xa0 -> If_icmp (Ne, branchoffset ())
  | 0xa1 -> If_icmp (Lt, branchoffset ())
  | 0xa2 -> If_icmp (Ge, branchoffset ())
  | 0xa3 -> If_icmp (Gt, branchoffset ())
  | 0xa4 -> If_icmp (Le, branchoffset ())
  | 0xa5 -> If_acmpeq (branchoffset ())
  | 0xa6 -> If_acmpne (branchoffset ())
  | 0xa7 -> Goto (branchoffset ())
  | 0xa8 -> Jsr (branchoffset ())
  | 0xa9 -> Ret (Io.read_u1 r)
  | 0xaa ->
      let () = Io.skip r (pad (pos + 1)) in
      let default = pos + Io.read_u4_int r in
      let low = Io.read_u4_int r in
      let high = Io.read_u4_int r in
      let noffsets = high - low + 1 in
      let offsets =
        Io.read_list_sized r noffsets (fun r -> pos + Io.read_u4_int r)
      in
      Tableswitch (default, (low, high), offsets)
  | 0xab ->
      let () = Io.skip r (pad (pos + 1)) in
      let default = pos + Io.read_u4_int r in
      let npairs = Io.read_u4_int r in
      let pairs =
        Io.read_list_sized r npairs (fun r ->
            let m = Io.read_u4_int r in
            let offset = Io.read_u4_int r in
            (m, pos + offset))
      in
      Lookupswitch (default, pairs)
  | 0xac -> Ireturn
  | 0xad -> Lreturn
  | 0xae -> Freturn
  | 0xaf -> Dreturn
  | 0xb0 -> Areturn
  | 0xb1 -> Return
  | 0xb2 ->
      let field = Io.read_u2 r |> const_pool_field pool in
      Getstatic field
  | 0xb3 ->
      let field = Io.read_u2 r |> const_pool_field pool in
      Putstatic field
  | 0xb4 ->
      let field = Io.read_u2 r |> const_pool_field pool in
      Getfield field
  | 0xb5 ->
      let field = Io.read_u2 r |> const_pool_field pool in
      Putfield field
  | 0xb6 ->
      let mth = Io.read_u2 r |> const_pool_method pool in
      Invokevirtual mth
  | 0xb7 ->
      let mth = Io.read_u2 r |> const_pool_method_or_interface_method pool in
      Invokespecial mth
  | 0xb8 ->
      let mth = Io.read_u2 r |> const_pool_method_or_interface_method pool in
      Invokestatic mth
  | 0xb9 ->
      let mth = Io.read_u2 r |> const_pool_interface_method pool in
      let count = Io.read_u1 r in
      let zero = Io.read_u1 r in
      assert (zero = 0);
      Invokeinterface (mth, count)
  | 0xba ->
      let dyn = Io.read_u2 r |> const_pool_invokedynamic pool in
      let zero1 = Io.read_u1 r in
      assert (zero1 = 0);
      let zero2 = Io.read_u1 r in
      assert (zero2 = 0);
      Invokedynamic dyn
  | 0xbb ->
      let cls = Io.read_u2 r |> const_pool_class pool in
      New cls
  | 0xbc ->
      let t = Io.read_u1 r |> read_newarray_type in
      Newarray t
  | 0xbd ->
      let cls = Io.read_u2 r |> const_pool_class pool in
      Anewarray cls
  | 0xbe -> Arraylength
  | 0xbf -> Athrow
  | 0xc0 ->
      let cls = Io.read_u2 r |> const_pool_class pool in
      Checkcast cls
  | 0xc1 ->
      let cls = Io.read_u2 r |> const_pool_class pool in
      Instanceof cls
  | 0xc2 -> Monitorenter
  | 0xc3 -> Monitorexit
  | 0xc4 -> failwith "wide :("
  | 0xc5 ->
      let cls = Io.read_u2 r |> const_pool_class pool in
      let dim = Io.read_u1 r in
      Multianewarray (cls, dim)
  | 0xc6 -> Ifnull (branchoffset ())
  | 0xc7 -> Ifnonnull (branchoffset ())
  | x ->
      failwith
        (Printf.sprintf "Failed to read instruction with opcode 0x%02X" x)

let read_code (pool : const_pool) (r : Io.reader) : instruction list =
  let rec read (i : instruction list) : instruction list =
    let pos = r.tell () in
    let opcode = Io.read_u1_opt r in
    (* let () =
         Printf.printf "opcode: %s\n"
           (match opcode with Some x -> Io.hex_u1 x | None -> "eof")
       in *)
    match opcode with
    | Some v ->
        let body = read_instr pos v pool r in
        read (i @ [ (pos, body) ])
    | None -> i
  in
  read []
