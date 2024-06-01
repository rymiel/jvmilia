open Constpool
open Instr

let read_instr (pos : int) (opcode : int) (pool : const_pool) (r : Io.reader) :
    instrbody =
  let branchoffset () = pos + Io.read_i2 r in
  match opcode with
  | 0x01 -> Aconst_null
  | 0x02 -> Iconst_m1
  | 0x03 -> Iconst_0
  | 0x04 -> Iconst_1
  | 0x05 -> Iconst_2
  | 0x06 -> Iconst_3
  | 0x07 -> Iconst_4
  | 0x08 -> Iconst_5
  | 0x09 -> Lconst_0
  | 0x0a -> Lconst_1
  | 0x10 -> Bipush (Io.read_u1 r)
  | 0x12 ->
      let const = Io.read_u1 r |> const_pool_loadable_constant pool in
      Ldc const
  | 0x13 ->
      let const = Io.read_u2 r |> const_pool_loadable_constant pool in
      Ldc_w const
  | 0x14 ->
      let const = Io.read_u2 r |> const_pool_loadable_constant2 pool in
      Ldc2_w const
  | 0x15 -> Iload (Io.read_u1 r)
  | 0x19 -> Aload (Io.read_u1 r)
  | 0x1a -> Iload_0
  | 0x1b -> Iload_1
  | 0x1c -> Iload_2
  | 0x1d -> Iload_3
  | 0x1e -> Lload_0
  | 0x1f -> Lload_1
  | 0x20 -> Lload_2
  | 0x21 -> Lload_3
  | 0x2a -> Aload_0
  | 0x2b -> Aload_1
  | 0x2c -> Aload_2
  | 0x2d -> Aload_3
  | 0x32 -> Aaload
  | 0x36 -> Istore (Io.read_u1 r)
  | 0x3a -> Astore (Io.read_u1 r)
  | 0x3b -> Istore_0
  | 0x3c -> Istore_1
  | 0x3d -> Istore_2
  | 0x3e -> Istore_3
  | 0x3f -> Lstore_0
  | 0x40 -> Lstore_1
  | 0x41 -> Lstore_2
  | 0x42 -> Lstore_3
  | 0x4b -> Astore_0
  | 0x4c -> Astore_1
  | 0x4d -> Astore_2
  | 0x4e -> Astore_3
  | 0x57 -> Pop
  | 0x59 -> Dup
  | 0x60 -> Iadd
  | 0x61 -> Ladd
  | 0x64 -> Isub
  | 0x84 ->
      let index = Io.read_u1 r in
      let const = Io.read_u1 r in
      Iinc (index, const)
  | 0x94 -> Lcmp
  | 0x99 -> Ifeq (branchoffset ())
  | 0x9a -> Ifne (branchoffset ())
  | 0x9b -> Iflt (branchoffset ())
  | 0x9c -> Ifge (branchoffset ())
  | 0x9d -> Ifgt (branchoffset ())
  | 0x9e -> Ifle (branchoffset ())
  | 0x9f -> If_icmpeq (branchoffset ())
  | 0xa0 -> If_icmpne (branchoffset ())
  | 0xa1 -> If_icmplt (branchoffset ())
  | 0xa2 -> If_icmpge (branchoffset ())
  | 0xa3 -> If_icmpgt (branchoffset ())
  | 0xa4 -> If_icmple (branchoffset ())
  | 0xa5 -> If_acmpeq (branchoffset ())
  | 0xa6 -> If_acmpne (branchoffset ())
  | 0xa7 -> Goto (branchoffset ())
  | 0xac -> Ireturn
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
      let mth = Io.read_u2 r |> const_pool_method pool in
      Invokespecial mth
  | 0xb8 ->
      let mth = Io.read_u2 r |> const_pool_method pool in
      Invokestatic mth
  | 0xb9 ->
      let mth = Io.read_u2 r |> const_pool_interface_method pool in
      let count = Io.read_u1 r in
      let zero = Io.read_u1 r in
      assert (zero = 0);
      Invokeinterface (mth, count)
  | 0xbb ->
      let cls = Io.read_u2 r |> const_pool_class pool in
      New cls
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
