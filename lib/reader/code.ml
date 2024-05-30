open Constpool
open Instr

let read_instr (opcode : int) (pool : const_pool) (r : Io.reader) : instrbody =
  match opcode with
  | 0x02 -> Iconst_m1
  | 0x03 -> Iconst_0
  | 0x04 -> Iconst_1
  | 0x05 -> Iconst_2
  | 0x06 -> Iconst_3
  | 0x07 -> Iconst_4
  | 0x08 -> Iconst_5
  | 0x12 ->
      let const = Io.read_u2 r |> const_pool_loadable_constant pool in
      Ldc const
  | 0x1a -> Iload_0
  | 0x1b -> Iload_1
  | 0x1c -> Iload_2
  | 0x1d -> Iload_3
  | 0x2a -> Aload_0
  | 0x2b -> Aload_1
  | 0x2c -> Aload_2
  | 0x2d -> Aload_3
  | 0x3b -> Istore_0
  | 0x3c -> Istore_1
  | 0x3d -> Istore_2
  | 0x3e -> Istore_3
  | 0x59 -> Dup
  | 0x60 -> Iadd
  | 0xa5 -> If_acmpeq (Io.read_u2 r)
  | 0xa6 -> If_acmpne (Io.read_u2 r)
  | 0xa7 -> Goto (Io.read_u2 r)
  | 0xac -> Ireturn
  | 0xb1 -> Return
  | 0xbb ->
      let cls = Io.read_u2 r |> const_pool_class pool in
      New cls
  | 0xb6 ->
      let mth = Io.read_u2 r |> const_pool_method pool in
      Invokevirtual mth
  | 0xb7 ->
      let mth = Io.read_u2 r |> const_pool_method pool in
      Invokespecial mth
  | x ->
      failwith
        (Printf.sprintf "Failed to read instruction with opcode 0x%02X" x)

let read_code (pool : const_pool) (r : Io.reader) : instruction list =
  let rec read (i : instruction list) : instruction list =
    let pos = 0 (* TODO*) in
    let opcode = Io.read_u1_opt r in
    (* let () =
         Printf.printf "opcode: %s\n"
           (match opcode with Some x -> Io.hex_u1 x | None -> "eof")
       in *)
    match opcode with
    | Some v ->
        let body = read_instr v pool r in
        read (i @ [ (pos, body) ])
    | None -> i
  in
  read []
