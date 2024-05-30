open Constpool
open Instr

let read_instr (pool : const_pool) (r : Io.reader) : instrbody option =
  let opcode = Io.read_u1_opt r in
  let () =
    Printf.printf "opcode: %s\n"
      (match opcode with Some x -> Io.hex_u1 x | None -> "eof")
  in
  match opcode with
  | None -> None
  | Some 0x2a -> Some Aload_0
  | Some 0x2b -> Some Aload_1
  | Some 0x2c -> Some Aload_2
  | Some 0x2d -> Some Aload_3
  | Some 0xb7 ->
      let index = Io.read_u2 r in
      let mth = const_pool_method pool index in
      Some (Invokespecial mth)
  | Some x ->
      failwith
        (Printf.sprintf "Failed to read instruction with opcode %d (%s)" x
           (Io.hex_u1 x))

let read_code (pool : const_pool) (r : Io.reader) : instruction list =
  let rec read (i : instruction list) : instruction list =
    let pos = 0 (* TODO*) in
    match read_instr pool r with
    | Some body -> read (i @ [ (pos, body) ])
    | None -> i
  in
  read []
