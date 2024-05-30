let read_bytes (ch : in_channel) (size : int) (reader : bytes -> int -> 'a) : 'a
    =
  let buf = Bytes.create size in
  let () = Stdlib.really_input ch buf 0 size in
  reader buf 0

let read_u4 (ch : in_channel) : int32 = read_bytes ch 4 Bytes.get_int32_be
let hex_u4 (num : int32) = Printf.sprintf "0x%04lX" num
let read_u2 (ch : in_channel) : int = read_bytes ch 2 Bytes.get_int16_be
let hex_u2 (num : int) = Printf.sprintf "0x%04X" num
let read_u1 (ch : in_channel) : int = read_bytes ch 1 Bytes.get_int8
let hex_u1 (num : int) = Printf.sprintf "0x%04X" num

let read_list (ch : in_channel) (reader : in_channel -> 'a) : 'a list =
  let len = read_u2 ch in
  let rec reads (list : 'a list) (n : int) : 'a list =
    if n = 0 then list else reads (list @ [ reader ch ]) (n - 1)
  in
  reads [] len

let read_list0 (ch : in_channel) (reader : in_channel -> 'a) : 'a list =
  let len = read_u2 ch - 1 in
  let rec reads (list : 'a list) (n : int) : 'a list =
    if n = 0 then list else reads (list @ [ reader ch ]) (n - 1)
  in
  reads [] len

let assert_end_of_file (ch : in_channel) : unit =
  let buf = Bytes.create 1 in
  let res = In_channel.really_input ch buf 0 1 in
  match res with Some () -> failwith "Expected end of file" | None -> ()
