type reader = { read : bytes -> int -> unit option; tell : unit -> int }

let ch_reader (ch : in_channel) : reader =
  {
    read = (fun buf size -> In_channel.really_input ch buf 0 size);
    tell = (fun () -> Stdlib.pos_in ch);
  }

let bytes_reader (inbuf : bytes) : reader =
  let offset = ref 0 in
  let max = Bytes.length inbuf in
  let read outbuf size =
    if !offset + size > max then None
    else
      let () = Bytes.blit inbuf !offset outbuf 0 size in
      offset := !offset + size;
      Some ()
  in
  let tell () = !offset in
  { read; tell }

let really_read (r : reader) (buf : bytes) (size : int) : unit =
  let res = r.read buf size in
  match res with Some () -> () | None -> raise End_of_file

let read_bytes (r : reader) (size : int) (decoder : bytes -> int -> 'a) : 'a =
  let buf = Bytes.create size in
  let () = really_read r buf size in
  decoder buf 0

let read_bytes_opt (r : reader) (size : int) (decoder : bytes -> int -> 'a) :
    'a option =
  let buf = Bytes.create size in
  let res = r.read buf size in
  match res with Some () -> Some (decoder buf 0) | None -> None

let read_u8 (r : reader) : int64 = read_bytes r 8 Bytes.get_int64_be
let read_u4 (r : reader) : int32 = read_bytes r 4 Bytes.get_int32_be

let read_u4_int (r : reader) : int =
  read_bytes r 4 Bytes.get_int32_be |> Int32.to_int
(* TODO: ideally i'd add like a check here that this isn't lossy *)

let hex_u4 (num : int32) = Printf.sprintf "0x%04lX" num
let read_u2 (r : reader) : int = read_bytes r 2 Bytes.get_uint16_be
let read_i2 (r : reader) : int = read_bytes r 2 Bytes.get_int16_be
let hex_u2 (num : int) = Printf.sprintf "0x%04X" num
let read_u1 (r : reader) : int = read_bytes r 1 Bytes.get_uint8
let read_i1 (r : reader) : int = read_bytes r 1 Bytes.get_int8
let read_u1_opt (r : reader) : int option = read_bytes_opt r 1 Bytes.get_uint8
let hex_u1 (num : int) = Printf.sprintf "0x%02X" num

let read_list (r : reader) (decoder : reader -> 'a) : 'a list =
  let len = read_u2 r in
  List.init len (fun _ -> decoder r)

let read_list_sized (r : reader) (len : int) (decoder : reader -> 'a) : 'a list
    =
  List.init len (fun _ -> decoder r)

let assert_end_of_file (r : reader) : unit =
  let buf = Bytes.create 1 in
  let res = r.read buf 1 in
  match res with Some () -> failwith "Expected end of file" | None -> ()

let skip (r : reader) (n : int) : unit =
  let buf = Bytes.create n in
  really_read r buf n
