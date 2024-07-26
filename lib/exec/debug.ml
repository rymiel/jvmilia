let indent = 2
let depth = ref 0

type entry = string

let stack = ref []
let stack_push (v : entry) = stack := v :: !stack

let stack_pop () : entry =
  match !stack with
  | [] -> failwith "Debug stack is empty"
  | x :: xs ->
      stack := xs;
      x

let concise =
  match Sys.getenv_opt "JVMILIA_EXEC_LOG" with
  | Some "concise" | Some "silent" -> true
  | Some _ | None -> false

let silent =
  match Sys.getenv_opt "JVMILIA_EXEC_LOG" with
  | Some "silent" -> true
  | Some _ | None -> false

let push (ctx : string) (s : string) : unit =
  if not silent then (
    print_string (String.make (!depth * indent) ' ');
    print_string "\027[34m>>\027[0m ";
    let prefix = Printf.sprintf "%s: %s" ctx s in
    print_string prefix;
    print_newline ();
    stack_push prefix;
    incr depth)

let pop () : unit =
  if not silent then (
    let top = stack_pop () in
    decr depth;
    print_string (String.make (!depth * indent) ' ');
    print_string "\027[34m<<\027[0m ";
    print_string top;
    print_newline ();
    ())

let instr (i : Instr.instrbody) (offset : int) : unit =
  if not silent then (
    print_string (String.make (!depth * indent) ' ');
    Printf.printf "** %4d: \027[36m%s\027[0m\n" offset (Instr.string_of_instr i))

let frame (f : Basic.exec_frame) : unit =
  if not silent then (
    print_string (String.make (!depth * indent) ' ');
    print_string "::";
    print_string (Basic.string_of_frame f);
    print_newline ())

let frame_detailed (f : Basic.exec_frame) : unit =
  if not concise then (
    incr depth;
    let print_indent () = print_string (String.make (!depth * indent) ' ') in
    Array.iteri
      (fun i (v : Basic.evalue) ->
        print_indent ();
        Printf.printf "↓↓local(%d) %s\n%!" i (Basic.string_of_evalue_detailed v))
      f.locals;
    List.iteri
      (fun i (v : Basic.evalue) ->
        print_indent ();
        Printf.printf "↑↑stack(%d) %s\n%!" i (Basic.string_of_evalue_detailed v))
      f.stack;
    decr depth)
