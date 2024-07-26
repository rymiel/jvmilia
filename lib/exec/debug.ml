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
  | Some "full" -> false
  | Some _ | None -> true

let silent =
  match Sys.getenv_opt "JVMILIA_EXEC_LOG" with
  | Some "full" | Some "concise" -> false
  | Some _ | None -> true

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

let args (arr : Basic.evalue list) : unit =
  if not silent then (
    print_string (String.make (!depth * indent) ' ');
    print_string "\027[93m>>>>\027[0m ";
    Printf.printf "[%s]\n"
      (String.concat ", " (List.map Basic.string_of_evalue arr)))

let fields (kind : string) (name : string)
    (fields : Basic.evalue ref Basic.StringMap.t) : unit =
  if not silent then (
    print_string (String.make (!depth * indent) ' ');
    Printf.printf "[%s] %s: %s\n" kind name
      (String.concat ", "
         (List.map
            (fun (k, v) -> Printf.sprintf "%s %s" k (Basic.string_of_evalue !v))
            (Basic.StringMap.to_list fields))))

let native_call (handle : int) (arg_types : Type.dtype list)
    (ret_type : Type.dtype) (values : Basic.evalue list) : unit =
  if not silent then
    Printf.printf "%#x ((%s) -> %s) [%s]\n%!" handle
      (String.concat ", " (List.map Type.string_of_dtype arg_types))
      (Type.string_of_dtype ret_type)
      (String.concat ", "
         (List.map
            (if concise then Basic.string_of_evalue
             else Basic.string_of_evalue_detailed ~seen:Basic.IntSet.empty)
            values))
