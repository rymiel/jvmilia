let indent = 2
let depth = ref 0

type entry = string * int ref

let stack = ref []
let pending = ref false
let stack_push (v : entry) = stack := v :: !stack

let stack_pop () : entry =
  match !stack with
  | [] -> failwith "Debug stack is empty"
  | x :: xs ->
      stack := xs;
      x

let stack_top () : entry = List.hd !stack
let class_diagnostic (cls : Java.jclass) : string = cls.name

let method_diagnostic (mth : Java.jmethod) (cls : Java.jclass) : string =
  Printf.sprintf "%s.%s %s" cls.name mth.name mth.desc

let env_diagnostic (env : Basic.jenvironment) : string =
  Printf.sprintf "%s.%s %s" env.cls.name env.mth.name env.mth.desc

let push (ctx : string) (s : string) : unit =
  print_string (String.make (!depth * indent) ' ');
  print_string "\027[34m>>\027[0m ";
  let prefix = Printf.sprintf "%s: %s" ctx s in
  print_string prefix;
  print_newline ();
  stack_push (prefix, ref 1);
  incr depth

let push_rec (ctx : string) (s : string) : unit =
  let prefix = Printf.sprintf "%s: %s" ctx s in
  let top_p, top_c = stack_top () in
  if prefix = top_p then incr top_c
  else (
    stack_push (prefix, ref 1);
    print_string (String.make (!depth * indent) ' ');
    print_string "\027[34m>>\027[0m ";
    print_string prefix;
    print_newline ();
    incr depth)

let pop (result : bool) : bool =
  let top_p, top_c = stack_pop () in
  if !top_c > 1 then (
    stack_push (top_p, top_c);
    decr top_c;
    result)
  else (
    decr depth;
    print_string (String.make (!depth * indent) ' ');
    print_string (if result then "\027[32m<<\027[0m " else "\027[31m<<\027[0m ");
    print_string top_p;
    print_string ": ";
    print_string
      (if result then "\027[32mtrue\027[0m" else "\027[31mfalse\027[0m");
    print_newline ();
    result)

let instr (i : Instr.instrbody) (offset : int) : unit =
  print_string (String.make (!depth * indent) ' ');
  Printf.printf "** %4d: %s\n" offset (Instr.string_of_instr i)

let frame (f : Basic.frame) : unit =
  print_string (String.make (!depth * indent) ' ');
  print_string "::";
  print_string (Basic.string_of_frame f);
  print_newline ()

let after_goto () : unit =
  print_string (String.make (!depth * indent) ' ');
  print_string ":: <after goto>";
  print_newline ()
