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

let push (ctx : string) (s : string) : unit =
  print_string (String.make (!depth * indent) ' ');
  print_string "\027[34m>>\027[0m ";
  let prefix = Printf.sprintf "%s: %s" ctx s in
  print_string prefix;
  print_newline ();
  stack_push prefix;
  incr depth

let pop () : unit =
  let top = stack_pop () in
  decr depth;
  print_string (String.make (!depth * indent) ' ');
  print_string "\027[34m<<\027[0m ";
  print_string top;
  print_newline ();
  ()
