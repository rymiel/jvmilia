open Shared
open Vtype
open Java

type flags = { is_this_uninit : bool }
type frame = { locals : vtype list; stack : vtype list; flags : flags }
type jstack_map = int * frame

let string_of_stack (stack : vtype list) : string =
  let stack_s = List.map string_of_vtype stack in
  Printf.sprintf "<[%s]" (String.concat ", " stack_s)

let string_of_frame (f : frame) : string =
  let locals_s =
    List.mapi (fun i s -> Printf.sprintf "%d=%s" i (string_of_vtype s)) f.locals
  in
  let stack = string_of_stack f.stack in
  Printf.sprintf "{locals=[%s]; stack=%s}" (String.concat ", " locals_s) stack

type merged_code = Instruction of Instr.instruction | StackMap of jstack_map

type jenvironment = {
  cls : jclass;
  mth : jmethod;
  return : vtype;
  instructions : merged_code list;
  max_stack : int;
  exception_handlers : exception_handler list;
}

type vclass = string * jloader
