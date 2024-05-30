open Shared
open Vtype

type frame_desc =
  | Same
  | SameLocals1StackItem of vtype
  | Append of vtype list
  | FullFrame of { locals : vtype list; stack : vtype list }

type delta_frame = int * frame_desc

type code_attribute = {
  frame_size : int;
  max_stack : int;
  handlers : exception_handler list;
  code : Instr.instruction list;
  attributes : attribute list;
}

and attribute =
  | Code of code_attribute
  | Unknown of string * bytes
  | StackMapTable of delta_frame list
