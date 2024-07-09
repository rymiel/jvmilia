val push : string -> string -> unit
val pop : unit -> unit
val instr : Instr.instrbody -> int -> unit
val frame : Basic.exec_frame -> unit
val frame_detailed : Basic.exec_frame -> unit
