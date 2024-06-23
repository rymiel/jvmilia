val class_diagnostic : Java.jclass -> string
val method_diagnostic : Java.jmethod -> Java.jclass -> string
val env_diagnostic : Basic.jenvironment -> string
val push : string -> string -> unit
val push_rec : string -> string -> unit
val pop : bool -> bool
val instr : Instr.instrbody -> int -> unit
val frame : Basic.frame -> unit
val after_goto : unit -> unit
val concise : bool
