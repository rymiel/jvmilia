open Java
module StringMap = Map.Make (String)

type evalue = Void | Null
type exec_frame = { locals : evalue array; mutable stack : evalue list }
type eclass = { raw : jclass; mutable static : evalue StringMap.t }
type instrlink = { instr : Instr.instruction; next : instrlink option }

let string_of_evalue (value : evalue) : string =
  match value with Void -> "void" | Null -> "null"

let find_method (cls : eclass) (name : string) (desc : string) : jmethod option
    =
  let matches m = m.desc = desc && m.name = name in
  List.find_opt matches cls.raw.methods

class jvm libjava interface =
  object (self)
    val mutable frames : exec_frame list = []
    val mutable loaded : eclass StringMap.t = StringMap.empty
    val libjava : int64 = libjava
    val interface : int64 = interface
    method free = Shim.free_native_interface interface
    method private curframe = List.hd frames

    method private push (value : evalue) =
      let frame = self#curframe in
      frame.stack <- value :: frame.stack

    method private pop () : evalue =
      let frame = self#curframe in
      let value = List.hd frame.stack in
      frame.stack <- List.tl frame.stack;
      value

    method private add_frame (frame_size : int) =
      let locals = Array.make frame_size Void in
      let stack = [] in
      let new_frame = { locals; stack } in
      frames <- new_frame :: frames

    method private mark_loaded (cls : eclass) =
      loaded <- StringMap.add cls.raw.name cls loaded

    method private find_loaded (name : string) : eclass option =
      StringMap.find_opt name loaded

    method private load_class (class_name : string) : eclass =
      match self#find_loaded class_name with
      | Some existing -> existing
      | None -> self#perform_class_load class_name

    method private perform_class_load (class_name : string) : eclass =
      let cls = Loader.load_class class_name Loader.bootstrap_loader in
      let safe = Verify.Main.classIsTypeSafe cls in
      if not safe then
        failwith (Printf.sprintf "Class %S failed verification" class_name);
      let ecls = { raw = cls; static = StringMap.empty } in
      self#mark_loaded ecls;
      (* mark as loaded before clinit, otherwise we recurse *)
      (match find_method ecls "<clinit>" "()V" with
      | Some clinit ->
          self#exec ecls clinit;
          ()
      | None -> ());
      (* openjdk is special *)
      (if cls.name = "java/lang/System" then
         match find_method ecls "initPhase1" "()V" with
         | Some init ->
             self#exec ecls init;
             ()
         | None -> ());
      (* todo other verification/linking stuff idk *)
      ecls

    method private exec_instr (_cls : eclass) (_mth : jmethod)
        (_code : Attr.code_attribute) (instr : Instr.instruction) : unit =
      let _i, body = instr in
      match body with
      | Invokestatic method_desc ->
          let def_cls = self#load_class method_desc.cls in
          let def_mth =
            match find_method def_cls method_desc.name method_desc.desc with
            | Some m -> m
            | None ->
                failwith
                  "Cannot find method to invokestatic (TODO add more info)"
          in
          assert (
            def_mth.access_flags.is_static
            && not def_mth.access_flags.is_abstract);
          (* todo frame stuff *)
          self#exec def_cls def_mth
      | Getstatic field_desc ->
          let def_cls = self#load_class field_desc.cls in
          failwith (Printf.sprintf "getstatic %s" def_cls.raw.name)
      | Putstatic field_desc ->
          let def_cls = self#load_class field_desc.cls in
          let value = self#pop () in
          def_cls.static <- StringMap.add field_desc.name value def_cls.static;
          Printf.printf "putstatic %s %s %s %s\n" def_cls.raw.name
            field_desc.name field_desc.desc (string_of_evalue value)
      | Aconst_null -> self#push Null
      | x ->
          failwith
            (Printf.sprintf "Unimplemented instruction excecution %s"
               (Instr.string_of_instr x))

    method private exec_code (cls : eclass) (mth : jmethod)
        (code : Attr.code_attribute) : unit =
      Debug.push "jvm_exec_code"
        (Printf.sprintf "%s.%s %s" cls.raw.name mth.name mth.desc);
      self#add_frame code.frame_size;
      List.iter (self#exec_instr cls mth code) code.code;
      Debug.pop ()

    method private exec (cls : eclass) (mth : jmethod) : unit =
      let find_code (attr : Attr.attribute) : Attr.code_attribute option =
        match attr with Code x -> Some x | _ -> None
      in
      if mth.access_flags.is_native then (
        let as_underscore c = if c = '/' then '_' else c in
        let native_name =
          "Java_" ^ String.map as_underscore cls.raw.name ^ "_" ^ mth.name
        in
        Printf.printf "Native method %s.%s -> %s\n" cls.raw.name mth.name
          native_name;
        let method_handle = Shim.load_method libjava native_name in
        Printf.printf "%s -> %#Lx\n%!" native_name method_handle;
        Shim.execute_native_noargs_void interface cls.raw.name method_handle)
      else
        match List.find_map find_code mth.attributes with
        | Some code_attr -> self#exec_code cls mth code_attr
        | None -> failwith "Can't execute non-code method"

    method exec_main (main_class_name : string) : unit =
      let main_class = self#load_class main_class_name in
      match find_method main_class "main" "([Ljava/lang/String;)V" with
      | Some main_method ->
          let flags = main_method.access_flags in
          if not flags.is_static then failwith "Main method is not static";
          if not flags.is_public then failwith "Main method is not public";
          self#exec main_class main_method
      | None -> failwith "This class does not have a main method"
  end

let create_jvm (loader : string -> jclass) : jvm =
  (* TODO: maybe this should be stored in jvm *)
  Loader.initialize_bootstrap_loader loader;
  let libjvm = Shim.load_library "/usr/lib/jvm/default/lib/server/libjvm.so" in
  let libjava = Shim.load_library "./class/extern-lib/libjava.so" in
  let interface = Shim.make_native_interface () in
  Printf.printf "libjvm: %#Lx, libjava: %#Lx\n" libjvm libjava;
  new jvm libjava interface
