open Java
open Basic

let find_method (cls : eclass) (name : string) (desc : string) : jmethod option
    =
  let matches (m : jmethod) = m.desc = desc && m.name = name in
  List.find_opt matches cls.raw.methods

let rec split (list : 'a list) (n : int) : 'a list * 'a list =
  if n = 0 then ([], list)
  else
    match list with
    | [] -> failwith "Ran out of list to split"
    | x :: xs ->
        let a, b = split xs (n - 1) in
        (x :: a, b)

class jvm libjava =
  object (self)
    val mutable frames : exec_frame list = []
    val mutable loaded : eclass StringMap.t = StringMap.empty
    val libjava : int = libjava
    val mutable interface : int = 0
    val mutable interface_data : Shim.native_interface option = None

    method init : unit =
      interface_data <- Some { Shim.find_class = self#load_class; dummy = 0 };
      interface <- Shim.make_native_interface (Option.get interface_data);
      Printf.printf "interface: %#x\nd: %#x\nfind_calss: %#x\n" interface
        (Obj.magic (Option.get interface_data))
        (Obj.magic (Option.get interface_data).find_class)

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

    method private pop_list n : evalue list =
      let frame = self#curframe in
      let popped, remaining = split frame.stack n in
      frame.stack <- remaining;
      popped

    method private load n : evalue = self#curframe.locals.(n)
    method private store n v : unit = self#curframe.locals.(n) <- v

    method private add_frame (frame_size : int) =
      let locals = Array.make frame_size Void in
      let stack = [] in
      let new_frame = { locals; stack; pc = 0; nextpc = 0; retval = None } in
      frames <- new_frame :: frames

    (* i might regret this returning just evalue instead of evalue option *)
    method private pop_frame () : evalue =
      let frame = self#curframe in
      frames <- List.tl frames;
      match frame.retval with
      | Some v -> v
      | None -> failwith "Frame returned nothing"

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
          self#exec ecls clinit [];
          ()
      | None -> ());
      (* openjdk is special *)
      (if cls.name = "java/lang/System" then
         match find_method ecls "initPhase1" "()V" with
         | Some init ->
             self#exec ecls init [];
             ()
         | None -> ());
      (* todo other verification/linking stuff idk *)
      ecls

    method private exec_instr (_cls : eclass) (_mth : jmethod)
        (_code : Attr.code_attribute) (instr : Instr.instrbody) : unit =
      Debug.frame self#curframe;
      Debug.instr instr self#curframe.pc;
      match instr with
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
          (* todo arguments *)
          let args = self#pop_list def_mth.nargs in
          Printf.printf ">>>>>>>>>>>>>>>>> [%s]\n"
            (String.concat ", " (List.map string_of_evalue args));
          self#exec def_cls def_mth args
      | Invokespecial method_desc ->
          let def_cls = self#load_class method_desc.cls in
          (* todo proper method resolution *)
          (* todo find actual method, looking up superclasses if necessary *)
          let def_mth =
            match find_method def_cls method_desc.name method_desc.desc with
            | Some m -> m
            | None ->
                failwith
                  "Cannot find method to invokespecial (TODO add more info)"
          in
          let objectref = self#pop () in
          (* todo remove this constraint *)
          assert (
            match objectref with
            | Class x ->
                x.cls.raw.name = method_desc.cls
                || method_desc.cls = "java/lang/Object"
            | _ -> false);
          (* todo frame stuff *)
          (* todo arguments *)
          assert (def_mth.nargs = 0);
          self#exec def_cls def_mth [ objectref ]
      | Invokevirtual method_desc ->
          let def_cls = self#load_class method_desc.cls in
          (* todo proper method resolution *)
          (* todo find actual method, looking up superclasses if necessary *)
          let def_mth =
            match find_method def_cls method_desc.name method_desc.desc with
            | Some m -> m
            | None ->
                failwith
                  "Cannot find method to invokevirtual (TODO add more info)"
          in
          let objectref = self#pop () in
          (* todo remove this constraint *)
          assert (
            match objectref with
            | Class x -> x.cls.raw.name = method_desc.cls
            | _ -> false);
          (* todo frame stuff *)
          (* todo arguments *)
          assert (def_mth.nargs = 0);
          self#exec def_cls def_mth [ objectref ]
      | Getstatic field_desc ->
          let def_cls = self#load_class field_desc.cls in
          failwith (Printf.sprintf "getstatic %s" def_cls.raw.name)
      | Getfield field_desc -> (
          match self#pop () with
          | Class x -> StringMap.find field_desc.name x.fields |> self#push
          | _ -> failwith "Can't get field of non-object type")
      | Putstatic field_desc ->
          let def_cls = self#load_class field_desc.cls in
          let value = self#pop () in
          def_cls.static <- StringMap.add field_desc.name value def_cls.static;
          Printf.printf "putstatic %s %s %s %s\n" def_cls.raw.name
            field_desc.name field_desc.desc (string_of_evalue value)
      | Aconst_null -> self#push Null
      | Return -> self#curframe.retval <- Some Void
      | Ireturn ->
          let value = self#pop () in
          self#curframe.retval <- Some value
      | New class_desc ->
          let def_cls = self#load_class class_desc.name in
          let fields = def_cls.raw.fields in
          List.iteri
            (fun i (field : jfield) ->
              Printf.printf "%d %s %s\n" i field.name field.desc)
            fields;
          (* todo: declare fields *)
          self#push (Class { cls = def_cls; fields = StringMap.empty })
      | Dup ->
          (* todo: longs/doubles stuff*)
          let v = self#pop () in
          self#push v;
          self#push v
      | Aload i -> self#load i |> self#push
      | Astore i -> self#pop () |> self#store i
      | Ifnonnull target -> (
          match self#pop () with
          | Null -> ()
          | _ -> self#curframe.nextpc <- target)
      | If (cond, target) -> (
          match (cond, self#pop ()) with
          | Ne, Int 0 -> ()
          | Ne, _ -> self#curframe.nextpc <- target
          | _ -> failwith "unimplemented")
      | Goto target -> self#curframe.nextpc <- target
      | Ldc x ->
          (* todo *)
          assert (match x with Shared.Class _ -> true | _ -> false);
          let fields = StringMap.add "classLoader" Null StringMap.empty in
          self#push (Class { cls = self#load_class "java/lang/Class"; fields })
          (* todo extract *)
      | Iconst v -> self#push (Int v)
      | Anewarray c ->
          let ty = Vtype.T (Vtype.Class (c.name, Loader.bootstrap_loader)) in
          let size =
            match self#pop () with
            | Int v -> v
            | x ->
                failwith (Printf.sprintf "Not an int %s" (string_of_evalue x))
          in
          let arr = Array.make size Null in
          self#push (Array { ty; arr })
      | x ->
          failwith
            (Printf.sprintf "Unimplemented instruction excecution %s"
               (Instr.string_of_instr x))

    method private exec_code (cls : eclass) (mth : jmethod)
        (code : Attr.code_attribute) (args : evalue list) : unit =
      Debug.push "jvm_exec_code"
        (Printf.sprintf "%s.%s %s" cls.raw.name mth.name mth.desc);
      self#add_frame code.frame_size;
      (* todo longs and stuff *)
      Debug.frame self#curframe;
      List.iteri (fun i v -> self#curframe.locals.(i) <- v) args;
      let frame = self#curframe in
      let map = Instr.map_instrs code.code in
      (* Instr.IntMap.iter
         (fun k (v : Instr.mappedinstr) ->
           Printf.printf "%d = %s -> %d\n" k
             (Instr.string_of_instr v.body)
             v.next)
         map; *)
      while frame.pc <> -1 && Option.is_none frame.retval do
        let m = Instr.IntMap.find frame.pc map in
        frame.nextpc <- m.next;
        self#exec_instr cls mth code m.body;
        frame.pc <- frame.nextpc
      done;
      (match self#pop_frame () with Void -> () | x -> self#push x);
      Debug.pop ()

    method private exec (cls : eclass) (mth : jmethod) (args : evalue list)
        : unit =
      let find_code (attr : Attr.attribute) : Attr.code_attribute option =
        match attr with Code x -> Some x | _ -> None
      in
      if mth.access_flags.is_native then (
        let registered =
          Shim.get_registered_fnptr interface cls.raw.name mth.name mth.desc
        in
        let method_handle =
          match registered with
          | Some fnptr -> fnptr
          | None ->
              let as_underscore c = if c = '/' then '_' else c in
              let native_name =
                "Java_" ^ String.map as_underscore cls.raw.name ^ "_" ^ mth.name
              in
              let filtered_name =
                Str.global_replace (Str.regexp "\\$") "_00024" native_name
              in
              Printf.printf "Native method %s.%s -> %s\n" cls.raw.name mth.name
                filtered_name;
              Shim.load_method libjava filtered_name
        in
        Printf.printf "%s %s %s -> %#x\n%!" cls.raw.name mth.name mth.desc
          method_handle;
        if method_handle = 0 then failwith "Method handle is null";
        let param_types, ret_type = Vtype.parse_method_descriptor mth.desc in
        let args_real =
          if mth.access_flags.is_static then
            (* todo extract *)
            let receiver =
              Class
                {
                  cls = self#load_class "java/lang/Class";
                  fields = StringMap.empty;
                }
            in
            receiver :: args
          else args
        in
        Printf.printf "%#x ((%s) -> %s) [%s]\n%!" method_handle
          (String.concat ", " (List.map Vtype.string_of_vtype param_types))
          (Vtype.string_of_vtype ret_type)
          (String.concat ", " (List.map string_of_evalue args_real));
        let result =
          Shim.execute_native_auto interface args_real param_types ret_type
            method_handle
        in
        Printf.printf "Return value: %s\n%!" (string_of_evalue result);
        match ret_type with Void -> () | _ -> self#push result)
      else
        match List.find_map find_code mth.attributes with
        | Some code_attr -> self#exec_code cls mth code_attr args
        | None -> failwith "Can't execute non-code method"

    method exec_main (main_class_name : string) : unit =
      let main_class = self#load_class main_class_name in
      match find_method main_class "main" "([Ljava/lang/String;)V" with
      | Some main_method ->
          let flags = main_method.access_flags in
          if not flags.is_static then failwith "Main method is not static";
          if not flags.is_public then failwith "Main method is not public";
          (* todo: the String[] argument *)
          self#exec main_class main_method []
      | None -> failwith "This class does not have a main method"
  end

let create_jvm (loader : string -> jclass) : jvm =
  (* TODO: maybe this should be stored in jvm *)
  Loader.initialize_bootstrap_loader loader;
  let libjava = Shim.load_library "./class/extern-lib/libjava.so" in
  Printf.printf "libjava: %#x\n" libjava;
  let jvm = new jvm libjava in
  jvm#init;
  jvm
