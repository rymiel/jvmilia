open Java
open Basic

let find_method (cls : eclass) (name : string) (desc : string) : jmethod option
    =
  let matches (m : jmethod) = m.desc = desc && m.name = name in
  List.find_opt matches cls.raw.methods

let instance_fields (cls : jclass) : jfield list =
  List.filter (fun (m : jfield) -> not m.access_flags.is_static) cls.fields

let static_fields (cls : jclass) : jfield list =
  List.filter (fun (m : jfield) -> m.access_flags.is_static) cls.fields

let as_int (v : evalue) : int =
  match v with
  | Int v -> v
  | x -> failwith (Printf.sprintf "Not an int %s" (string_of_evalue x))

(* todo: maybe descriptors shouldn't become vtype, but some smaller subset of vtype,
   that can then be converted to vtype in the verifier whenever needed? *)
let default_value (ty : Vtype.vtype) : evalue =
  Vtype.(
    match ty with
    | Top | OneWord | TwoWord | Void | Uninitialized | UninitializedThis
    | UninitializedOffset _ ->
        failwith "Not a valid type"
    | Int -> Int 0
    | Float -> failwith "unimplemented float"
    | Long -> Long 0L
    | Double -> failwith "unimplemented double"
    | Class (_, _) | Array _ | Reference | Null -> Null)

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
    val mutable string_pool : evalue StringMap.t = StringMap.empty
    val libjava : int = libjava
    val mutable interface : int = 0

    method init : unit =
      let interface_data =
        {
          Shim.find_class =
            (fun name ->
              self#make_class_instance (self#load_class name).raw.name);
          get_static_method = self#find_static_method;
        }
      in
      interface <- Shim.make_native_interface interface_data

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
      let fields =
        static_fields cls
        |> List.fold_left
             (fun m (f : jfield) ->
               StringMap.add f.name
                 (default_value (Vtype.parse_field_descriptor f.desc))
                 m)
             StringMap.empty
      in
      Printf.printf "%s %s\n" class_name
        (String.concat ", "
           (List.map
              (fun (k, v) -> Printf.sprintf "%s %s" k (string_of_evalue v))
              (StringMap.to_list fields)));
      let ecls = { raw = cls; static = fields } in
      self#mark_loaded ecls;
      (* mark as loaded before clinit, otherwise we recurse *)
      (match find_method ecls "<clinit>" "()V" with
      | Some clinit ->
          self#exec clinit [];
          ()
      | None -> ());
      (* openjdk is special *)
      (if cls.name = "java/lang/System" then
         match find_method ecls "initPhase1" "()V" with
         | Some init ->
             self#exec init [];
             ()
         | None -> ());
      (* todo other verification/linking stuff idk *)
      ecls

    (* todo: interning? *)
    method private make_string_instance (str : string) : evalue =
      let fields =
        StringMap.empty
        |> StringMap.add "value" (ByteArray (Bytes.of_string str))
      in
      Class { cls = self#load_class "java/lang/String"; fields }

    method private make_class_instance (class_name : string) : evalue =
      let fields =
        StringMap.empty
        |> StringMap.add "classLoader" Null
        |> StringMap.add "__jvmilia_name" (self#make_string_instance class_name)
      in
      Class { cls = self#load_class "java/lang/Class"; fields }

    (* bad method*)
    (* todo: remove*)
    method private is_indirect_superclass (cls : eclass) (target : string)
        : bool =
      if cls.raw.name = target then true
      else
        match cls.raw.superclass with
        | Some super ->
            self#is_indirect_superclass (self#load_class super) target
        | None -> false

    method private find_static_method (cls : string) (name : string)
        (desc : string) : jmethod =
      let def_cls = self#load_class cls in
      let def_mth =
        match find_method def_cls name desc with
        | Some m -> m
        | None ->
            failwith "Cannot find method to invokestatic (TODO add more info)"
      in
      assert (
        def_mth.access_flags.is_static && not def_mth.access_flags.is_abstract);
      def_mth

    method private exec_instr (_mth : jmethod) (_code : Attr.code_attribute)
        (instr : Instr.instrbody) : unit =
      Debug.frame self#curframe;
      Debug.frame_detailed self#curframe;
      Debug.instr instr self#curframe.pc;
      match instr with
      | Invokestatic method_desc ->
          let def_mth =
            self#find_static_method method_desc.cls method_desc.name
              method_desc.desc
          in
          (* todo frame stuff *)
          (* todo arguments *)
          let args = self#pop_list def_mth.nargs in
          Printf.printf ">>>>>>>>>>>>>>>>> [%s]\n"
            (String.concat ", " (List.map string_of_evalue args));
          assert (def_mth.nargs <= 1);
          self#exec def_mth args
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
          (* todo arguments *)
          let args = self#pop_list def_mth.nargs in
          Printf.printf ">>>>>>>>>>>>>>>>> [%s]\n"
            (String.concat ", " (List.map string_of_evalue args));
          assert (def_mth.nargs <= 1);
          let objectref = self#pop () in
          (* todo remove this constraint *)
          assert (
            match objectref with
            | Class x -> self#is_indirect_superclass x.cls method_desc.cls
            | _ -> false);
          (* todo frame stuff *)
          self#exec def_mth (objectref :: args)
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
          (* todo arguments *)
          assert (def_mth.nargs = 0);
          let objectref = self#pop () in
          (* todo remove this constraint *)
          assert (
            match objectref with
            | Class x -> x.cls.raw.name = method_desc.cls
            | _ -> false);
          (* todo frame stuff *)
          self#exec def_mth [ objectref ]
      | Getstatic field_desc ->
          let def_cls = self#load_class field_desc.cls in
          StringMap.find field_desc.name def_cls.static |> self#push
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
      | Ireturn | Areturn ->
          let value = self#pop () in
          self#curframe.retval <- Some value
      | New class_desc ->
          let def_cls = self#load_class class_desc.name in
          let fields = instance_fields def_cls.raw in
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
      | Ifnull target -> (
          match self#pop () with
          | Null -> self#curframe.nextpc <- target
          | _ -> ())
      | If (cond, target) ->
          let v = self#pop () |> as_int in
          if
            match cond with
            | Instr.Eq -> v = 0
            | Instr.Ne -> v <> 0
            | Instr.Lt -> v < 0
            | Instr.Ge -> v >= 0
            | Instr.Gt -> v > 0
            | Instr.Le -> v <= 0
          then self#curframe.nextpc <- target
      | If_icmp (cond, target) ->
          let a = self#pop () |> as_int in
          let b = self#pop () |> as_int in
          if
            match cond with
            | Instr.Eq -> a = b
            | Instr.Ne -> a <> b
            | Instr.Lt -> a < b
            | Instr.Ge -> a >= b
            | Instr.Gt -> a > b
            | Instr.Le -> a <= b
          then self#curframe.nextpc <- target
      | Goto target -> self#curframe.nextpc <- target
      | Ldc x -> (
          match x with
          | Shared.Class c -> self#make_class_instance c |> self#push
          | Shared.String s -> (
              match StringMap.find_opt s string_pool with
              | Some existing -> self#push existing
              | None ->
                  let v = self#make_string_instance s in
                  string_pool <- StringMap.add s v string_pool;
                  self#push v)
          | _ -> assert false (* todo *))
      | Iconst v | Bipush v -> self#push (Int v)
      | Anewarray c ->
          let ty = Vtype.T (Vtype.Class (c.name, Loader.bootstrap_loader)) in
          let size = self#pop () |> as_int in
          let arr = Array.make size Null in
          self#push (Array { ty; arr })
      | Aastore ->
          let v = self#pop () in
          let i = self#pop () |> as_int in
          let a =
            match self#pop () with Array x -> x | _ -> failwith "Not an array"
          in
          a.arr.(i) <- v
      | x ->
          failwith
            (Printf.sprintf "Unimplemented instruction excecution %s"
               (Instr.string_of_instr x))

    method private exec_code (mth : jmethod) (code : Attr.code_attribute)
        (args : evalue list) : unit =
      Debug.push "jvm_exec_code"
        (Printf.sprintf "%s.%s %s" mth.cls mth.name mth.desc);
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
        self#exec_instr mth code m.body;
        frame.pc <- frame.nextpc
      done;
      (match self#pop_frame () with Void -> () | x -> self#push x);
      Debug.pop ()

    method private exec (mth : jmethod) (args : evalue list) : unit =
      let find_code (attr : Attr.attribute) : Attr.code_attribute option =
        match attr with Code x -> Some x | _ -> None
      in
      if mth.access_flags.is_native then (
        let registered =
          Shim.get_registered_fnptr interface mth.cls mth.name mth.desc
        in
        let method_handle =
          match registered with
          | Some fnptr -> fnptr
          | None ->
              let as_underscore c = if c = '/' then '_' else c in
              let native_name =
                "Java_" ^ String.map as_underscore mth.cls ^ "_" ^ mth.name
              in
              let filtered_name =
                Str.global_replace (Str.regexp "\\$") "_00024" native_name
              in
              Printf.printf "Native method %s.%s -> %s\n" mth.cls mth.name
                filtered_name;
              Shim.load_method libjava filtered_name
        in
        Printf.printf "%s %s %s -> %#x\n%!" mth.cls mth.name mth.desc
          method_handle;
        if method_handle = 0 then failwith "Method handle is null";
        let param_types, ret_type = Vtype.parse_method_descriptor mth.desc in
        let args_real =
          if mth.access_flags.is_static then
            let receiver = self#make_class_instance mth.cls in
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
        | Some code_attr -> self#exec_code mth code_attr args
        | None -> failwith "Can't execute non-code method"

    method exec_main (main_class_name : string) : unit =
      let main_class = self#load_class main_class_name in
      match find_method main_class "main" "([Ljava/lang/String;)V" with
      | Some main_method ->
          let flags = main_method.access_flags in
          if not flags.is_static then failwith "Main method is not static";
          if not flags.is_public then failwith "Main method is not public";
          (* todo: the String[] argument *)
          self#exec main_method []
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
