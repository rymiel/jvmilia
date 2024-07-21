open Java
open Basic

let char_max = 65535l
let char_min = 0l
let short_max = 32767l
let short_min = -32768l
let clamp min max v = if v < min then min else if v > max then max else v

let find_methodj (cls : jclass) (name : string) (desc : string) : jmethod option
    =
  let matches (m : jmethod) = m.desc = desc && m.name = name in
  List.find_opt matches cls.methods

(* todo: collapse with method above *)
let find_method (cls : eclass) (name : string) (desc : string) : jmethod option
    =
  find_methodj cls.raw name desc

let instance_fields (cls : jclass) : jfield list =
  List.filter (fun (m : jfield) -> not m.access_flags.is_static) cls.fields

let static_fields (cls : jclass) : jfield list =
  List.filter (fun (m : jfield) -> m.access_flags.is_static) cls.fields

let as_int (v : evalue) : int32 =
  match v with
  | Int v -> v
  | x -> failwith (Printf.sprintf "Not an int %s" (string_of_evalue x))

let as_float (v : evalue) : float =
  match v with
  | Float v -> v
  | x -> failwith (Printf.sprintf "Not a float %s" (string_of_evalue x))

let object_type_name (v : evalue) : string =
  match v with Object o -> o.cls.raw.name | _ -> failwith "Not an object"

let object_instance_field (v : evalue) (name : string) : evalue =
  match v with
  | Object o -> StringMap.find name o.fields
  | _ -> failwith "Not an object"

let java_lang_Class_name (v : evalue) : string =
  assert (object_type_name v = "java/lang/Class");
  match object_instance_field v "__jvmilia_name" with
  | ByteArray a -> Bytes.to_string a
  | _ -> failwith "Nope"

(* todo: maybe descriptors shouldn't become vtype, but some smaller subset of vtype,
   that can then be converted to vtype in the verifier whenever needed? *)
let default_value (ty : Vtype.vtype) : evalue =
  Vtype.(
    match ty with
    | Top | OneWord | TwoWord | Void | Uninitialized | UninitializedThis
    | UninitializedOffset _ ->
        failwith "Not a valid type"
    | Int -> Int 0l
    | Float -> Float 0.0
    | Long -> Long 0L
    | Double -> failwith "unimplemented double"
    | Class (_, _) | Array _ | Reference | Null -> Null)

let default_arraytype_value (ty : Vtype.arraytype) : evalue =
  Vtype.(
    match ty with
    | T x -> default_value x
    | Byte | Char | Short | Boolean -> Int 0l)

let is_static (mth : jmethod) : bool =
  mth.access_flags.is_static && not mth.access_flags.is_abstract

let not_static (mth : jmethod) : bool = not mth.access_flags.is_static

let make_object_array (size : int) (name : string) (default_value : evalue) :
    evalue =
  let ty = Vtype.T (Vtype.Class (name, Loader.bootstrap_loader)) in
  let arr = Array.make size default_value in
  Array { ty; arr }

let set_object_array (array : Basic.evalue) (index : int) (value : Basic.evalue)
    : unit =
  (match array with
  | Array x -> x.arr.(index) <- value
  | _ -> failwith "Not an array");
  Printf.printf "%s\n%!" (string_of_evalue_detailed array)

let compare_cond (a : int32) (b : int32) cond =
  match cond with
  | Instr.Eq -> a = b
  | Instr.Ne -> a <> b
  | Instr.Lt -> a < b
  | Instr.Ge -> a >= b
  | Instr.Gt -> a > b
  | Instr.Le -> a <= b

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
              (self#load_class name).raw.name |> self#make_class_instance);
          get_static_method =
            (fun a b c ->
              let m = self#find_method a b c in
              assert (is_static m);
              m);
          class_name = java_lang_Class_name;
          make_string = self#make_string_instance;
          invoke_method = self#exec_with_return;
          get_virtual_method =
            (fun a b c ->
              let cls = self#load_class a in
              self#find_virtual_method cls.raw b c);
          make_object_array;
          set_object_array;
          object_type_name;
          object_instance_field;
          make_class_direct = self#make_class_instance;
        }
      in
      interface <- Shim.make_native_interface interface_data

    method free = Shim.free_native_interface interface
    method private curframe = List.hd frames

    method private push (value : evalue) =
      let frame = self#curframe in
      match value with
      | Long _ -> frame.stack <- value :: Void :: frame.stack
      | _ -> frame.stack <- value :: frame.stack

    method private pop () : evalue =
      let frame = self#curframe in
      let value = List.hd frame.stack in
      (match value with
      | Long _ -> (
          match List.tl frame.stack with
          | Void :: xs -> frame.stack <- xs
          | _ -> failwith "invalid stack state")
      | _ -> frame.stack <- List.tl frame.stack);
      value

    method private pop_list n : evalue list =
      List.init n (fun _ -> self#pop ()) |> List.rev

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

    (* note: doesn't need to be a method, but if we move the loader inside the class, then maybe*)
    method private load_class_definition (class_name : string) : jclass =
      Loader.load_class class_name Loader.bootstrap_loader

    method private perform_class_load (class_name : string) : eclass =
      let cls = self#load_class_definition class_name in
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
        |> StringMap.add "coder" (Int 0l)
      in
      Object { cls = self#load_class "java/lang/String"; fields }

    method private make_class_instance (class_name : string) : evalue =
      let fields =
        StringMap.empty
        |> StringMap.add "classLoader" Null
        |> StringMap.add "__jvmilia_name"
             (ByteArray (Bytes.of_string class_name))
      in
      Object { cls = self#load_class "java/lang/Class"; fields }

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

    method private find_method (cls : string) (name : string) (desc : string)
        : jmethod =
      let def_cls = self#load_class cls in
      let def_mth =
        match find_method def_cls name desc with
        | Some m -> m
        | None -> failwith "Cannot find static method (TODO add more info)"
      in
      def_mth

    (* note: only a method because load_class_definition is,
       but load_class_definition doesn't have to be *)
    method private find_virtual_method (cls : jclass) (name : string)
        (desc : string) : jmethod =
      match find_methodj cls name desc with
      | Some m ->
          assert (not_static m);
          m
      | None -> (
          match cls.superclass with
          | Some super ->
              let super_cls = self#load_class_definition super in
              self#find_virtual_method super_cls name desc
          | None -> failwith "Virtual dispatch failed (TODO add more info)")

    method private exec_instr (_mth : jmethod) (_code : Attr.code_attribute)
        (instr : Instr.instrbody) : unit =
      Debug.frame self#curframe;
      Debug.frame_detailed self#curframe;
      Debug.instr instr self#curframe.pc;
      match instr with
      | Invokestatic desc ->
          let def_mth = self#find_method desc.cls desc.name desc.desc in
          assert (is_static def_mth);
          (* todo frame stuff *)
          let args = self#pop_list def_mth.nargs in
          Printf.printf ">>>>>>>>>>>>>>>>> [%s]\n"
            (String.concat ", " (List.map string_of_evalue args));
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
          let args = self#pop_list def_mth.nargs in
          Printf.printf ">>>>>>>>>>>>>>>>> [%s]\n"
            (String.concat ", " (List.map string_of_evalue args));
          let objectref = self#pop () in
          (* todo remove this constraint *)
          assert (
            match objectref with
            | Object x -> self#is_indirect_superclass x.cls method_desc.cls
            | _ -> false);
          (* todo frame stuff *)
          self#exec def_mth (objectref :: args)
      | Invokevirtual desc ->
          let base_mth = self#find_method desc.cls desc.name desc.desc in
          assert (not_static base_mth);
          let args = self#pop_list base_mth.nargs in
          Printf.printf ">>>>>>>>>>>>>>>>> [%s]\n"
            (String.concat ", " (List.map string_of_evalue args));
          let objectref = self#pop () in
          (* todo remove this constraint *)
          let clsref =
            match objectref with
            | Object x ->
                assert (self#is_indirect_superclass x.cls desc.cls);
                x.cls.raw
            | _ -> failwith "not an object"
          in
          let resolved_mth =
            self#find_virtual_method clsref desc.name desc.desc
          in
          (* todo frame stuff *)
          self#exec resolved_mth (objectref :: args)
      | Getstatic field_desc ->
          let def_cls = self#load_class field_desc.cls in
          StringMap.find field_desc.name def_cls.static |> self#push
      | Getfield field_desc -> (
          match self#pop () with
          | Object x -> StringMap.find field_desc.name x.fields |> self#push
          | _ -> failwith "Can't get field of non-object type")
      | Putstatic field_desc ->
          let def_cls = self#load_class field_desc.cls in
          let value = self#pop () in
          def_cls.static <- StringMap.add field_desc.name value def_cls.static;
          Printf.printf "putstatic %s %s %s %s\n" def_cls.raw.name
            field_desc.name field_desc.desc (string_of_evalue value)
      | Putfield field_desc -> (
          let value = self#pop () in
          match self#pop () with
          | Object x ->
              x.fields <- StringMap.add field_desc.name value x.fields;
              Printf.printf "putfield %s %s %s %s\n"
                (string_of_evalue (Object x))
                field_desc.name field_desc.desc (string_of_evalue value)
          | _ -> failwith "Can't put field of non-object type")
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
          self#push (Object { cls = def_cls; fields = StringMap.empty })
      | Dup ->
          (* todo: longs/doubles stuff*)
          let v = self#pop () in
          self#push v;
          self#push v
      | Aload i | Iload i | Lload i | Fload i -> self#load i |> self#push
      | Astore i | Istore i | Lstore i -> self#pop () |> self#store i
      | Iinc (i, v) ->
          self#load i |> as_int
          |> Int32.add (Int32.of_int v)
          |> (fun x -> Int x)
          |> self#store i
      | Iarith op ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          Int
            (match op with
            | Div -> Int32.div a b
            | Add -> Int32.add a b
            | Sub -> Int32.sub a b
            | _ ->
                failwith
                  (Printf.sprintf "iarith unimplemented %s"
                     (Instr.string_of_arith_op op)))
          |> self#push
      | Iushr ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          let s = Int32.logand b 0b11111l |> Int32.to_int in
          Int (Int32.shift_right_logical a s) |> self#push
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
            | Instr.Eq -> v = 0l
            | Instr.Ne -> v <> 0l
            | Instr.Lt -> v < 0l
            | Instr.Ge -> v >= 0l
            | Instr.Gt -> v > 0l
            | Instr.Le -> v <= 0l
          then self#curframe.nextpc <- target
      | If_icmp (cond, target) ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          if
            match cond with
            | Instr.Eq -> a = b
            | Instr.Ne -> a <> b
            | Instr.Lt -> a < b
            | Instr.Ge -> a >= b
            | Instr.Gt -> a > b
            | Instr.Le -> a <= b
          then self#curframe.nextpc <- target
      | If_acmpeq target ->
          let b = self#pop () in
          let a = self#pop () in
          if a == b then self#curframe.nextpc <- target
      | If_acmpne target ->
          let b = self#pop () in
          let a = self#pop () in
          if a != b then self#curframe.nextpc <- target
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
          | Shared.Float f -> Float f |> self#push
          | Shared.Integer i -> Int i |> self#push)
      | Ldc2_w x -> (
          match x with
          | Shared.Long l -> self#push (Long l)
          | _ -> assert false (* todo *))
      | Iconst v | Bipush v | Sipush v -> self#push (Int v)
      | Fconst f -> self#push (Float f)
      | Fcmpg ->
          let b = self#pop () |> as_float in
          let a = self#pop () |> as_float in
          if Float.is_nan a || Float.is_nan b then self#push (Int 1l)
          else Int (compare a b |> Int32.of_int) |> self#push
          (* deduplicate? *)
      | Fcmpl ->
          let b = self#pop () |> as_float in
          let a = self#pop () |> as_float in
          if Float.is_nan a || Float.is_nan b then self#push (Int (-1l))
          else Int (compare a b |> Int32.of_int) |> self#push
      | Anewarray c ->
          let size = self#pop () |> as_int |> Int32.to_int in
          make_object_array size c.name Null |> self#push
      | Newarray ty ->
          assert (ty != Byte);
          let size = self#pop () |> as_int |> Int32.to_int in
          let arr = default_arraytype_value ty |> Array.make size in
          self#push (Array { ty; arr })
      | Aastore ->
          let v = self#pop () in
          let i = self#pop () |> as_int |> Int32.to_int in
          let a =
            match self#pop () with Array x -> x | _ -> failwith "Not an array"
          in
          a.arr.(i) <- v
      | Castore ->
          let v = self#pop () |> as_int |> clamp char_min char_max in
          let i = self#pop () |> as_int |> Int32.to_int in
          let a =
            match self#pop () with Array x -> x | _ -> failwith "Not an array"
          in
          a.arr.(i) <- Int v
      | Aaload ->
          let i = self#pop () |> as_int |> Int32.to_int in
          (match self#pop () with
          | Array x -> x.arr.(i)
          | _ -> failwith "Not an array")
          |> self#push
      | Baload ->
          let i = self#pop () |> as_int |> Int32.to_int in
          (match self#pop () with
          | ByteArray x -> Int (Char.code (Bytes.get x i) |> Int32.of_int)
          | _ -> failwith "Not an array")
          |> self#push
      | Arraylength ->
          self#push
            (Int
               ((match self#pop () with
                | Array x -> Array.length x.arr
                | ByteArray x -> Bytes.length x
                | _ -> failwith "Not an array")
               |> Int32.of_int))
      | Instanceof desc -> (
          match self#pop () with
          | Object o ->
              self#push (Int (if o.cls.raw.name = desc.name then 1l else 0l))
          | _ -> failwith "not an object")
      | Checkcast desc ->
          let v = self#pop () in
          (match v with
          | Object o -> assert (self#is_indirect_superclass o.cls desc.name)
          | _ -> failwith "not an object");
          self#push v
      (* TODO monitor stuff? *)
      | Monitorenter | Monitorexit ->
          let _ignore = self#pop () in
          ()
      | x ->
          failwith
            (Printf.sprintf "Unimplemented instruction excecution %s"
               (Instr.string_of_instr x))

    method private initialize_locals (args : evalue list) =
      let i = ref 0 in
      List.iter
        (fun v ->
          self#curframe.locals.(!i) <- v;
          i := !i + match v with Long _ -> 2 | _ -> 1)
        args

    method private exec_code (mth : jmethod) (code : Attr.code_attribute)
        (args : evalue list) : evalue =
      Debug.push "jvm_exec_code"
        (Printf.sprintf "%s.%s %s" mth.cls mth.name mth.desc);
      self#add_frame code.frame_size;
      (* todo longs and stuff *)
      Debug.frame self#curframe;
      self#initialize_locals args;
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
      Debug.pop ();
      self#pop_frame ()

    method private exec_with_return (mth : jmethod) (args : evalue list)
        : evalue =
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
          (String.concat ", " (List.map string_of_evalue_detailed args_real));
        let result =
          Shim.execute_native_auto interface args_real param_types ret_type
            method_handle
        in
        Printf.printf "Return value: %s\n%!" (string_of_evalue result);
        result)
      else if mth.access_flags.is_abstract then
        failwith
          (Printf.sprintf "Can't execute abstract method %s %s %s" mth.cls
             mth.name mth.desc)
      else
        match List.find_map find_code mth.attributes with
        | Some code_attr -> self#exec_code mth code_attr args
        | None ->
            failwith
              (Printf.sprintf "Can't execute non-code method %s %s %s" mth.cls
                 mth.name mth.desc)

    method private exec (mth : jmethod) (args : evalue list) : unit =
      match self#exec_with_return mth args with
      | Void -> ()
      | result -> self#push result

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
