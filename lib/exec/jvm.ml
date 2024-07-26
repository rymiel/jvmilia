open Java
open Basic

let truncate (m : bytes -> int -> int) (v : int32) : int32 =
  let buf = Bytes.create 4 in
  Bytes.set_int32_le buf 0 v;
  m buf 0 |> Int32.of_int

let truncate_byte_range x = truncate Bytes.get_int8 x
let truncate_char_range x = truncate Bytes.get_uint16_le x
let truncate_short_range x = truncate Bytes.get_int16_le x

let find_method (cls : jclass) (name : string) (desc : string) : jmethod option
    =
  let matches (m : jmethod) = m.desc = desc && m.name = name in
  List.find_opt matches cls.methods

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

let as_long (v : evalue) : int64 =
  match v with
  | Long v -> v
  | x -> failwith (Printf.sprintf "Not a long %s" (string_of_evalue x))

let as_double (v : evalue) : float =
  match v with
  | Double v -> v
  | x -> failwith (Printf.sprintf "Not a double %s" (string_of_evalue x))

let reference_type_name (v : evalue) : string =
  match v with
  | Object o -> o.cls.raw.name
  | Array a -> Printf.sprintf "%s[]" (Type.string_of_dtype a.ty)
  | ByteArray _ -> "byte[]"
  | _ -> failwith "Not an object 1"

let object_instance_field (v : evalue) (name : string) : evalue ref =
  match v with
  | Object o -> StringMap.find name o.fields
  | _ -> failwith "Not an object 2"

let java_lang_Class_name (v : evalue) : string =
  assert (reference_type_name v = "java/lang/Class");
  match !(object_instance_field v "__jvmilia_name") with
  | ByteArray a -> Bytes.to_string a
  | _ -> failwith "Nope"

let default_value (ty : Type.dtype) : evalue =
  Type.(
    match ty with
    | Void -> failwith "Can't initialize void"
    | Int | Byte | Char | Short | Boolean -> Int 0l
    | Float -> Float 0.0
    | Long -> Long 0L
    | Double -> Double 0.0
    | Object _ | Array _ -> Null)

let fields_default_mapped =
  List.fold_left
    (fun m (f : jfield) ->
      StringMap.add f.name (default_value f.field_type |> ref) m)
    StringMap.empty

let is_static (mth : jmethod) : bool =
  mth.access_flags.is_static && not mth.access_flags.is_abstract

let not_static (mth : jmethod) : bool = not mth.access_flags.is_static

let make_object_array (size : int) (name : string) (default_value : evalue) :
    evalue =
  let ty = Type.Object name in
  let arr = Array.make size default_value in
  Array { ty; arr }

let set_object_array (array : Basic.evalue) (index : int) (value : Basic.evalue)
    : unit =
  (match array with
  | Array x -> x.arr.(index) <- value
  | _ -> failwith "Not an array");
  Printf.printf "%s\n%!" (string_of_evalue_detailed array)

(* TODO: kinda bad *)
let shared_counters =
  StringMap.empty |> StringMap.add "__jvmilia_next_tid" (ref (Long 2L))

(*TODO: remove*)
let get_field_by_hash (v : evalue) (hash : int) : evalue ref =
  let _, r =
    (match v with
    | Object o -> o.fields
    | Null -> shared_counters
    | _ -> failwith "Not an object 3")
    |> StringMap.bindings
    |> List.find (fun (n, _) -> String.hash n = hash)
  in
  r

let compare_cond (a : int32) (b : int32) cond =
  match cond with
  | Instr.Eq -> a = b
  | Instr.Ne -> a <> b
  | Instr.Lt -> a < b
  | Instr.Ge -> a >= b
  | Instr.Gt -> a > b
  | Instr.Le -> a <= b

let size v = match v with Long _ | Double _ -> 2 | _ -> 1
let pick_fst _ v1 _ = Some v1

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
          Native.find_class =
            (fun name ->
              (self#load_class name).raw.name |> self#make_class_instance);
          get_static_method = self#find_static_method;
          class_name = java_lang_Class_name;
          make_string = self#make_string_instance;
          invoke_method = self#exec_with_return;
          get_virtual_method =
            (fun a b c ->
              let cls = self#load_class a in
              self#find_virtual_method cls.raw.name b c);
          make_object_array;
          set_object_array;
          reference_type_name;
          object_instance_field;
          make_class_direct = self#make_class_instance;
          string_hash = (fun x -> Bytes.to_string x |> String.hash);
          get_field_by_hash;
          class_static_field = self#class_static_field;
          make_new = self#make_new;
          print_evalue_detailed =
            (fun x -> string_of_evalue_detailed x |> print_endline);
        }
      in
      interface <- Native.make_native_interface interface_data

    method free = Native.free_native_interface interface
    method private curframe = List.hd frames

    method private push (value : evalue) =
      let frame = self#curframe in
      match value with
      | Long _ | Double _ -> frame.stack <- value :: Void :: frame.stack
      | _ -> frame.stack <- value :: frame.stack

    method private pop () : evalue =
      let frame = self#curframe in
      let value = List.hd frame.stack in
      (match value with
      | Long _ | Double _ -> (
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
      if class_name.[0] = '[' then (
        (* TODO: component type stuff *)
        assert (class_name.[1] = 'B');
        {
          name = class_name;
          superclass = Some "java/lang/Object";
          superinterfaces = [ "java/lang/Cloneable"; "java/io/Serializable" ];
          loader = Loader.bootstrap_loader;
          methods = [];
          fields = [];
          access_flags =
            {
              is_public = true;
              is_final = true;
              is_super = true;
              is_interface = false;
              is_abstract = true;
              is_synthetic = false;
              is_annotation = false;
              is_enum = false;
              is_module = false;
            };
        })
      else Loader.load_class class_name Loader.bootstrap_loader

    method private perform_class_load (class_name : string) : eclass =
      let cls = self#load_class_definition class_name in
      let safe = Verify.Main.classIsTypeSafe cls in
      if not safe then
        failwith (Printf.sprintf "Class %S failed verification" class_name);
      (* Make sure parent is class-loaded and take the static fields *)
      let parent_static_fields =
        match cls.superclass with
        | Some superclass_name -> (self#load_class superclass_name).static
        | None -> StringMap.empty
      in
      let this_static_fields = static_fields cls |> fields_default_mapped in
      let merged_static_fields =
        StringMap.union pick_fst this_static_fields parent_static_fields
      in
      Printf.printf "[static fields] %s: %s\n" class_name
        (String.concat ", "
           (List.map
              (fun (k, v) -> Printf.sprintf "%s %s" k (string_of_evalue !v))
              (StringMap.to_list merged_static_fields)));
      let ecls = { raw = cls; static = merged_static_fields } in
      self#mark_loaded ecls;
      (* mark as loaded before clinit, otherwise we recurse *)
      (match find_method cls "<clinit>" "()V" with
      | Some clinit ->
          self#exec clinit [];
          ()
      | None -> ());
      (* todo other verification/linking stuff idk *)
      ecls

    (* todo: interning? *)
    method private make_string_instance (str : string) : evalue =
      (* todo: use actual field initialization, to make sure we don't miss any *)
      let fields =
        StringMap.empty
        |> StringMap.add "value" (ref (ByteArray (Bytes.of_string str)))
        |> StringMap.add "coder" (ref (Int 0l))
        |> StringMap.add "hash" (ref (Int 0l))
        |> StringMap.add "hashIsZero" (ref (Int 0l))
      in
      Object { cls = self#load_class "java/lang/String"; fields }

    method private make_class_instance (class_name : string) : evalue =
      (* todo: use actual field initialization, to make sure we don't miss any *)
      let fields =
        StringMap.empty
        |> StringMap.add "classLoader" (ref Null)
        |> StringMap.add "__jvmilia_name"
             (ref (ByteArray (Bytes.of_string class_name)))
      in
      Object { cls = self#load_class "java/lang/Class"; fields }

    method private class_static_field (v : evalue) (name : string) : evalue ref
        =
      let cls_name = java_lang_Class_name v in
      let cls = self#load_class cls_name in
      StringMap.find name cls.static

    (* bad method*)
    (* todo: remove*)
    method private is_indirect_superclass (cls : eclass) (target : string)
        : bool =
      if cls.raw.name = target then true
      else
        (match cls.raw.superclass with
        | Some super ->
            self#is_indirect_superclass (self#load_class super) target
        | None -> false)
        || cls.raw.superinterfaces
           |> List.exists (fun super ->
                  self#is_indirect_superclass (self#load_class super) target)

    method private find_static_method (cls : string) (name : string)
        (desc : string) : jmethod =
      let def_cls = self#load_class cls in
      let def_mth =
        match find_method def_cls.raw name desc with
        | Some m ->
            assert (is_static m);
            m
        | None -> failwith "Cannot find method (TODO add more info)"
      in
      def_mth

    (* note: only a method because load_class_definition is,
       but load_class_definition doesn't have to be *)
    method private find_virtual_method (cls : string) (name : string)
        (desc : string) : jmethod =
      let def_cls = self#load_class_definition cls in
      match find_method def_cls name desc with
      | Some m ->
          assert (not_static m);
          m
      | None -> (
          match def_cls.superclass with
          | Some super -> self#find_virtual_method super name desc
          | None -> failwith "Virtual dispatch failed (TODO add more info)")

    (* note: only a method because load_class_definition is,
       but load_class_definition doesn't have to be *)
    method private instance_fields (cls : jclass) : jfield list =
      List.filter (fun (m : jfield) -> not m.access_flags.is_static) cls.fields
      @
      match cls.superclass with
      | Some super -> self#instance_fields (self#load_class_definition super)
      | None -> []

    method private make_new (name : string) : evalue =
      let cls = self#load_class name in
      let fields = self#instance_fields cls.raw |> fields_default_mapped in
      Printf.printf "%s %s\n" name
        (String.concat ", "
           (List.map
              (fun (k, v) -> Printf.sprintf "%s %s" k (string_of_evalue !v))
              (StringMap.to_list fields)));
      Object { cls; fields }

    method private exec_instr (_mth : jmethod) (_code : Attr.code_attribute)
        (instr : Instr.instrbody) : unit =
      Debug.frame self#curframe;
      Debug.frame_detailed self#curframe;
      Debug.instr instr self#curframe.pc;
      match instr with
      | Invokestatic desc ->
          let def_mth = self#find_static_method desc.cls desc.name desc.desc in
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
            match find_method def_cls.raw method_desc.name method_desc.desc with
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
      | Invokevirtual desc | Invokeinterface (desc, _) -> (
          let base_mth =
            self#find_virtual_method desc.cls desc.name desc.desc
          in
          assert (not_static base_mth);
          let args = self#pop_list base_mth.nargs in
          Printf.printf ">>>>>>>>>>>>>>>>> [%s]\n"
            (String.concat ", " (List.map string_of_evalue args));
          let objectref = self#pop () in
          match objectref with
          | Object x ->
              (* todo remove this constraint *)
              assert (self#is_indirect_superclass x.cls desc.cls);
              let clsref = x.cls.raw in
              let resolved_mth =
                self#find_virtual_method clsref.name desc.name desc.desc
              in
              (* todo frame stuff *)
              self#exec resolved_mth (objectref :: args)
          | ByteArray ba ->
              (* TODO extract someone else? *)
              if
                base_mth.cls = "java/lang/Object"
                && base_mth.name = "clone"
                && base_mth.desc = "()Ljava/lang/Object;"
              then self#push (ByteArray (Bytes.copy ba))
              else failwith "invokevirtual on bytearray"
          | _ -> failwith "not an object 4")
      | Getstatic field_desc ->
          let def_cls = self#load_class field_desc.cls in
          StringMap.find field_desc.name def_cls.static |> ( ! ) |> self#push
      | Getfield field_desc -> (
          match self#pop () with
          | Object x ->
              StringMap.find field_desc.name x.fields |> ( ! ) |> self#push
          | _ -> failwith "Can't get field of non-object type")
      | Putstatic field_desc ->
          let def_cls = self#load_class field_desc.cls in
          let value = self#pop () in
          StringMap.find field_desc.name def_cls.static := value;
          Printf.printf "putstatic %s %s %s %s\n" def_cls.raw.name
            field_desc.name field_desc.desc (string_of_evalue value)
      | Putfield field_desc -> (
          let value = self#pop () in
          match self#pop () with
          | Object x ->
              StringMap.find field_desc.name x.fields := value;
              Printf.printf "putfield %s %s %s %s\n"
                (string_of_evalue (Object x))
                field_desc.name field_desc.desc (string_of_evalue value)
          | _ -> failwith "Can't put field of non-object type")
      | Aconst_null -> self#push Null
      | Return -> self#curframe.retval <- Some Void
      | Ireturn | Areturn | Lreturn | Freturn ->
          let value = self#pop () in
          self#curframe.retval <- Some value
      | New class_desc -> self#make_new class_desc.name |> self#push
      | Dup ->
          let v = self#pop () in
          self#push v;
          self#push v
      | Dup_x1 ->
          let v1 = self#pop () in
          let v2 = self#pop () in
          self#push v1;
          self#push v2;
          self#push v1
      | Dup2 ->
          let a = self#pop () in
          if size a = 2 then (
            self#push a;
            self#push a)
          else
            let b = self#pop () in
            self#push b;
            self#push a;
            self#push b;
            self#push a
      | Pop ->
          let _ = self#pop () in
          ()
      | Aload i | Iload i | Lload i | Fload i -> self#load i |> self#push
      | Astore i | Istore i | Lstore i | Fstore i -> self#pop () |> self#store i
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
            | Mul -> Int32.mul a b
            | Rem -> Int32.rem a b)
          |> self#push
      | Ishl ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          let s = Int32.logand b 0b11111l |> Int32.to_int in
          Int (Int32.shift_left a s) |> self#push
      | Ishr ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          let s = Int32.logand b 0b11111l |> Int32.to_int in
          Int (Int32.shift_right a s) |> self#push
      | Iushr ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          let s = Int32.logand b 0b11111l |> Int32.to_int in
          Int (Int32.shift_right_logical a s) |> self#push
      | Iand ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          Int (Int32.logand a b) |> self#push
      | Ixor ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          Int (Int32.logxor a b) |> self#push
      | Ior ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          Int (Int32.logor a b) |> self#push
      | Ineg ->
          let a = self#pop () |> as_int in
          Int (Int32.neg a) |> self#push
      | Larith op ->
          let b = self#pop () |> as_long in
          let a = self#pop () |> as_long in
          Long
            (match op with
            | Div -> Int64.div a b
            | Add -> Int64.add a b
            | Sub -> Int64.sub a b
            | Mul -> Int64.mul a b
            | Rem -> Int64.rem a b)
          |> self#push
      | Lshr ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_long in
          let s = Int32.logand b 0b11111_11111l |> Int32.to_int in
          Long (Int64.shift_right a s) |> self#push
      | Lshl ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_long in
          let s = Int32.logand b 0b11111_11111l |> Int32.to_int in
          Long (Int64.shift_left a s) |> self#push
      | I2f -> Float (self#pop () |> as_int |> Int32.to_float) |> self#push
      | I2b -> Int (self#pop () |> as_int |> truncate_byte_range) |> self#push
      | I2c -> Int (self#pop () |> as_int |> truncate_char_range) |> self#push
      | I2l -> Long (self#pop () |> as_int |> Int64.of_int32) |> self#push
      | F2i -> Int (self#pop () |> as_float |> Int32.of_float) |> self#push
      | F2d -> Double (self#pop () |> as_float) |> self#push
      | L2i -> Int (self#pop () |> as_long |> Int64.to_int32) |> self#push
      | L2f -> Float (self#pop () |> as_long |> Int64.to_float) |> self#push
      | D2l -> Long (self#pop () |> as_double |> Int64.of_float) |> self#push
      | Farith op ->
          let b = self#pop () |> as_float in
          let a = self#pop () |> as_float in
          Float
            (match op with
            | Div -> a /. b
            | Add -> a +. b
            | Sub -> a -. b
            | Mul -> a *. b
            | Rem -> Float.rem a b)
          |> self#push
      | Fneg ->
          let a = self#pop () |> as_float in
          Float (Float.neg a) |> self#push
      | Darith op ->
          let b = self#pop () |> as_double in
          let a = self#pop () |> as_double in
          Double
            (match op with
            | Div -> a /. b
            | Add -> a +. b
            | Sub -> a -. b
            | Mul -> a *. b
            | Rem -> Float.rem a b)
          |> self#push
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
          if compare_cond v 0l cond then self#curframe.nextpc <- target
      | If_icmp (cond, target) ->
          let b = self#pop () |> as_int in
          let a = self#pop () |> as_int in
          if compare_cond a b cond then self#curframe.nextpc <- target
      | If_acmpeq target ->
          let b = self#pop () in
          let a = self#pop () in
          if a == b then self#curframe.nextpc <- target
      | If_acmpne target ->
          let b = self#pop () in
          let a = self#pop () in
          if a != b then self#curframe.nextpc <- target
      | Lcmp ->
          let b = self#pop () in
          let a = self#pop () in
          Int (compare a b |> Int32.of_int) |> self#push
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
          | Shared.Double d -> self#push (Double d))
      | Iconst v | Bipush v | Sipush v -> self#push (Int v)
      | Lconst v -> self#push (Long v)
      | Fconst f -> self#push (Float f)
      | Dconst d -> self#push (Double d)
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
          let size = self#pop () |> as_int |> Int32.to_int in
          (match ty with
          | Byte -> ByteArray (Bytes.make size (Char.chr 0))
          | _ ->
              let arr = default_value ty |> Array.make size in
              Array { ty; arr })
          |> self#push
      | Aastore | Iastore -> (
          let v = self#pop () in
          let i = self#pop () |> as_int |> Int32.to_int in
          match self#pop () with
          | Array a -> a.arr.(i) <- v
          | _ -> failwith "Not an array")
      | Bastore -> (
          let v = self#pop () |> as_int |> Int32.to_int in
          let i = self#pop () |> as_int |> Int32.to_int in
          match self#pop () with
          | ByteArray a -> Bytes.set_int8 a i v
          | _ -> failwith "Not an array")
      | Castore -> (
          let v = self#pop () |> as_int |> truncate_char_range in
          let i = self#pop () |> as_int |> Int32.to_int in
          match self#pop () with
          | Array a -> a.arr.(i) <- Int v
          | _ -> failwith "Not an array")
      | Aaload ->
          let i = self#pop () |> as_int |> Int32.to_int in
          (match self#pop () with
          | Array x -> x.arr.(i)
          | _ -> failwith "Not an array")
          |> self#push
      | Baload ->
          let i = self#pop () |> as_int |> Int32.to_int in
          (match self#pop () with
          | ByteArray x -> Int (Bytes.get_int8 x i |> Int32.of_int)
          | _ -> failwith "Not an array")
          |> self#push
      | Caload ->
          let i = self#pop () |> as_int |> Int32.to_int in
          (match self#pop () with
          | Array x -> Int (x.arr.(i) |> as_int |> truncate_char_range)
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
          | Null -> self#push (Int 0l)
          | _ -> failwith "not an object 5")
      | Checkcast desc ->
          let v = self#pop () in
          (match v with
          | Object o -> assert (self#is_indirect_superclass o.cls desc.name)
          | ByteArray _ ->
              assert (desc.name = "[B" (*this is so dirty pls fixme*))
          | Null -> ()
          | _ -> failwith "not an object 6");
          self#push v
      (* TODO monitor stuff? *)
      | Monitorenter | Monitorexit ->
          let _ignore = self#pop () in
          ()
      | Lookupswitch (default, table) ->
          let key = self#pop () |> as_int |> Int32.to_int in
          List.iter (fun (a, b) -> Printf.printf "%d -> %d\n" a b) table;
          Printf.printf "default %d\n" default;
          let res = List.assoc_opt key table in
          (match res with
          | Some x -> Printf.printf "Some %d\n" x
          | None -> Printf.printf "None\n");
          assert (Option.is_none res);
          self#curframe.nextpc <-
            (match res with Some x -> x | None -> default)
      | Tableswitch (default, (low, high), targets) ->
          let key = self#pop () |> as_int |> Int32.to_int in
          let res =
            if key < low || key > high then default
            else List.nth targets (key - low)
          in
          self#curframe.nextpc <- res
      | x ->
          failwith
            (Printf.sprintf "Unimplemented instruction excecution %s"
               (Instr.string_of_instr x))

    method private initialize_locals (args : evalue list) =
      let i = ref 0 in
      List.iter
        (fun v ->
          self#curframe.locals.(!i) <- v;
          i := !i + size v)
        args

    method private exec_code (mth : jmethod) (code : Attr.code_attribute)
        (args : evalue list) : evalue =
      Debug.push "jvm_exec_code"
        (Printf.sprintf "%s.%s %s" mth.cls mth.name mth.desc);
      self#add_frame code.frame_size;
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
          Native.get_registered_fnptr interface mth.cls mth.name mth.desc
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
              Native.load_method libjava filtered_name
        in
        Printf.printf "%s %s %s -> %#x\n%!" mth.cls mth.name mth.desc
          method_handle;
        if method_handle = 0 then failwith "Method handle is null";
        let args_real =
          if mth.access_flags.is_static then
            let receiver = self#make_class_instance mth.cls in
            receiver :: args
          else args
        in
        Printf.printf "%#x ((%s) -> %s) [%s]\n%!" method_handle
          (String.concat ", " (List.map Type.string_of_dtype mth.arg_types))
          (Type.string_of_dtype mth.ret_type)
          (String.concat ", " (List.map string_of_evalue_detailed args_real));
        let result =
          Native.execute_native_auto interface args_real mth.arg_types
            mth.ret_type method_handle
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
      (* expected by openjdk *)
      let required_classes =
        [
          "java/lang/System";
          "java/lang/ThreadGroup";
          "java/lang/Thread";
          "java/lang/ref/Finalizer";
        ]
      in
      List.iter
        (fun x ->
          let _ignored = self#load_class x in
          ())
        required_classes;
      (* openjdk is special *)
      (match
         find_method
           (self#load_class_definition "java/lang/System")
           "initPhase1" "()V"
       with
      | Some init ->
          self#exec init [];
          ()
      | None -> ());

      let main_class = self#load_class main_class_name in
      match find_method main_class.raw "main" "([Ljava/lang/String;)V" with
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
  let libjava = Native.load_library "./class/extern-lib/libjava.so" in
  Printf.printf "libjava: %#x\n" libjava;
  let jvm = new jvm libjava in
  jvm#init;
  jvm
