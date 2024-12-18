open Basic
open Shared
open Java
open Type
open Attr

(* classIsInterface(Class) *)
let classIsInterface (cls : jclass) : bool = cls.access_flags.is_interface

(* classIsNotFinal(Class) *)
let classIsNotFinal (cls : jclass) : bool = not cls.access_flags.is_final

(* classSuperClassName(Class, SuperClassName) *)
let classSuperClassName (cls : jclass) : string =
  match cls.superclass with
  | Some x -> x
  | None ->
      failwith
        (Printf.sprintf
           "classSuperClassName: This class (%S) does not have a superclass"
           cls.name)

(* isBootstrapLoader(Loader) *)
let isBootstrapLoader (loader : jloader) : bool = loader = Bootstrap

let method_matches_desc (d : method_desc) (m : jmethod) : bool =
  m.name = d.name && m.desc = d.desc

let member_method_opt (desc : method_desc) (cls : jclass) : jmethod option =
  List.find_opt (method_matches_desc desc) cls.methods

let thisClass (env : jenvironment) : vclass = (env.cls.name, env.cls.loader)
let currentClassLoader (env : jenvironment) : jloader = env.cls.loader

let rec superclassChain (name : string) (loader : jloader) : vclass list =
  let cls = Loader.load_class name loader in
  match cls.name with
  | "java/lang/Object" ->
      assert (isBootstrapLoader cls.loader);
      []
  | _ ->
      let super_name = classSuperClassName cls in
      (super_name, cls.loader) :: superclassChain super_name cls.loader

let isJavaSubclassOf (sub : string) (sub_l : jloader) (super : string)
    (super_l : jloader) : bool =
  if sub = super && sub_l = super_l then true
  else
    let chain = superclassChain sub sub_l in
    let res = List.find_opt (fun (x, _) -> x = super) chain in
    match res with
    | Some (_, c_l) ->
        let a = Loader.load_class super c_l in
        let b = Loader.load_class super super_l in
        assert (a = b);
        true
    | None -> false

let array_supertypes =
  [ "java/lang/Object"; "java/lang/Cloneable"; "java/io/Serializable" ]

let rec isJavaAssignable (a : vtype) (b : vtype) : bool =
  match (a, b) with
  | x, y when x = y -> true
  | Class (_, _), Class ("java/lang/Object", Bootstrap) -> true
  | Class (f, fl), Class (t, tl) ->
      let tc = Loader.load_class t tl in
      if classIsInterface tc then true else isJavaSubclassOf f fl t tl
  | Array _, Class (name, loader)
    when loader = Loader.bootstrap_loader && List.mem name array_supertypes ->
      true
  | Array _, Class (_, _) -> false
  | Array (T f), Array (T t) -> isJavaAssignable f t
  | Array f, Array t -> f = t
  | _, _ -> false

let rec isAssignable (a : vtype) (b : vtype) : bool =
  match (a, b) with
  | x, y when x = y -> true
  | Void, _ -> failwith "void is not assignable"
  | _, Void -> failwith "void cannot be assigned to"
  | Top, _ -> false
  | OneWord, Top -> true
  | TwoWord, Top -> true
  | OneWord, _ -> false
  | TwoWord, _ -> false
  | Null, Class (_, _) -> true
  | Null, Array _ -> true
  | Class (f, fl), Class (t, tl) ->
      isJavaAssignable (Class (f, fl)) (Class (t, tl))
  | Array f, Class (t, tl) -> isJavaAssignable (Array f) (Class (t, tl))
  | Array f, Array t -> isJavaAssignable (Array f) (Array t)
  | Int, x -> isAssignable OneWord x
  | Float, x -> isAssignable OneWord x
  | Long, x -> isAssignable TwoWord x
  | Double, x -> isAssignable TwoWord x
  | Reference, x -> isAssignable OneWord x
  | Class (_, _), x -> isAssignable Reference x
  | Array _, x -> isAssignable Reference x
  | Uninitialized, x -> isAssignable Reference x
  | UninitializedThis, x -> isAssignable Uninitialized x
  | UninitializedOffset _, x -> isAssignable Uninitialized x
  | Null, x ->
      isAssignable (Class ("java/lang/Object", Loader.bootstrap_loader)) x

let rec finalMethodNotOverridden (mth : jmethod) (superclass : jclass) : bool =
  match
    member_method_opt { cls = ""; name = mth.name; desc = mth.desc } superclass
  with
  | Some mth ->
      let mflags = mth.access_flags in
      (mflags.is_final && (mflags.is_private || mflags.is_static))
      || (not mflags.is_final)
         && (mflags.is_private || mflags.is_static)
         && doesNotOverrideFinalMethodOfSuperclass superclass mth
      || (not mflags.is_final) && (not mflags.is_private)
         && not mflags.is_static
  | None -> doesNotOverrideFinalMethodOfSuperclass superclass mth

and doesNotOverrideFinalMethodOfSuperclass (cls : jclass) (mth : jmethod) : bool
    =
  Debug.push "doesNotOverrideFinalMethodOfSuperclass"
    (Debug.method_diagnostic mth cls);
  (* The spec doesn't specify this check for Object, but I don't see how it's
     possible for this to work without this check *)
  if cls.name = "java/lang/Object" then Debug.pop true
  else
    let superclass_name = classSuperClassName cls in
    let superclass = Loader.load_class superclass_name cls.loader in
    let result = finalMethodNotOverridden mth superclass in
    Debug.pop result

let doesNotOverrideFinalMethod (cls : jclass) (mth : jmethod) : bool =
  (* Debug.push "doesNotOverrideFinalMethod" (Debug.method_diagnostic mth cls); *)
  let result =
    (cls.name = "java/lang/Object" && isBootstrapLoader cls.loader)
    || mth.access_flags.is_private || mth.access_flags.is_static
    || (not mth.access_flags.is_private)
       && (not mth.access_flags.is_static)
       && doesNotOverrideFinalMethodOfSuperclass cls mth
  in
  (* Debug.pop result *)
  result

let rec mergeStackMapAndCode (stack_map : jstack_map list)
    (code : Instr.instruction list) : merged_code list =
  (* Printf.printf "mergeStackMapAndCode: m[%s] p[%s]\n"
     (String.concat ", " (List.map (fun (i, _) -> string_of_int i) stack_map))
     (String.concat ", " (List.map (fun (i, _) -> string_of_int i) code)); *)
  match (stack_map, code) with
  | [], _ -> List.map (fun x -> Instruction x) code
  | m :: m_rest, p :: p_rest ->
      let m_offset, _ = m in
      let p_offset, _ = p in
      if m_offset = p_offset then
        StackMap m :: Instruction p :: mergeStackMapAndCode m_rest p_rest
      else if p_offset < m_offset then
        Instruction p :: mergeStackMapAndCode (m :: m_rest) p_rest
      else failwith "Undefined: case 1"
  | _ -> failwith "Undefined: case 2"

let rec expandTypeList (args : vtype list) : vtype list =
  match args with
  | [] -> []
  | x :: xs -> (
      match size x with
      | 1 -> x :: expandTypeList xs
      | 2 -> x :: Top :: expandTypeList xs
      | _ -> failwith "Invalid size")

let methodInitialThisType (cls : jclass) (mth : jmethod) : vtype option =
  if mth.access_flags.is_static then (
    if mth.name = "<init>" then failwith "<init> cannot be static";
    None)
  else if mth.name = "<init>" then
    if cls.name = "java/lang/Object" then
      Some (Class ("java/lang/Object", Loader.bootstrap_loader))
    else Some UninitializedThis
  else Some (Class (cls.name, cls.loader))

let rec expandToLength (al : 'a list) (size : int) (filler : 'a) : 'a list =
  let als = List.length al in
  if als = size then al
  else if als > size then failwith "cannot expand, already larger"
  else expandToLength (al @ [ filler ]) size filler

let methodInitialStackFrame (cls : jclass) (mth : jmethod) (frame_size : int) :
    frame * vtype =
  let raw_args = List.map vtype_of_dtype mth.arg_types in
  let ret = vtype_of_dtype mth.ret_type in
  let args = expandTypeList raw_args in
  let this = methodInitialThisType cls mth in
  let flags =
    {
      is_this_uninit =
        (match this with Some UninitializedThis -> true | _ -> false);
    }
  in
  let this_args = match this with Some t -> t :: args | None -> args in
  let locals = expandToLength this_args frame_size Top in
  ({ locals; stack = []; flags }, ret)

let exists_offset (offset : int) (instr : merged_code) : bool =
  match instr with Instruction (i, _) -> i = offset | _ -> false

let offsetStackFrame (env : jenvironment) (offset : int) : frame =
  let instructions = env.instructions in
  let stack_map =
    List.find_map
      (function
        | StackMap (i, f) -> if i = offset then Some f else None | _ -> None)
      instructions
  in
  match stack_map with
  | Some x -> x
  | None ->
      failwith (Printf.sprintf "No stack frame exists at offset %d" offset)

let handlerIsLegal (env : jenvironment) (handler : exception_handler) : bool =
  assert (handler.starti < handler.endi);
  assert (List.exists (exists_offset handler.starti) env.instructions);
  assert (List.exists (exists_offset handler.endi) env.instructions);
  let _ = offsetStackFrame env handler.target in
  let loader = currentClassLoader env in
  let throwable = Class ("java/lang/Throwable", Loader.bootstrap_loader) in
  let exception_class =
    match handler.class_name with
    | Some name -> Class (name, loader)
    | None -> throwable
  in
  isAssignable exception_class throwable

let handlersAreLegal (env : jenvironment) : bool =
  let handlers = env.exception_handlers in
  List.for_all (handlerIsLegal env) handlers

let frameIsAssignable (a : frame) (b : frame) : unit =
  try
    let () =
      if List.length a.stack = List.length b.stack then ()
      else failwith "Stack size mismatch"
    in
    let () =
      if List.length a.locals = List.length b.locals then ()
      else failwith "Locals size mismatch"
    in
    let () =
      if List.for_all2 isAssignable a.locals b.locals then ()
      else failwith "Locals assignable mismatch"
    in
    let () =
      if List.for_all2 isAssignable a.stack b.stack then ()
      else failwith "Stack assignable mismatch"
    in
    let () =
      if a.flags.is_this_uninit = b.flags.is_this_uninit then ()
      else failwith "Flags mismatch"
    in
    ()
  with Failure x ->
    failwith
      (Printf.sprintf "Cannot assign frame %s to frame %s: %s"
         (string_of_frame a) (string_of_frame b) x)

type mframe = Frame of frame | AfterGoto

let instructionSatisfiesHandlers (_env : jenvironment) (_offset : int)
    (_excframe : frame) =
  true (* TODO *)

let popMatchingType (stack : vtype list) (t : vtype) : vtype list * vtype =
  try
    let expected_size, actual, rest =
      match stack with
      | Top :: actual :: rest -> (2, actual, rest)
      | actual :: rest -> (1, actual, rest)
      | _ -> failwith "Invalid stack state"
    in
    if size actual <> expected_size then
      failwith
        (Printf.sprintf "Top of stack expected to be size %d, but was size %d"
           expected_size (size actual));
    if not @@ isAssignable actual t then
      failwith
        (Printf.sprintf "%s not assignable to %s" (string_of_vtype actual)
           (string_of_vtype t));
    (rest, actual)
  with Failure f ->
    failwith
      (Printf.sprintf "Failure trying to pop %s from stack %s: %s"
         (string_of_vtype t) (string_of_stack stack) f)

let rec popMatchingList (stack : vtype list) (expected : vtype list) :
    vtype list =
  match expected with
  | [] -> stack
  | p :: rest ->
      let a, _ = popMatchingType stack p in
      popMatchingList a rest

let popCategory1 (stack : vtype list) : vtype * vtype list =
  let head = List.hd stack in
  assert (head <> Top);
  assert (size head = 1);
  (head, List.tl stack)

let popCategory2 (stack : vtype list) : vtype * vtype list =
  match stack with
  | Top :: t :: rest ->
      assert (size t = 2);
      (t, rest)
  | _ -> failwith "Can't pop category 2"

let pushOperandStack (stack : vtype list) (t : vtype) : vtype list =
  match t with
  | Void -> stack
  | _ -> (
      match size t with
      | 1 -> t :: stack
      | 2 -> Top :: t :: stack
      | _ -> failwith "Invalid size")

let operandStackHasLegalLength (env : jenvironment) (stack : vtype list) : unit
    =
  if List.length stack > env.max_stack then
    failwith "Stack is larger than max stack size"
  else ()

let validTypeTransition (env : jenvironment) (expected : vtype list)
    (result : vtype) (frame : frame) : frame =
  let a = popMatchingList frame.stack expected in
  let b = pushOperandStack a result in
  let () = operandStackHasLegalLength env b in
  { frame with stack = b }

let loadIsTypeSafe (env : jenvironment) (index : int) (t : vtype)
    (frame : frame) : frame =
  let locals = frame.locals in
  let actual_type = List.nth locals index in
  if isAssignable actual_type t then
    validTypeTransition env [] actual_type frame
  else
    failwith
      (Printf.sprintf
         "Cannot load %s from index %d of frame %s, actual type is %s"
         (string_of_vtype t) index (string_of_frame frame)
         (string_of_vtype actual_type))

let modifyPreIndexVariable (t : vtype) : vtype =
  match size t with 1 -> t | 2 -> Top | _ -> failwith "Invalid size"

let modifyLocalVariable (index : int) (t : vtype) (locals : vtype list) :
    vtype list =
  let size = size t in
  let modify (i : int) (n : vtype) : vtype =
    if i = index - 1 then modifyPreIndexVariable n
    else if i = index then (
      if size = 2 then assert (i + 2 <= List.length locals);
      t)
    else if i = index + 1 then if size = 2 then Top else n
    else n
  in
  List.mapi modify locals

let storeIsTypeSafe (_env : jenvironment) (index : int) (t : vtype)
    (frame : frame) : frame =
  let next_stack, actual_type = popMatchingType frame.stack t in
  let next_locals = modifyLocalVariable index actual_type frame.locals in
  { frame with stack = next_stack; locals = next_locals }

let canPop (f : frame) (types : vtype list) : frame =
  let new_stack = popMatchingList f.stack types in
  (* let () =
       Printf.printf "canPop: new=%s old=%s\n%!"
         (string_of_stack new_stack)
         (string_of_stack f.stack)
     in *)
  { f with stack = new_stack }

let canSafelyPush (env : jenvironment) (stack : vtype list) (t : vtype) :
    vtype list =
  let n = pushOperandStack stack t in
  let () = operandStackHasLegalLength env n in
  n

let rec canPushList (stack : vtype list) (ts : vtype list) : vtype list =
  match ts with
  | [] -> stack
  | t :: rest ->
      let n = pushOperandStack stack t in
      canPushList n rest

let canSafelyPushList (env : jenvironment) (stack : vtype list)
    (ts : vtype list) : vtype list =
  let n = canPushList stack ts in
  let () = operandStackHasLegalLength env n in
  n

let rewrittenUninitializedTypeThis (env : jenvironment) (cls : vclass) : vclass
    =
  let this = thisClass env in
  let expected =
    if cls = this then cls
    else
      let this_name, this_loader = this in
      let chain = superclassChain this_name this_loader in
      List.hd chain
  in
  if cls = expected then this (* not what the spec says *)
  else failwith "Failure rewriting uninitializedThis"

let rewrittenUninitializedTypeOffset (env : jenvironment) (offset : int)
    (cls : vclass) : vclass =
  let instr = env.instructions in
  let name, _ = cls in
  let expected_instruction = Instruction (offset, New { name }) in
  if List.mem expected_instruction instr then cls
  else failwith "Failure rewriting uninitializedOffset"

let substitute (prev : 'a) (next : 'a) (src : 'a list) : 'a list =
  List.map (fun x -> if x = prev then next else x) src

let targetIsTypeSafe (env : jenvironment) (frame : frame) (target : int) : unit
    =
  let new_frame = offsetStackFrame env target in
  (* let () =
       Printf.printf "new_frame=%s frame=%s\n%!"
         (string_of_frame new_frame)
         (string_of_frame frame)
     in *)
  frameIsAssignable frame new_frame

let exceptionStackFrame (frame : frame) : frame = { frame with stack = [] }

let loadable_vtype (c : loadable_constant) : vtype =
  match c with
  | Integer _ -> Int
  | Float _ -> Float
  | String _ -> Class ("java/lang/String", Loader.bootstrap_loader)
  | Class _ -> Class ("java/lang/Class", Loader.bootstrap_loader)
  | MethodType _ ->
      Class ("java/lang/invoke/MethodType", Loader.bootstrap_loader)
  | MethodHandle _ ->
      Class ("java/lang/invoke/MethodHandle", Loader.bootstrap_loader)

let loadable_vtype2 (c : loadable_constant2) : vtype =
  match c with Long _ -> Long | Double _ -> Double

(* TODO: assertion candidate *)
let isSmallArray (t : vtype) : bool =
  match t with Array Byte | Array Boolean | Null -> true | _ -> false

let next x = Frame x

let check_value_return env frame instr t =
  if env.return <> t then
    failwith
      (Printf.sprintf "%s: Function must return %s"
         (Instr.string_of_instr instr)
         (string_of_vtype t))
  else
    let _ = canPop frame [ t ] in
    AfterGoto

let next_frame_of_instr (i : Instr.instrbody) (env : jenvironment)
    (offset : int) (frame : frame) (exc_frame : frame ref) : mframe =
  match i with
  | Nop -> Frame frame
  | Aload i -> loadIsTypeSafe env i Reference frame |> next
  | Invokespecial m -> (
      let op_args, r = parse_method_descriptor m.desc |> map_vtype_method in
      match m.name with
      | "<init>" ->
          assert (r = Void);
          let stack_args = op_args |> List.rev in
          let f = canPop frame stack_args in
          let loader = currentClassLoader env in
          let head = List.hd f.stack in
          let stack = List.tl f.stack in
          let this_v, next_flags =
            match f.stack with
            | UninitializedThis :: _ ->
                let this_v =
                  rewrittenUninitializedTypeThis env (m.cls, loader)
                in
                let next_flags = { is_this_uninit = false } in
                (this_v, next_flags)
            | UninitializedOffset offset :: _ ->
                let this_v =
                  rewrittenUninitializedTypeOffset env offset (m.cls, loader)
                in
                let next_flags = frame.flags in
                (this_v, next_flags)
            | _ ->
                failwith
                  "invokespecial: Top of stack must have uninitialized value"
          in
          let this_name, this_loader = this_v in
          let this = Class (this_name, this_loader) in
          let next_stack = substitute head this stack in
          let next_locals = substitute head this f.locals in
          let exc_locals = substitute Top this f.locals in
          exc_frame := { locals = exc_locals; stack = []; flags = f.flags };
          let next_frame =
            { locals = next_locals; stack = next_stack; flags = next_flags }
          in
          (* TODO: passesProtectedCheck *)
          next next_frame
      | "<clinit>" -> failwith "invokespecial: <clinit> is not allowed"
      | _ ->
          let this_name, this_loader = thisClass env in
          let this = Class (this_name, this_loader) in
          let method_class = Class (m.cls, this_loader) in
          assert (isAssignable this method_class);
          let stack_args = List.rev (this :: op_args) in
          let next_frame = validTypeTransition env stack_args r frame in
          let stack_args2 = List.rev (method_class :: op_args) in
          let _ = validTypeTransition env stack_args2 r frame in
          next next_frame)
  | Invokevirtual m ->
      assert (m.name <> "<init>");
      assert (m.name <> "<clinit>");
      (*oh no*)
      let _loader = currentClassLoader env in
      let op_args, r = parse_method_descriptor m.desc |> map_vtype_method in
      let cls = Type.parse_class_internal_name m.cls |> Type.vtype_of_dtype in
      let stack_arg_list = List.rev (cls :: op_args) in
      let n = validTypeTransition env stack_arg_list r frame in
      (* let arg_list = List.rev op_args in *)
      (* let popped = canPop frame arg_list in *)
      (* TODO: passesProtectedCheck *)
      next n
  | Invokestatic m ->
      assert (m.name <> "<init>");
      assert (m.name <> "<clinit>");
      let op_args, r = parse_method_descriptor m.desc |> map_vtype_method in
      let stack_arg_list = List.rev op_args in
      validTypeTransition env stack_arg_list r frame |> next
  | Invokedynamic m ->
      assert (m.name <> "<init>");
      assert (m.name <> "<clinit>");
      let op_args, r = parse_method_descriptor m.desc |> map_vtype_method in
      let stack_arg_list = List.rev op_args in
      validTypeTransition env stack_arg_list r frame |> next
  | Invokeinterface (m, count) ->
      let assert_countIsValid (count : int) (in_frame : frame)
          (out_frame : frame) : unit =
        let in_len = List.length in_frame.stack in
        let out_len = List.length out_frame.stack in
        assert (in_len - out_len = count)
      in

      assert (m.name <> "<init>");
      assert (m.name <> "<clinit>");
      let op_args, r = parse_method_descriptor m.desc |> map_vtype_method in
      let loader = currentClassLoader env in
      let intf = Class (m.cls, loader) in
      let stack_arg_list = List.rev (intf :: op_args) in
      let temp_frame = canPop frame stack_arg_list in
      let n = validTypeTransition env [] r temp_frame in
      let () = assert_countIsValid count frame temp_frame in
      next n
  | Return ->
      if env.return <> Void then failwith "return: Function must return void"
      else if frame.flags.is_this_uninit then
        failwith "return: Cannot return when this is uninitialized"
      else AfterGoto
  | Areturn ->
      if not @@ isAssignable env.return Reference then
        failwith "areturn: Function must return reference"
      else
        let _ = canPop frame [ env.return ] in
        AfterGoto
  | Ireturn -> check_value_return env frame i Int
  | Lreturn -> check_value_return env frame i Long
  | Freturn -> check_value_return env frame i Float
  | Dreturn -> check_value_return env frame i Double
  | Iconst _ -> validTypeTransition env [] Int frame |> next
  | Lconst _ -> validTypeTransition env [] Long frame |> next
  | Fconst _ -> validTypeTransition env [] Float frame |> next
  | Dconst _ -> validTypeTransition env [] Double frame |> next
  | Iload i -> loadIsTypeSafe env i Int frame |> next
  | Lload i -> loadIsTypeSafe env i Long frame |> next
  | Fload i -> loadIsTypeSafe env i Float frame |> next
  | Dload i -> loadIsTypeSafe env i Double frame |> next
  | Istore i -> storeIsTypeSafe env i Int frame |> next
  | Lstore i -> storeIsTypeSafe env i Long frame |> next
  | Astore i -> storeIsTypeSafe env i Reference frame |> next
  | Fstore i -> storeIsTypeSafe env i Float frame |> next
  | Dstore i -> storeIsTypeSafe env i Double frame |> next
  | If_acmpeq t | If_acmpne t ->
      let next_frame = canPop frame [ Reference; Reference ] in
      let () = targetIsTypeSafe env next_frame t in
      next next_frame
  | If_icmp (_, t) ->
      let next_frame = canPop frame [ Int; Int ] in
      let () = targetIsTypeSafe env next_frame t in
      next next_frame
  | If (_, t) ->
      let next_frame = canPop frame [ Int ] in
      let () = targetIsTypeSafe env next_frame t in
      next next_frame
  | Goto t ->
      let () = targetIsTypeSafe env frame t in
      AfterGoto
  | Iarith _ | Ishl | Ishr | Iushr | Iand | Ior | Ixor ->
      validTypeTransition env [ Int; Int ] Int frame |> next
  | Larith _ | Land | Lor | Lxor ->
      validTypeTransition env [ Long; Long ] Long frame |> next
  | Lshl | Lshr | Lushr ->
      validTypeTransition env [ Int; Long ] Long frame |> next
  | Farith _ -> validTypeTransition env [ Float; Float ] Float frame |> next
  | Darith _ -> validTypeTransition env [ Double; Double ] Double frame |> next
  | New _ ->
      let new_item = UninitializedOffset offset in
      assert (not @@ List.mem new_item frame.stack);
      let new_locals = substitute new_item Top frame.locals in
      validTypeTransition env [] new_item { frame with locals = new_locals }
      |> next
  | Dup ->
      let t, _ = popCategory1 frame.stack in
      let next_stack = canSafelyPush env frame.stack t in
      { frame with stack = next_stack } |> next
  | Dup_x1 ->
      let t1, s1 = popCategory1 frame.stack in
      let t2, s2 = popCategory1 s1 in
      let next_stack = canSafelyPushList env s2 [ t1; t2; t1 ] in
      { frame with stack = next_stack } |> next
  | Dup_x2 ->
      let t1, s1 = popCategory1 frame.stack in
      let next_stack =
        match List.hd s1 |> size with
        | 1 ->
            let t2, s2 = popCategory1 s1 in
            let t3, rest = popCategory1 s2 in
            canSafelyPushList env rest [ t1; t3; t2; t1 ]
        | 2 ->
            let t2, rest = popCategory2 s1 in
            canSafelyPushList env rest [ t1; t2; t1 ]
        | _ -> failwith "invalid"
      in
      { frame with stack = next_stack } |> next
  | Dup2 ->
      let next_stack =
        match frame.stack with
        | Top :: _ ->
            let t, _ = popCategory2 frame.stack in
            canSafelyPush env frame.stack t
        | _ ->
            let t1, temp = popCategory1 frame.stack in
            let t2, _ = popCategory1 temp in
            canSafelyPushList env frame.stack [ t2; t1 ]
      in
      { frame with stack = next_stack } |> next
  | Ldc c ->
      let t = loadable_vtype c in
      validTypeTransition env [] t frame |> next
  | Ldc2_w c ->
      let t = loadable_vtype2 c in
      validTypeTransition env [] t frame |> next
  | Pop ->
      let _, rest = popCategory1 frame.stack in
      { frame with stack = rest } |> next
  | Athrow ->
      let throwable = Class ("java/lang/Throwable", Loader.bootstrap_loader) in
      let _ = canPop frame [ throwable ] in
      AfterGoto
  | Lcmp -> validTypeTransition env [ Long; Long ] Int frame |> next
  | Getfield f ->
      let t = parse_field_descriptor f.desc |> vtype_of_dtype in
      (* TODO: passesProtectedCheck *)
      let loader = currentClassLoader env in
      validTypeTransition env [ Class (f.cls, loader) ] t frame |> next
  | Putfield f ->
      let t = parse_field_descriptor f.desc |> vtype_of_dtype in
      if List.nth frame.stack 1 = UninitializedThis then (
        assert (env.mth.name = "<init>");
        assert (
          env.cls.fields |> List.exists (fun (f' : jfield) -> f'.name = f.name));
        canPop frame [ t; UninitializedThis ] |> next)
      else
        let _popped = canPop frame [ t ] in
        (* TODO: passesProtectedCheck *)
        let loader = currentClassLoader env in
        canPop frame [ t; Class (f.cls, loader) ] |> next
  | Getstatic f ->
      let t = parse_field_descriptor f.desc |> vtype_of_dtype in
      validTypeTransition env [] t frame |> next
  | Putstatic f ->
      let t = parse_field_descriptor f.desc |> vtype_of_dtype in
      canPop frame [ t ] |> next
  | Arraylength ->
      let arraytype = List.nth frame.stack 0 in
      (match arraytype with
      | Array _ -> ()
      | _ -> failwith "arraylength: must have array on top of stack");
      validTypeTransition env [ Top ] Int frame |> next
  | Aconst_null -> validTypeTransition env [] Null frame |> next
  | Ineg | I2b | I2c | I2s -> validTypeTransition env [ Int ] Int frame |> next
  | Lneg -> validTypeTransition env [ Long ] Long frame |> next
  | Fneg -> validTypeTransition env [ Float ] Float frame |> next
  | Sipush _ | Bipush _ -> validTypeTransition env [] Int frame |> next
  | Iinc (i, _) ->
      assert (List.nth frame.locals i = Int);
      next frame
  | Checkcast c ->
      let t = Type.parse_class_internal_name c.name |> Type.vtype_of_dtype in
      let obj = Class ("java/lang/Object", Loader.bootstrap_loader) in
      validTypeTransition env [ obj ] t frame |> next
  | Instanceof c ->
      let _ = Type.parse_class_internal_name c.name |> Type.vtype_of_dtype in
      let obj = Class ("java/lang/Object", Loader.bootstrap_loader) in
      validTypeTransition env [ obj ] Int frame |> next
  | Anewarray c ->
      let t = Type.parse_class_internal_name c.name |> Type.vtype_of_dtype in
      validTypeTransition env [ Int ] (Array (T t)) frame |> next
  | Newarray t ->
      validTypeTransition env [ Int ] (Array (Type.arraytype_of_dtype t)) frame
      |> next
  | Ifnull t | Ifnonnull t ->
      let n = canPop frame [ Reference ] in
      let () = targetIsTypeSafe env n t in
      Frame n
  | I2f -> validTypeTransition env [ Int ] Float frame |> next
  | I2d -> validTypeTransition env [ Int ] Double frame |> next
  | D2i -> validTypeTransition env [ Double ] Int frame |> next
  | D2f -> validTypeTransition env [ Double ] Float frame |> next
  | D2l -> validTypeTransition env [ Double ] Long frame |> next
  | F2i -> validTypeTransition env [ Float ] Int frame |> next
  | F2l -> validTypeTransition env [ Float ] Long frame |> next
  | F2d -> validTypeTransition env [ Float ] Double frame |> next
  | I2l -> validTypeTransition env [ Int ] Long frame |> next
  | L2i -> validTypeTransition env [ Long ] Int frame |> next
  | L2f -> validTypeTransition env [ Long ] Float frame |> next
  | L2d -> validTypeTransition env [ Long ] Double frame |> next
  | Monitorenter | Monitorexit -> canPop frame [ Reference ] |> next
  | Aaload ->
      let arraytype = List.nth frame.stack 1 in
      let component_type =
        match arraytype with
        | Array (T x) -> x
        | Null -> Null
        | _ -> failwith "Invalid component type for aaload"
      in
      let obj = Class ("java/lang/Object", Loader.bootstrap_loader) in
      validTypeTransition env [ Int; Array (T obj) ] component_type frame
      |> next
  | Aastore ->
      let obj = Class ("java/lang/Object", Loader.bootstrap_loader) in
      canPop frame [ obj; Int; Array (T obj) ] |> next
  | Baload ->
      let arraytype = List.nth frame.stack 1 in
      assert (isSmallArray arraytype);
      validTypeTransition env [ Int; Top ] Int frame |> next
  | Bastore ->
      let arraytype = List.nth frame.stack 2 in
      assert (isSmallArray arraytype);
      canPop frame [ Int; Int; Top ] |> next
  | Caload -> validTypeTransition env [ Int; Array Char ] Int frame |> next
  | Castore -> canPop frame [ Int; Int; Array Char ] |> next
  | Daload ->
      validTypeTransition env [ Int; Array (T Double) ] Double frame |> next
  | Dastore -> canPop frame [ Double; Int; Array (T Double) ] |> next
  | Faload ->
      validTypeTransition env [ Int; Array (T Float) ] Float frame |> next
  | Fastore -> canPop frame [ Float; Int; Array (T Float) ] |> next
  | Iaload -> validTypeTransition env [ Int; Array (T Int) ] Int frame |> next
  | Iastore -> canPop frame [ Int; Int; Array (T Int) ] |> next
  | Laload -> validTypeTransition env [ Int; Array (T Long) ] Long frame |> next
  | Lastore -> canPop frame [ Long; Int; Array (T Long) ] |> next
  | Saload -> validTypeTransition env [ Int; Array Short ] Int frame |> next
  | Sastore -> canPop frame [ Int; Int; Array Short ] |> next
  | Lookupswitch (default, pairs) ->
      let sorted =
        List.sort (fun (k1, _) (k2, _) -> Stdlib.compare k1 k2) pairs
      in
      assert (pairs = sorted);
      let branch_frame = canPop frame [ Int ] in
      let () =
        List.iter (fun (_, v) -> targetIsTypeSafe env branch_frame v) pairs
      in
      let () = targetIsTypeSafe env branch_frame default in
      AfterGoto
  | Tableswitch (default, (low, high), offsets) ->
      (* pretty sure it'd already have blown up if this were to fail*)
      assert (low <= high);
      let branch_frame = canPop frame [ Int ] in
      let () = List.iter (targetIsTypeSafe env branch_frame) offsets in
      let () = targetIsTypeSafe env branch_frame default in
      AfterGoto
  | Fcmpl | Fcmpg -> validTypeTransition env [ Float; Float ] Int frame |> next
  | Dcmpl | Dcmpg ->
      validTypeTransition env [ Double; Double ] Int frame |> next
  | Multianewarray (cls, dim) ->
      let dimensionality =
        match String.rindex_opt cls.name '[' with Some i -> i + 1 | None -> 0
      in
      assert (dim > 0);
      assert (dimensionality >= 0);
      let res = parse_class_internal_name cls.name |> Type.vtype_of_dtype in
      let int_list = List.init dim (fun _ -> Int : _ -> vtype) in
      validTypeTransition env int_list res frame |> next
  | unimplemented ->
      failwith
        (Printf.sprintf "unimplemented instruction verification %s"
           (Instr.string_of_instr unimplemented))

let instructionIsTypeSafe (i : Instr.instrbody) (env : jenvironment)
    (offset : int) (frame : frame) : mframe * frame =
  Debug.instr i offset;
  let exc_frame = exceptionStackFrame frame |> ref in
  let next_frame = next_frame_of_instr i env offset frame exc_frame in
  (next_frame, !exc_frame)

let rec mergedCodeIsTypeSafe (env : jenvironment) (code : merged_code list)
    (mframe : mframe) : bool =
  (* Debug.push_rec "mergedCodeIsTypeSafe" (Debug.env_diagnostic env); *)
  (match mframe with
  | Frame frame -> Debug.frame frame
  | AfterGoto -> Debug.after_goto ());

  (* let result = *)
  match (code, mframe) with
  | StackMap (_, map_frame) :: more_code, Frame frame ->
      let () = frameIsAssignable frame map_frame in
      mergedCodeIsTypeSafe env more_code (Frame map_frame)
  | Instruction (offset, body) :: more_code, Frame frame ->
      let next_frame, exc_frame = instructionIsTypeSafe body env offset frame in
      assert (instructionSatisfiesHandlers env offset exc_frame);
      mergedCodeIsTypeSafe env more_code next_frame
  | StackMap (_, map_frame) :: more_code, AfterGoto ->
      mergedCodeIsTypeSafe env more_code (Frame map_frame)
  | Instruction (_, _) :: _, AfterGoto ->
      failwith "No stack frame after unconditional branch"
  | [], AfterGoto -> true
  | [], Frame _ -> failwith "Ran out of code?"
(* in *)
(* Debug.pop result *)

let rec chop_rev (stack : vtype list) (remove : int) (original_remove : int) :
    vtype list =
  if remove = 0 then stack
  else
    match stack with
    | [] -> failwith "Ran out of space to chop off"
    | Top :: long :: rest when size long = 2 ->
        Top :: Top :: chop_rev rest (remove - 1) original_remove
    | Top :: rest ->
        Top
        :: chop_rev rest
             (if remove = original_remove then remove else remove - 1)
             original_remove
    | _ :: rest -> Top :: chop_rev rest (remove - 1) original_remove

let chop (stack : vtype list) (remove : int) : vtype list =
  List.rev (chop_rev (List.rev stack) remove remove)

let rec append (stack : vtype list) (extra : vtype list) : vtype list =
  (* let () =
       Printf.printf "[append] %s <- %s\n" (string_of_stack stack)
         (string_of_stack extra)
     in *)
  match (stack, extra) with
  | _, [] -> stack
  | [], _ -> failwith "Ran out of space to append to"
  | Top :: Top :: s_rest, a :: a_rest
    when size a = 2 && (not @@ List.exists (( <> ) Top) s_rest) ->
      a :: Top :: append s_rest a_rest
  | Top :: s_rest, a :: a_rest when not @@ List.exists (( <> ) Top) s_rest ->
      a :: append s_rest a_rest
  | long :: Top :: s_rest, _ when size long = 2 ->
      long :: Top :: append s_rest extra
  | s :: s_rest, _ -> s :: append s_rest extra

let expand_locals (frame_size : int) (locals : vtype list) : vtype list =
  let x = expandTypeList locals in
  let pad = List.init (frame_size - List.length x) (fun _ -> Top) in
  x @ pad

let convert_stack_map (frame_size : int) ((offset, frame) : jstack_map)
    ((delta, desc) : delta_frame) : jstack_map * jstack_map =
  if offset = 0 && not Debug.concise then
    Printf.printf "   0: %s\n" (string_of_frame frame);
  let this_offset = offset + delta in
  let next_offset = this_offset + 1 in
  if not Debug.concise then
    Printf.printf "%4d: %s\n" this_offset
      (match desc with
      | Same -> "same"
      | SameLocals1StackItem i ->
          Printf.sprintf "same_locals_1_stack_item %s" (string_of_vtype i)
      | Chop i -> Printf.sprintf "chop %d" i
      | Append i ->
          List.map string_of_vtype i |> String.concat ", "
          |> Printf.sprintf "append [%s]"
      | FullFrame i ->
          Printf.sprintf "full_frame locals=[%s] stack=[%s]"
            (List.map string_of_vtype i.locals |> String.concat ", ")
            (List.map string_of_vtype i.stack |> String.concat ", "));
  let v =
    match desc with
    | Same -> { frame with stack = [] }
    | SameLocals1StackItem i ->
        { frame with stack = expandTypeList [ i ] |> List.rev }
    | Chop i -> { frame with stack = []; locals = chop frame.locals i }
    | Append i -> { frame with stack = []; locals = append frame.locals i }
    | FullFrame i ->
        {
          frame with
          stack = expandTypeList i.stack |> List.rev;
          locals = expand_locals frame_size i.locals;
        }
  in
  let has_uninit = List.mem UninitializedThis v.locals in
  let v_with_uninit = { v with flags = { is_this_uninit = has_uninit } } in
  if not Debug.concise then
    Printf.printf "      %s\n" (string_of_frame v_with_uninit);
  ((next_offset, v_with_uninit), (this_offset, v_with_uninit))

let get_stack_map (code : code_attribute) : delta_frame list =
  let v =
    List.find_map
      (function StackMapTable x -> Some x | _ -> None)
      code.attributes
  in
  match v with Some x -> x | None -> []

let methodWithCodeIsTypeSafe (cls : jclass) (mth : jmethod) : bool =
  (* Debug.push "methodWithCodeIsTypeSafe" (Debug.method_diagnostic mth cls); *)
  let find_code (attr : attribute) : code_attribute option =
    match attr with Code x -> Some x | _ -> None
  in
  match List.find_map find_code mth.attributes with
  | Some code ->
      let stack_frame, return =
        methodInitialStackFrame cls mth code.frame_size
      in
      let _, stack_map =
        List.fold_left_map
          (convert_stack_map code.frame_size)
          (0, stack_frame) (get_stack_map code)
      in
      (* let () =
           print_endline
             (String.concat ","
                (List.map
                   (fun (i, x) -> Printf.sprintf "(%d, %s)" i (string_of_frame x))
                   stack_map))
         in *)
      let merged = mergeStackMapAndCode stack_map code.code in
      let env : jenvironment =
        {
          cls;
          mth;
          return;
          instructions = merged;
          max_stack = code.max_stack;
          exception_handlers = code.handlers;
        }
      in

      assert (handlersAreLegal env);

      let result = mergedCodeIsTypeSafe env merged (Frame stack_frame) in
      (* Debug.pop result *)
      result
  | None ->
      failwith
        (Printf.sprintf "Method %s is missing Code attribute"
           (Debug.method_diagnostic mth cls))

let methodIsTypeSafe (cls : jclass) (mth : jmethod) : bool =
  Debug.push "methodIsTypeSafe" (Debug.method_diagnostic mth cls);
  let result =
    doesNotOverrideFinalMethod cls mth
    && ((mth.access_flags.is_abstract || mth.access_flags.is_native)
       || (not mth.access_flags.is_abstract)
          && (not mth.access_flags.is_native)
          && methodWithCodeIsTypeSafe cls mth)
  in
  Debug.pop result

(* classIsTypeSafe(Class) *)
let classIsTypeSafe (cls : jclass) : bool =
  Debug.push "classIsTypeSafe" (Debug.class_diagnostic cls);
  let result =
    match cls.name with
    | "java/lang/Object" ->
        let () =
          if isBootstrapLoader cls.loader then ()
          else
            failwith "Loader of java.lang.Object must be the bootstrap loader"
        in
        List.for_all (methodIsTypeSafe cls) cls.methods
    | _ ->
        let super_chain = superclassChain cls.name cls.loader in
        let () = assert (not (List.is_empty super_chain)) in
        let superclass_name = classSuperClassName cls in
        let superclass = Loader.load_class superclass_name cls.loader in
        let () =
          if classIsNotFinal superclass then ()
          else
            failwith
              (Printf.sprintf "%S, superclass of %S is final" superclass.name
                 cls.name)
        in
        List.for_all (methodIsTypeSafe cls) cls.methods
  in
  Debug.pop result
