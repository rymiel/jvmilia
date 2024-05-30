open Basic
open Shared

(* classClassName(Class, ClassName) *)
let classClassName (cls : jclass) : string = cls.name

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

(* classInterfaces(Class, Interfaces) *)
let classInterfaces (cls : jclass) : string list = cls.superinterfaces

(* classMethods(Class, Methods) *)
let classMethods (cls : jclass) : jmethod list = cls.methods

(* classDeclaresMember(Class, MemberName, MemberDescriptor) *)
(* let classDeclaresMember (cls : jclass) (name : string) (desc : string) =
   failwith "x" *)

(* classDefiningLoader(Class, Loader) *)
let classDefiningLoader (cls : jclass) : jloader =
  match cls.loader with
  | Some loader -> loader
  | None ->
      failwith
        (Printf.sprintf "Loader wasn't initialized for class %S" cls.name)

(* isBootstrapLoader(Loader) *)
let isBootstrapLoader (loader : jloader) : bool =
  match loader with Bootstrap -> true | _ -> false

(* loadedClass(Name, InitiatingLoader, ClassDefinition) *)

(* methodName(Method, Name) *)
let methodName (mth : jmethod) : string = mth.name

(* methodAccessFlags(Method, AccessFlags) *)
let methodAccessFlags (mth : jmethod) : method_access_flags = mth.access_flags

(* methodDescriptor(Method, Descriptor) *)
let methodDescriptor (mth : jmethod) : string = mth.desc

(* methodAttributes(Method, Attributes) *)
let methodAttributes (mth : jmethod) : jattribute list = mth.attributes

(* isInit(Method) *)
let isInit (mth : jmethod) : bool = mth.name = "<init>"

let method_matches_desc (d : method_desc) (m : jmethod) : bool =
  m.name = d.name && m.desc = d.desc

let member_method_opt (desc : method_desc) (cls : jclass) : jmethod option =
  List.find_opt (method_matches_desc desc) cls.methods

let member_method (desc : method_desc) (cls : jclass) : jmethod =
  List.find (method_matches_desc desc) cls.methods

(* loadedClass(Name, InitiatingLoader, ClassDefinition) *)
let load_class (name : string) (loader : jloader) : jclass =
  match loader with
  | Bootstrap -> (
      let impl = bootstrap_loader_impl () in
      match StringMap.find_opt name !(impl.known) with
      | Some existing -> existing
      | None ->
          let cls = impl.load name in
          cls.loader <- Some loader;
          impl.known := StringMap.add name cls !(impl.known);
          cls)
  | UserDefined n ->
      failwith (Printf.sprintf "Cannot use user-defined loader %s" n)

(* parseMethodDescriptor(Descriptor, ArgTypeList, ReturnType) *)
let rec parseMethodDescriptor (desc : string) : vtype list * vtype =
  let s = desc in
  assert (String.get s 0 = '(');
  let offset = ref 1 in
  let args = ref [] in
  while String.get s !offset <> ')' do
    let t = parse_vtype s offset in
    args := !args @ [ t ]
  done;
  incr offset;
  let ret = parse_vtype s offset in
  (!args, ret)

and parse_vtype (s : string) (offset : int ref) : vtype =
  match parse_arraytype s offset with
  | T x -> x
  | Byte | Char | Short | Boolean -> Int

and parse_arraytype (s : string) (offset : int ref) : arraytype =
  let c = String.get s !offset in
  incr offset;
  match c with
  | 'D' -> T Double
  | 'F' -> T Float
  | 'J' -> T Long
  | 'I' -> T Int
  | 'B' -> Byte
  | 'C' -> Char
  | 'S' -> Short
  | 'Z' -> Boolean
  | 'V' -> T Void
  | '[' -> T (Array (parse_arraytype s offset))
  | 'L' ->
      let count = ref 0 in
      while String.get s (!offset + !count) <> ';' do
        incr count
      done;
      let classname = String.sub s !offset !count in
      offset := !offset + !count + 1;
      T (Class (classname, bootstrap_loader))
  | c -> failwith (Printf.sprintf "Invalid descriptor %c" c)

let thisClass (env : jenvironment) : vclass =
  let cls = env.cls in
  let loader = classDefiningLoader cls in
  let name = classClassName cls in
  (name, loader)

let currentClassLoader (env : jenvironment) : jloader =
  let _, loader = thisClass env in
  loader

let rec superclassChain (name : string) (loader : jloader) : vclass list =
  let cls = load_class name loader in
  let cloader = classDefiningLoader cls in
  match cls.name with
  | "java/lang/Object" ->
      assert (isBootstrapLoader cloader);
      []
  | _ ->
      let super_name = classSuperClassName cls in
      (super_name, cloader) :: superclassChain super_name cloader

let isJavaSubclassOf (sub : string) (sub_l : jloader) (super : string)
    (super_l : jloader) : bool =
  if sub = super && sub_l = super_l then true
  else
    let chain = superclassChain sub sub_l in
    let res = List.find_opt (fun (x, _) -> x = super) chain in
    match res with
    | Some (_, c_l) ->
        let a = load_class super c_l in
        let b = load_class super super_l in
        assert (a = b);
        true
    | None -> false

let rec isAssignable (a : vtype) (b : vtype) : bool =
  if a = b then true
  else
    match (a, b) with
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
        let tc = load_class t tl in
        if classIsInterface tc then true else isJavaSubclassOf f fl t tl
    | Array _, Class (_, _) -> failwith "TODO: array to class assignment"
    | Array _, Array _ -> failwith "TODO: array to array assignment"
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
    | Null, x -> isAssignable (Class ("java/lang/Object", bootstrap_loader)) x

let rec finalMethodNotOverridden (mth : jmethod) (superclass : jclass) : bool =
  let method_name = methodName mth in
  let method_desc = methodDescriptor mth in
  match
    member_method_opt
      { cls = ""; name = method_name; desc = method_desc }
      superclass
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
    let loader = classDefiningLoader cls in
    let superclass = load_class superclass_name loader in
    let result = finalMethodNotOverridden mth superclass in
    Debug.pop result

let doesNotOverrideFinalMethod (cls : jclass) (mth : jmethod) : bool =
  Debug.push "doesNotOverrideFinalMethod" (Debug.method_diagnostic mth cls);
  let result =
    cls.name = "java/lang/Object"
    && isBootstrapLoader (classDefiningLoader cls)
    || mth.access_flags.is_private || mth.access_flags.is_static
    || (not mth.access_flags.is_private)
       && (not mth.access_flags.is_static)
       && doesNotOverrideFinalMethodOfSuperclass cls mth
  in
  Debug.pop result

let rec mergeStackMapAndCode (stack_map : jstack_map list)
    (code : jinstruction list) : merged_code list =
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

let sizeOf (t : vtype) : int =
  match t with
  | Top | OneWord | Int | Float | Reference | Uninitialized | UninitializedThis
  | UninitializedOffset _
  | Class (_, _)
  | Array _ | Null ->
      1
  | TwoWord | Long | Double -> 2
  | Void -> failwith "Void has no size"

let rec expandTypeList (args : vtype list) : vtype list =
  match args with
  | [] -> []
  | x :: xs -> (
      match sizeOf x with
      | 1 -> x :: expandTypeList xs
      | 2 -> x :: Top :: expandTypeList xs
      | _ -> failwith "Invalid size")

let methodInitialThisType (cls : jclass) (mth : jmethod) : vtype option =
  if mth.access_flags.is_static then (
    if mth.name = "<init>" then failwith "<init> cannot be static";
    None)
  else if mth.name = "<init>" then
    if cls.name = "java/lang/Object" then
      Some (Class ("java/lang/Object", bootstrap_loader))
    else Some UninitializedThis
  else Some (Class (cls.name, classDefiningLoader cls))

let rec expandToLength (al : 'a list) (size : int) (filler : 'a) : 'a list =
  let als = List.length al in
  if als = size then al
  else if als > size then failwith "cannot expand, already larger"
  else expandToLength (al @ [ filler ]) size filler

let methodInitialStackFrame (cls : jclass) (mth : jmethod) (frame_size : int) :
    frame * vtype =
  let desc = methodDescriptor mth in
  let raw_args, ret = parseMethodDescriptor desc in
  let args = expandTypeList raw_args in
  let this = methodInitialThisType cls mth in
  let flags =
    {
      is_this_uninit =
        (match this with Some UninitializedThis -> true | _ -> false);
    }
  in
  let this_args = match this with Some t -> args @ [ t ] | None -> args in
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
  let throwable = Class ("java/lang/Throwable", bootstrap_loader) in
  let exception_class =
    match handler.class_name with
    | Some name -> Class (name, loader)
    | None -> throwable
  in
  isAssignable exception_class throwable

let handlersAreLegal (env : jenvironment) : bool =
  let handlers = env.exception_handlers in
  List.for_all (handlerIsLegal env) handlers

let frameIsAssignable (a : frame) (b : frame) =
  List.length a.stack = List.length b.stack
  && List.length a.locals = List.length b.locals
  && List.for_all2 isAssignable a.locals b.locals
  && List.for_all2 isAssignable a.stack b.stack
  && a.flags.is_this_uninit = b.flags.is_this_uninit

type mframe = Frame of frame | AfterGoto

let instructionSatisfiesHandlers (_env : jenvironment) (_offset : int)
    (_excframe : frame) =
  true (* TODO *)

let popMatchingType (stack : vtype list) (t : vtype) : vtype list * vtype =
  try
    match stack with
    | Top :: actual :: rest ->
        if sizeOf actual = 2 then (rest, actual)
        else
          failwith
            (Printf.sprintf "Top of stack is size 1 despite top guard: %s"
               (string_of_vtype actual))
    | actual :: rest ->
        if sizeOf actual = 1 then (rest, actual)
        else
          failwith
            (Printf.sprintf "Top of stack is size 2 without top guard: %s"
               (string_of_vtype actual))
    | _ -> failwith "Invalid stack state"
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

let pushOperandStack (stack : vtype list) (t : vtype) : vtype list =
  match t with
  | Void -> stack
  | _ -> (
      match sizeOf t with
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

let canPop (f : frame) (types : vtype list) : frame =
  let new_stack = popMatchingList f.stack types in
  (* let () =
       Printf.printf "canPop: new=%s old=%s\n%!"
         (string_of_stack new_stack)
         (string_of_stack f.stack)
     in *)
  { f with stack = new_stack }

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
  if cls = expected then cls else failwith "Failure rewriting uninitializedThis"

let substitute (prev : 'a) (next : 'a) (src : 'a list) : 'a list =
  List.map (fun x -> if x = prev then next else x) src

let targetIsTypeSafe (env : jenvironment) (frame : frame) (target : int) : bool
    =
  let new_frame = offsetStackFrame env target in
  (* let () =
       Printf.printf "new_frame=%s frame=%s\n%!"
         (string_of_frame new_frame)
         (string_of_frame frame)
     in *)
  frameIsAssignable frame new_frame

let exceptionStackFrame (frame : frame) : frame = { frame with stack = [] }

let rec instructionIsTypeSafe (i : instrbody) (env : jenvironment)
    (offset : int) (frame : frame) : mframe * frame =
  Debug.instr i offset;
  let defer (j : instrbody) = instructionIsTypeSafe j env offset frame in
  match i with
  | Aload_0 -> defer (Aload 0)
  | Aload_1 -> defer (Aload 1)
  | Aload_2 -> defer (Aload 2)
  | Aload_3 -> defer (Aload 3)
  | Aload i ->
      let n = loadIsTypeSafe env i Reference frame in
      (Frame n, exceptionStackFrame frame)
  | Invokespecial m -> (
      let op_args, r = parseMethodDescriptor m.desc in
      match m.name with
      | "<init>" -> (
          assert (r = Void);
          let stack_args = List.rev op_args in
          let f = canPop frame stack_args in
          let loader = currentClassLoader env in
          match f.stack with
          | UninitializedThis :: stack ->
              let this_v = rewrittenUninitializedTypeThis env (m.cls, loader) in
              let this_name, this_loader = this_v in
              let this = Class (this_name, this_loader) in
              let next_flags = { is_this_uninit = false } in
              let next_stack = substitute UninitializedThis this stack in
              let next_locals = substitute UninitializedThis this f.locals in
              let exc_locals = substitute Top this f.locals in
              let next_frame =
                { locals = next_locals; stack = next_stack; flags = next_flags }
              in
              let exc_frame =
                { locals = exc_locals; stack = []; flags = f.flags }
              in
              (Frame next_frame, exc_frame)
          | UninitializedOffset _ :: _ ->
              failwith "TODO: invokespecial uninitialized(n)"
          | _ ->
              failwith
                "invokespecial: Top of stack must have uninitialized value"
          (* failwith (Printf.sprintf "xxx %s" (string_of_frame f)) *))
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
          (Frame next_frame, exceptionStackFrame frame))
  | Return ->
      if env.return = Void then
        if frame.flags.is_this_uninit then
          failwith "return: Cannot return when this is uninitialized"
        else (AfterGoto, exceptionStackFrame frame)
      else failwith "return: Function must return void"
  | Iconst_m1 ->
      let next_frame = validTypeTransition env [] Int frame in
      (Frame next_frame, exceptionStackFrame frame)
  | Iconst_0 -> defer Iconst_m1
  | Iconst_1 -> defer Iconst_m1
  | Iconst_2 -> defer Iconst_m1
  | Iconst_3 -> defer Iconst_m1
  | Iconst_4 -> defer Iconst_m1
  | Iconst_5 -> defer Iconst_m1
  | Istore_0 -> failwith "TODO: istore_0"
  | Istore_1 -> failwith "TODO: istore_1"
  | Istore_2 -> failwith "TODO: istore_2"
  | Istore_3 -> failwith "TODO: istore_3"
  | Istore _ -> failwith "TODO: istore"
  | Iload_0 -> defer (Iload 0)
  | Iload_1 -> defer (Iload 1)
  | Iload_2 -> defer (Iload 2)
  | Iload_3 -> defer (Iload 3)
  | Iload i ->
      let n = loadIsTypeSafe env i Int frame in
      (Frame n, exceptionStackFrame frame)
  | Iadd -> failwith "TODO: iadd"
  | Ireturn ->
      if env.return = Int then
        let _ = canPop frame [ Int ] in
        (AfterGoto, exceptionStackFrame frame)
      else failwith "ireturn: Function must return int"
  | If_acmpeq t ->
      let next_frame = canPop frame [ Reference; Reference ] in
      assert (targetIsTypeSafe env next_frame t);
      (Frame next_frame, exceptionStackFrame frame)
  | If_acmpne t -> defer (If_acmpeq t)
  | Goto t ->
      assert (targetIsTypeSafe env frame t);
      (AfterGoto, exceptionStackFrame frame)

let rec mergedCodeIsTypeSafe (env : jenvironment) (code : merged_code list)
    (mframe : mframe) : bool =
  Debug.push_rec "mergedCodeIsTypeSafe" (Debug.env_diagnostic env);
  (match mframe with
  | Frame frame -> Debug.frame frame
  | AfterGoto -> Debug.after_goto ());

  let result =
    match (code, mframe) with
    | StackMap (_, map_frame) :: more_code, Frame frame ->
        assert (frameIsAssignable frame map_frame);
        mergedCodeIsTypeSafe env more_code (Frame map_frame)
    | Instruction (offset, body) :: more_code, Frame frame ->
        let next_frame, exc_frame =
          instructionIsTypeSafe body env offset frame
        in
        assert (instructionSatisfiesHandlers env offset exc_frame);
        mergedCodeIsTypeSafe env more_code next_frame
    | StackMap (_, map_frame) :: more_code, AfterGoto ->
        mergedCodeIsTypeSafe env more_code (Frame map_frame)
    | Instruction (_, _) :: _, AfterGoto ->
        failwith "No stack frame after unconditional branch"
    | [], AfterGoto -> true
    | [], Frame _ -> failwith "Ran out of code?"
  in
  Debug.pop result

let convertStackMap ((offset, frame) : jstack_map) ((delta, desc) : delta_frame)
    : jstack_map * jstack_map =
  let this_offset = offset + delta in
  let next_offset = this_offset + 1 in
  let v =
    match desc with
    | Same -> frame
    | SameLocals1StackItem i -> { frame with stack = [ i ] }
  in
  ((next_offset, v), (this_offset, v))

let methodWithCodeIsTypeSafe (cls : jclass) (mth : jmethod) : bool =
  Debug.push "methodWithCodeIsTypeSafe" (Debug.method_diagnostic mth cls);
  let find_code (attr : jattribute) : code_attribute option =
    match attr with Code x -> Some x | _ -> None
  in
  match List.find_map find_code mth.attributes with
  | Some code ->
      let stack_frame, return =
        methodInitialStackFrame cls mth code.frame_size
      in
      let _, stack_map =
        List.fold_left_map convertStackMap (0, stack_frame) code.stack_map_desc
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
      Debug.pop result
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

(* classIsTypeSafe(Class) :-
    classClassName(Class, Name),
    classDefiningLoader(Class, L),
    superclassChain(Name, L, Chain),
    Chain \= [],
    classSuperClassName(Class, SuperclassName),
    loadedClass(SuperclassName, L, Superclass),
    classIsNotFinal(Superclass),
    classMethods(Class, Methods),
    checklist(methodIsTypeSafe(Class), Methods). *)
let classIsTypeSafe (cls : jclass) : bool =
  Debug.push "classIsTypeSafe" (Debug.class_diagnostic cls);
  let name = classClassName cls in
  let loader = classDefiningLoader cls in
  let result =
    match name with
    | "java/lang/Object" ->
        let () =
          if isBootstrapLoader loader then ()
          else
            failwith "Loader of java.lang.Object must be the bootstrap loader"
        in
        let methods = classMethods cls in
        List.for_all (methodIsTypeSafe cls) methods
    | _ ->
        let super_chain = superclassChain name loader in
        let () = assert (not (List.is_empty super_chain)) in
        let superclass_name = classSuperClassName cls in
        let superclass = load_class superclass_name loader in
        let () =
          if classIsNotFinal superclass then ()
          else
            failwith
              (Printf.sprintf "%S, superclass of %S is final" superclass.name
                 cls.name)
        in
        let methods = classMethods cls in
        List.for_all (methodIsTypeSafe cls) methods
  in
  Debug.pop result
