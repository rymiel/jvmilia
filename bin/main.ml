open Jvmilia.Verify
open Jvmilia.Testclasses
open Jvmilia.Basic

let test_loader (name : string) : jclass =
  Printf.printf "\027[33m! bootstrap class loader is loading %S\027[0m\n" name;
  match name with
  | "java/lang/Object" -> java_lang_Object
  | "java/lang/String" -> java_lang_String
  | "test/One" -> test_One
  | "test/Two" -> test_Two
  | _ -> failwith (Printf.sprintf "test_loader can't load %S" name)

let () =
  initialize_bootstrap_loader test_loader;
  let cls = load_class Sys.argv.(1) bootstrap_loader in
  let safe = classIsTypeSafe cls in
  Printf.printf "Class %S is safe: %B\n" cls.name safe;
  if safe then print_endline "\027[32;1mSUCCESS!!\027[0m"
