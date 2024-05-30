open Jvmilia.Reader
open Jvmilia.Verify
open Jvmilia.Verify.Basic

let parse (path : string) =
  let ch = In_channel.open_bin path in
  Classfile.read_class_file ch

let verify (name : string) =
  initialize_bootstrap_loader Testclasses.test_loader;
  let cls = Main.load_class name bootstrap_loader in
  let safe = Main.classIsTypeSafe cls in
  Printf.printf "Class %S is safe: %B\n" cls.name safe;
  if safe then print_endline "\027[32;1mSUCCESS!!\027[0m"

let () =
  if Array.length Sys.argv <= 2 then print_endline "usage: <action> <param>"
  else
    let action = Sys.argv.(1) in
    let param = Sys.argv.(2) in
    match action with
    | "verify" ->
        let _ = verify param in
        ()
    | "parse" ->
        let _ = parse param in
        ()
    | _ -> print_endline "error: Action must be verify or parse"
