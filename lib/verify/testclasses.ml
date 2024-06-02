open Java

let parse (path : string) : jclass =
  let ch = In_channel.open_bin path in
  let classfile = Reader.Classfile.read_class_file ch in
  Reader.Classfile.convert_class_file classfile Loader.bootstrap_loader

let class_dir =
  let slash = String.rindex __FILE__ '/' in
  let dir = String.sub __FILE__ 0 (slash + 1) in
  dir ^ "../../test/classes/"

let test_loader (name : string) : jclass =
  let class_file_name = class_dir ^ name ^ ".class" in
  let real_path = Unix.realpath class_file_name in
  Printf.printf
    "\027[33m! bootstrap class loader is loading %S from %S\027[0m\n" name
    real_path;
  parse real_path
