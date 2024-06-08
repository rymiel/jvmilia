open Java

let parse (path : string) : jclass =
  let ch = In_channel.open_bin path in
  let classfile = Reader.Classfile.read_class_file ch in
  Reader.Classfile.convert_class_file classfile Loader.bootstrap_loader

let class_base_dir = "class/"
let class_sub_dirs = [ "lib/"; "test/"; "openjdk/" ] (* in order of priority *)

let test_loader (name : string) : jclass =
  let make_path subdir = class_base_dir ^ subdir ^ name ^ ".class" in
  let candidate_paths = List.map make_path class_sub_dirs in
  Printf.printf "\027[33m! bootstrap class loader is loading %S\027[0m\n" name;
  let find_class path =
    if Sys.file_exists path then Some (parse path) else None
  in
  let found_class = List.find_map find_class candidate_paths in
  match found_class with
  | Some cls -> cls
  | None ->
      Printf.printf
        "\027[31mClass %S not found. Searched the following locations: %s\027[0m\n"
        name
        (String.concat ", "
           (List.map (fun x -> Printf.sprintf "%S" x) candidate_paths));
      failwith "Class not found"
