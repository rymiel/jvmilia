open Java

let parse (path : string) : jclass =
  let ch = In_channel.open_bin path in
  let classfile = Reader.Classfile.read_class_file ch in
  Reader.Classfile.convert_class_file classfile Loader.bootstrap_loader

let class_base_dir = "class/"
let class_sub_dirs = [ "lib/"; "test/"; "extern/" ] (* in order of priority *)

let silent =
  match Sys.getenv_opt "JVMILIA_BOOTSTRAP_LOG" with
  | Some "silent" -> true
  | Some _ | None -> false

let test_loader (name : string) : jclass =
  let make_path subdir = (class_base_dir ^ subdir ^ name ^ ".class", subdir) in
  let candidate_paths = List.map make_path class_sub_dirs in
  if not silent then
    Printf.printf "\027[33m! bootstrap loading %S\027[0m..." name;
  let find_class (path, subdir) =
    if Sys.file_exists path then Some (parse path, subdir) else None
  in
  let found_class = List.find_map find_class candidate_paths in
  match found_class with
  | Some (cls, path) ->
      if not silent then Printf.printf "\t\027[33mfound in %S\027[0m\n" path;
      cls
  | None ->
      Printf.printf
        "\n\
         \027[31mClass %S not found. Searched the following locations: %s\027[0m\n"
        name
        (String.concat ", "
           (List.map
              (fun (x, subdir) -> Printf.sprintf "%S (%s)" x subdir)
              candidate_paths));
      failwith "Class not found"
