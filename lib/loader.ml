open Java
open Shared
module StringMap = Map.Make (String)

type bootstrap_loader = {
  mutable known : jclass StringMap.t;
  load : string -> jclass;
}

let bootstrap_loader_ref : bootstrap_loader option ref = ref None

let initialize_bootstrap_loader (loader : string -> jclass) : unit =
  match !bootstrap_loader_ref with
  | Some _ -> failwith "Bootstrap loader has already been initialized"
  | None ->
      bootstrap_loader_ref :=
        Some { known = StringMap.empty; load = loader }

let bootstrap_loader_impl () : bootstrap_loader =
  match !bootstrap_loader_ref with
  | Some loader -> loader
  | None -> failwith "Bootstrap loader has not been initialized"

let bootstrap_loader = Bootstrap

let load_class (name : string) (loader : jloader) : jclass =
  match loader with
  | Bootstrap -> (
      let impl = bootstrap_loader_impl () in
      match StringMap.find_opt name impl.known with
      | Some existing -> existing
      | None ->
          let cls = impl.load name in
          impl.known <- StringMap.add name cls impl.known;
          cls)
  | UserDefined n ->
      failwith (Printf.sprintf "Cannot use user-defined loader %s" n)
