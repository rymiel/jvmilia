open Java
open Shared

module StringMap = Map.Make (String)

type bootstrap_loader = {
  known : jclass StringMap.t ref;
  load : string -> jclass;
}

let bootstrap_loader_ref : bootstrap_loader option ref = ref None

let initialize_bootstrap_loader (loader : string -> jclass) : unit =
  match !bootstrap_loader_ref with
  | Some _ -> failwith "Bootstrap loader has already been initialized"
  | None ->
      bootstrap_loader_ref :=
        Some { known = ref StringMap.empty; load = loader }

let bootstrap_loader_impl () : bootstrap_loader =
  match !bootstrap_loader_ref with
  | Some loader -> loader
  | None -> failwith "Bootstrap loader has not been initialized"

let bootstrap_loader = Bootstrap
