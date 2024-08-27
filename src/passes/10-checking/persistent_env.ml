open Ast_typed
module Module_map = Map.Make (Module_var)

type t =
  { cmis : (Cmi.t * Cmi.crc) Filename.Map.t
  ; path_tbl : Filename.t Module_map.t
  ; virtual_env : Ast_typed.sig_item list
  }

let empty : t =
  { cmis = Filename.Map.empty
  ; path_tbl = Module_map.empty
  ; virtual_env = []
  }

let get_crc filename cmis = Map.find_exn cmis filename |> fun (_, crc) -> crc

let add_signature
    : t -> Module_var.t -> Filename.t -> Filename.t list -> Ast_typed.signature -> t
  =
 fun { cmis; path_tbl; virtual_env } m path imports sign ->
  let imports = List.map ~f:(fun filename -> filename, get_crc filename cmis) imports in
  let cmi = Cmi.{ path; sign; imports } in
  let crc = Cmi.Serialized.output cmi in
  let cmis = Map.set cmis ~key:path ~data:(cmi, crc) in
  let path_tbl = Map.set path_tbl ~key:m ~data:path in
  { cmis; path_tbl; virtual_env }

let add_virtual : t -> Ast_typed.signature -> t =
 fun { cmis; path_tbl; virtual_env } sig_ ->
  let virtual_env = sig_.sig_items @ virtual_env in
  { cmis; path_tbl; virtual_env }

let get_signatures : t -> sig_item list * (Module_var.t * signature) list =
 fun { cmis; path_tbl; virtual_env; _ } ->
  path_tbl
  |> Map.to_alist
  |> List.map ~f:(fun (mv, path) -> mv, (Tuple2.get1 (Map.find_exn cmis path)).sign)
  |> fun sigs -> virtual_env, sigs

let find_signature : t -> Module_var.t -> signature =
 fun { cmis; path_tbl; virtual_env; _ } mv ->
  let path = Map.find_exn path_tbl mv in
  Map.find_exn cmis path |> fun (cmi, crc) -> cmi.Cmi.sign

let of_init_sig : signature -> t = fun sig_ -> add_virtual empty sig_
