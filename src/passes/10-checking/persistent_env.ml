open Ast_typed
module Module_map = Map.Make (Module_var)

type t =
  { cmis : (Cmi.t * Cmi.crc) Filename.Map.t
  ; path_tbl : Filename.t Module_map.t
  ; virtual_env : Ast_typed.sig_item list
  }

let empty : t =
  { cmis = Filename.Map.empty; path_tbl = Module_map.empty; virtual_env = [] }

let verify_deps : t -> (Filename.t * Cmi.crc) list -> bool =
 fun { cmis; _ } deps ->
  List.for_all
    ~f:(fun (filename, crc) ->
      match Map.find cmis filename with
      | None -> false
      | Some (_, crc') -> Md5.equal crc crc')
    deps

type key = Module of Module_var.t | File of Filename.t

let find_cmi : t -> key -> Cmi.t * Cmi.crc =
 fun { cmis; path_tbl; _ } key -> match key with
 | Module mv -> let path = Map.find_exn path_tbl mv in Map.find_exn cmis path
 | File f -> Map.find_exn cmis f

let compute_cmi : t -> Filename.t -> Filename.t list -> Ast_typed.signature -> Cmi.t * Cmi.crc =
 fun ({ cmis; _ } as env) path imports sign ->
  let imports =
    List.map ~f:(fun filename -> filename, Tuple2.get2 @@ find_cmi env (File filename)) imports
  in
  let cmi = Cmi.{ path; sign; imports } in
  let crc = Cmi.Serialized.compute_crc cmi in
  cmi, crc

let add_signature
    : t -> Module_var.t -> Filename.t -> Filename.t list -> Ast_typed.signature -> t
  =
 fun ({ cmis; path_tbl; virtual_env } as env) m path imports sign ->
  let cmi, crc = compute_cmi env path imports sign in
  let cmis = Map.set cmis ~key:path ~data:(cmi, crc) in
  let path_tbl = Map.set path_tbl ~key:m ~data:path in
  { cmis; path_tbl; virtual_env }

let add_cmi : t -> Module_var.t -> Filename.t -> Cmi.t -> Cmi.crc -> t =
 fun { cmis; path_tbl; virtual_env } m path cmi crc ->
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
