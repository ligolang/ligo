(** Memory representation of the lock file. Contains parsing/serialising logic *)

open Package_management_alpha_shared
module Semver = Package_management_external_libs.Ligo_semver
module NameVersion = Name_version
module NameVersionMap = Caml.Map.Make (NameVersion)

module Source = struct
  type dist = string [@@deriving yojson]

  type default =
    { type_ : string [@key "type"]
    ; path : string
    ; manifest : string
    }
  [@@deriving yojson]

  type node =
    { type_ : string [@key "type"]
    ; source : dist list
    }
  [@@deriving yojson]

  type t =
    | Node of node
    | Default of default
  [@@deriving yojson]

  let to_yojson = function
    | Node node -> node_to_yojson node
    | Default node -> default_to_yojson node


  let trim_quotes : string -> string =
   fun s -> String.strip s ~drop:(fun ch -> Char.equal ch '\"')


  let of_yojson y =
    let ( let+ ) = Caml.Result.bind in
    match Yojson.Safe.(Util.member "type" y |> to_string |> trim_quotes) with
    | exception Yojson.Safe.Util.Type_error _ ->
      Error "Couldn't parse lock file json while parsing 'source' field"
    | "link-dev" ->
      let+ default = default_of_yojson y in
      Ok (Default default)
    | _ ->
      let+ node = node_of_yojson y in
      Ok (Node node)
end

module Node = struct
  type dependency = string * string [@@deriving yojson]
  type source = Source.t [@@deriving yojson]

  (* In default node version is a string and not a semver equivalent string *)
  type version =
    | NodeVersion of Semver.t
    | DefaultVersion of string
  [@@deriving yojson]

  let version_to_yojson = function
    | NodeVersion semver -> `String (Semver.to_string semver)
    | DefaultVersion version -> `String version


  let version_of_yojson = function
    | `String v ->
      (match v with
      | "link-dev:./package.json" -> Ok (DefaultVersion v)
      | v ->
        (match Semver.of_string v with
        | Some semver -> Ok (NodeVersion semver)
        | None -> Error ("Failed to parse semver" ^ v ^ " while parsing lockfile")))
    | _ -> Error "Unexpected json value. Expected string"


  type t =
    { id : string
    ; name : string
    ; version : version
    ; source : source
    ; overrides : string list
          (* TODO : Not sure what overrides would be, it's never used but esy generates this for every node element in the lock file. Needs proper type *)
    ; dependencies : NameVersion.t list
    ; dev_dependencies : NameVersion.t list [@key "devDependencies"]
    }
  [@@deriving yojson]

  let compare a b =
    match String.compare a.name b.name with
    | 0 ->
      (match a.version, b.version with
      | NodeVersion a, NodeVersion b -> Semver.compare a b
      | DefaultVersion a, DefaultVersion b -> String.compare a b
      (* In case of Node _, Default _ or Default _, Node _ what comes first is small by default *)
      | _, _ -> -1)
    | n -> n
end

module NodeMap = struct
  include NameVersionMap

  type t = Node.t NameVersionMap.t

  let of_yojson = function
    | `Assoc kvs ->
      let init = Ok NameVersionMap.empty in
      let f acc (k, v) =
        match acc, Node.of_yojson v, NameVersion.of_string k with
        | Ok acc_v, Ok v, Ok k -> Result.return @@ NameVersionMap.add k v acc_v
        | _, _, Error (`Msg e) -> Error e
        | _, Error e, _ -> Error e
        | Error e, _, _ -> Error e
      in
      List.fold_left ~f ~init kvs
    | _ -> Error "Invalid NodeMap json"


  let to_yojson v =
    let kvs = NameVersionMap.bindings v in
    let assoc v = `Assoc v in
    let f (k, v) = k |> NameVersion.to_string, Node.to_yojson v in
    assoc @@ List.map ~f kvs
end

type t =
  { checksum : string
  ; root : string
  ; node : NodeMap.t
  }
[@@deriving yojson]
