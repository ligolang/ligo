(**
   Datastructures used to interact with Ligo's package registry APIs
 *)

module Semver = Package_management_external_libs.Ligo_semver
module SMap = Caml.Map.Make (String)
open Package_management_shared
module RepositoryUrl = Repository_url
module Uri = Package_management_external_libs.Ligo_uri

let get_tarball_name ~name ~version = Format.sprintf "%s-%s.tgz" name version

type object_ = (string * string) list

let object__to_yojson o =
  `Assoc (List.fold o ~init:[] ~f:(fun kvs (k, v) -> (k, `String v) :: kvs))


let object__of_yojson = function
  | `Assoc kvs ->
    let f acc (k, v) =
      match acc, v with
      | Ok acc, `String s -> Ok ((k, s) :: acc)
      | _, _ -> Error "object__of_yojson failed: did not received String as expected"
    in
    let init = Ok [] in
    List.fold_left ~f ~init kvs
  | _ -> Error "object__of_yojson failed"


type sem_ver = string [@@deriving yojson]
type dist_tag = { latest : sem_ver } [@@deriving yojson]

module PackageStats = struct
  type t =
    { name : string
    ; version : Semver.t
    ; file_count : int
    ; unpacked_size : int
    ; packed_size : int
    ; tarball_name : string
    ; tarball_content : bytes
    ; sha1 : string
    ; sha512 : string
    ; integrity : string
    }

  let make ~name ~version ~unpacked_size ~packed_size ~fcount ~sha1 ~sha512 ~tarball =
    { name
    ; version
    ; file_count = fcount
    ; unpacked_size
    ; packed_size
    ; tarball_name = get_tarball_name ~name ~version:(Semver.to_string version)
    ; tarball_content = tarball
    ; sha1
    ; sha512
    ; integrity = Format.sprintf "sha512-%s" (Base64.encode_exn sha512)
    }
end

module Dist = struct
  type t =
    { integrity : string
    ; shasum : string
    ; tarball : Uri.t
    ; file_count : int [@key "fileCount"]
    ; unpacked_size : int [@key "unpackedSize"]
    }
  [@@deriving yojson]

  let make ~tarball ~package_stats =
    let PackageStats.{ sha1; file_count; unpacked_size; integrity; _ } = package_stats in
    { integrity; shasum = sha1; tarball; file_count; unpacked_size }
end

type author = { name : string } [@@deriving yojson]

module Version = struct
  type t =
    { name : string
    ; author : author
    ; main : string
    ; type_ : string [@key "type"]
    ; storage_fn : string option
    ; storage_arg : string option
    ; repository : RepositoryUrl.t
    ; version : Semver.t
    ; description : string
    ; scripts : object_
    ; dependencies : object_
    ; dev_dependencies : object_ [@key "devDependencies"]
    ; readme : string
    ; bugs : Bugs.t
    ; id : string [@key "_id"]
    ; dist : Dist.t
    }
  [@@deriving yojson { strict = false }]

  let make
      ~ligo_registry
      ~package_stats
      ~name
      ~version
      ~main
      ~scripts
      ~dependencies
      ~dev_dependencies
      ~description
      ~readme
      ~author
      ~type_
      ~repository
      ~storage_fn
      ~storage_arg
      ~bugs
    =
    let endpoint_uri =
      Format.sprintf "/-/api/%s/-/%s" name package_stats.PackageStats.tarball_name
    in
    let tarball = Uri.with_path ligo_registry endpoint_uri in
    { main
    ; name
    ; author = { name = author }
    ; type_
    ; storage_fn
    ; storage_arg
    ; repository
    ; version
    ; description
    ; scripts
    ; dependencies
    ; dev_dependencies
    ; readme
    ; bugs
    ; id = Format.sprintf "%s@%s" name (Semver.to_string version)
    ; dist = Dist.make ~tarball ~package_stats
    }
end

module Versions = struct
  type t = Version.t SMap.t

  let to_yojson t =
    let kvs = SMap.fold (fun k v xs -> (k, Version.to_yojson v) :: xs) t [] in
    `Assoc kvs


  let of_yojson = function
    | `Assoc kvs ->
      let f smap (k, v) =
        match smap, Version.of_yojson v with
        | Ok smap, Ok version -> Ok (SMap.add k version smap)
        | Ok _, Error e -> Error e
        | Error e, _ -> Error e
      in
      let init = Ok SMap.empty in
      kvs |> List.fold_left ~f ~init
    | _ -> Error "Versions.of_yojson failed: did not receive object as expected"
end

module type JSONABLE = sig
  type t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module type Attachment = sig
  type t

  include JSONABLE with type t := t
end

module GetAttachment = struct
  type t = { shasum : string } [@@deriving yojson]
end

module PutAttachment = struct
  type t =
    { content_type : string
    ; data : string
    ; length : int
    }
  [@@deriving yojson]
end

(* module PutAttachments = struct *)
(*   type t = PutAttachment.t SMap.t *)

(*   let of_yojson = function *)
(*     | `Assoc kvs -> *)
(*       let f acc (k, v) = *)
(*         match acc, PutAttachment.of_yojson v with *)
(*         | Ok acc, Ok v -> Ok (SMap.add k v acc) *)
(*         | Error e, _ -> Error e *)
(*         | _, Error e -> Error e *)
(*       in *)
(*       let init = Ok SMap.empty in *)
(*       kvs |> List.fold_left ~f ~init *)
(*     | _ -> Error "Attachment.of_yojson failed: did not receive object as expected" *)

(*   let to_yojson t = *)
(*     let kvs = SMap.fold (fun k v xs -> (k, PutAttachment.to_yojson v) :: xs) t [] in *)
(*     `Assoc kvs *)
(* end *)

module type Attachments = sig
  type attachment
  type t = attachment SMap.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t
end

module MakeAttachments (Attachment : Attachment) :
  Attachments with type attachment = Attachment.t = struct
  type attachment = Attachment.t
  type t = attachment SMap.t

  let of_yojson = function
    | `Assoc kvs ->
      let f acc (k, v) =
        match acc, Attachment.of_yojson v with
        | Ok acc, Ok v -> Ok (SMap.add k v acc)
        | Error e, _ -> Error e
        | _, Error e -> Error e
      in
      let init = Ok SMap.empty in
      kvs |> List.fold_left ~f ~init
    | _ -> Error "Attachment.of_yojson failed: did not receive object as expected"


  let to_yojson t =
    let kvs = SMap.fold (fun k v xs -> (k, Attachment.to_yojson v) :: xs) t [] in
    `Assoc kvs
end

(* This module represents the body of the HTTP request needed to publish a 
   package to LIGO registry *)
module MakeBody (Attachments : Attachments) = struct
  type t =
    { id : string [@key "_id"]
    ; name : string
    ; description : string
    ; dist_tags : dist_tag [@key "dist-tags"]
    ; versions : Versions.t
    ; readme : string
    ; revision : string option [@key "_rev"]
    ; attachments : Attachments.t [@key "_attachments"]
    }
  [@@deriving yojson { strict = false }]

  let make
      ~name
      ~dist_tags
      ~versions
      ~readme
      ~description
      ~revision
      ~(attachments : Attachments.t)
    =
    { id = name; name; description; dist_tags; versions; readme; revision; attachments }
end

module PutAttachments = struct
  include MakeAttachments (PutAttachment)

  let make ~(package_stats : PackageStats.t) =
    let gzipped_tarball = package_stats.PackageStats.tarball_content in
    SMap.add
      package_stats.PackageStats.tarball_name
      PutAttachment.
        { content_type = "application/octet-stream"
        ; data = Base64.encode_exn (Bytes.to_string gzipped_tarball)
        ; length = Bytes.length gzipped_tarball
        }
      SMap.empty
end

module PutBody = MakeBody (PutAttachments)
module GetAttachments = MakeAttachments (GetAttachment)
module GetBody = MakeBody (GetAttachments)

let show_stats stats =
  let PackageStats.
        { name
        ; version
        ; tarball_name
        ; packed_size
        ; unpacked_size
        ; sha1
        ; sha512
        ; file_count
        ; _
        }
    =
    stats
  in
  let human_readable_size size =
    let kilo = 1024.0 in
    let mega = kilo *. kilo in
    let giga = kilo *. mega in
    match float_of_int size with
    | size when Float.(size >= giga) -> Format.sprintf "%0.1f GB" (size /. giga)
    | size when Float.(size >= mega) -> Format.sprintf "%0.1f MB" (size /. mega)
    | size when Float.(size >= kilo) -> Format.sprintf "%0.1f kB" (size /. kilo)
    | size -> Format.sprintf "%d B" (int_of_float size)
  in
  let prefix = String.sub sha512 ~pos:0 ~len:13 in
  let suffix = String.sub sha512 ~pos:(String.length sha512 - 15) ~len:15 in
  let version = Semver.to_string version in
  let integrity = Format.sprintf "sha512-%s[...]%s" prefix suffix in
  let () = Format.printf "    publishing: %s@%s\n%!" name version in
  let () = Format.printf "    === Tarball Details ===\n%!" in
  let () = Format.printf "    name:          %s\n%!" name in
  let () = Format.printf "    version:       %s\n%!" version in
  let () = Format.printf "    filename:      %s\n%!" tarball_name in
  let () = Format.printf "    package size:  %s\n%!" (human_readable_size packed_size) in
  let () =
    Format.printf "    unpacked size: %s\n%!" (human_readable_size unpacked_size)
  in
  let () = Format.printf "    shasum:        %s\n%!" sha1 in
  let () = Format.printf "    integrity:     %s\n%!" integrity in
  let () = Format.printf "    total files:   %d\n%!" file_count in
  ()
