type crc = Md5.t [@@deriving bin_io]

type t =
  { path : Filename.t
  ; imports : (Filename.t * crc) list
  ; sign : Ast_typed.signature
  }
[@@deriving bin_io]

module Serialized = struct
  type contents =
    { magic : Bytes.t
    ; cmi : t
    }
  [@@deriving bin_io]

  type serialized =
    { contents : contents
    ; crc : crc
    }
  [@@deriving bin_io]

  let magic_number =
    (* bin_prot format prepends small Bytes.t with one length byte *)
    (* so resulting magic_number size will be 8 bytes *)
    Bytes.of_string "LIGOCMI"


  let compute_crc contents =
    contents |> Bin_prot.Writer.to_bytes bin_writer_contents |> Md5.digest_bytes


  let to_serialized t =
    let contents = { magic = magic_number; cmi = t } in
    let crc = compute_crc contents in
    { contents; crc }


  let make_path p =
    let open Filename in
    let dir = dirname p in
    let base = chop_extension (basename p) ^ ".cmi" in
    concat dir base


  let output t : crc =
    let open Out_channel in
    let serialized = to_serialized t in
    let cmi_path = make_path t.path in
    (try
       with_file ~binary:true cmi_path ~f:(fun oc ->
           serialized |> Bin_prot.Writer.to_bytes bin_writer_serialized |> output_bytes oc)
     with
    | Sys_error _ -> ());
    serialized.crc


  module Of_serialized = struct
    let read_file path =
      let cmi_path = make_path path in
      try
        In_channel.with_file ~binary:true cmi_path ~f:(fun ic ->
            let%bind.Option file_len = In_channel.length ic |> Int.of_int64 in
            let buf = Bytes.create file_len in
            let%map.Option _ = In_channel.really_input ic ~buf ~pos:0 ~len:file_len in
            buf)
      with
      | Sys_error _ -> None


    let guard b = if b then Some () else None

    let of_bytes bytes =
      try Some (Bin_prot.Reader.of_bytes bin_reader_serialized bytes) with
      | exn -> None


    let verify_magic magic = guard (Bytes.equal magic magic_number)

    let verify_consist cmi expected_md5 =
      let actual_md5 = compute_crc cmi in
      if Md5.equal actual_md5 expected_md5 then Some actual_md5 else None
  end

  let input path : (t * crc) option =
    let open Option.Let_syntax in
    let open Of_serialized in
    let%bind file = read_file path in
    let%bind { crc; contents } = of_bytes file in
    let { magic; cmi } = contents in
    let%bind _ = verify_magic magic in
    let%map crc = verify_consist contents crc in
    cmi, crc
end
