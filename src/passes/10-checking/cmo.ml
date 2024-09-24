type t =
  { path : Filename.t
  ; impl : Ast_typed.module_
  }
[@@deriving bin_io]

module Serialized = struct
  type serialized =
    { magic : Bytes.t
    ; cmo : t
    ; crc : Cmi.crc
    }
  [@@deriving bin_io]

  let magic_number = Bytes.of_string "LIGOCMO"
  let compute_crc t = t |> Bin_prot.Writer.to_bytes bin_writer_t |> Md5.digest_bytes

  let to_serialized t =
    let magic = magic_number in
    let cmo = t in
    let crc = compute_crc t in
    { magic; cmo; crc }


  let make_path p =
    let open Filename in
    let dir = dirname p in
    let base = chop_extension (basename p) ^ ".cmo" in
    concat dir base


  let output t =
    let open Out_channel in
    let serialized = to_serialized t in
    let cmo_path = make_path t.path in
    try
      with_file ~binary:true cmo_path ~f:(fun oc ->
          serialized |> Bin_prot.Writer.to_bytes bin_writer_serialized |> output_bytes oc)
    with
    | Sys_error msg ->
      eprintf "%s\nFailed to write cmo file %s\n" msg cmo_path;
      exit 1


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

    let verify_consist cmo expected_md5 =
      let actual_md5 = compute_crc cmo in
      guard (Md5.equal actual_md5 expected_md5)
  end

  let input path : t option =
    let open Option.Let_syntax in
    let open Of_serialized in
    let%bind file = read_file path in
    let%bind { crc; magic; cmo } = of_bytes file in
    let%bind _ = verify_magic magic in
    let%map _ = verify_consist cmo crc in
    cmo
end
