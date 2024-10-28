module Location = Simple_utils.Location
type crc = Md5.t [@@deriving bin_io]

type t =
  { path : Filename.t
  ; imports : (Filename.t * crc) list
  ; sign : Ast_typed.signature
  }
[@@deriving bin_io]

let get_deps cmi = List.map ~f:fst cmi.imports

module Serialized = struct
  type serialized =
    { magic : Bytes.t
    ; cmi : t
    ; crc : crc
    }
  [@@deriving bin_io]

  let magic_number =
    (* bin_prot format prepends small Bytes.t with one length byte *)
    (* so resulting magic_number size will be 8 bytes *)
    Bytes.of_string "LIGOCMI"


  let is_cmi path =
    let suffix = Filename.check_suffix path ".cmi" in
    let magic_bytes =
      In_channel.with_file path ~f:(fun handle ->
          let read_bytes = Bytes.create 8 in
          let expected_bytes =
            let init = Bytes.create 8 in
            Bytes.set init 0 '\x07';
            Bytes.blit ~dst:init ~src:magic_number ~src_pos:0 ~dst_pos:1 ~len:7;
            init
          in
          ignore (In_channel.really_input handle ~buf:read_bytes ~pos:0 ~len:8);
          Bytes.equal read_bytes expected_bytes)
    in
    suffix && magic_bytes


  let compute_crc t = t |> Bin_prot.Writer.to_bytes bin_writer_t |> Md5.digest_bytes

  let to_serialized t =
    let magic = magic_number in
    let cmi = t in
    let crc = compute_crc t in
    { magic; cmi; crc }


  let make_path p =
    let open Filename in
    let dir = dirname p in
    let base = chop_extension (basename p) ^ ".cmi" in
    concat dir base


  let output t =
    let open Out_channel in
    let serialized = to_serialized t in
    let cmi_path = make_path t.path in
    try
      with_file ~binary:true cmi_path ~f:(fun oc ->
          serialized |> Bin_prot.Writer.to_bytes bin_writer_serialized |> output_bytes oc)
    with
    | Sys_error msg ->
      eprintf "%s\nFailed to write cmi file %s\n" msg cmi_path;
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

    let verify_consist cmi expected_md5 =
      let actual_md5 = compute_crc cmi in
      if Md5.equal actual_md5 expected_md5 then Some actual_md5 else None
  end

  let input path : (t * crc) option =
    let open Option.Let_syntax in
    let open Of_serialized in
    let%bind file = read_file path in
    let%bind { crc; cmi; magic } = of_bytes file in
    let%bind _ = verify_magic magic in
    let%map crc = verify_consist cmi crc in
    cmi, crc
end
