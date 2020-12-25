(* Vendor dependencies *)

(*open Simple_utils.Display*)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Internal dependencies *)


(* Errors *)

type preproc_error = [
  | `Preproc_generic of string Region.reg
  ]

let stage = "preproc"

let generic reg = `Preproc_generic reg

let error_ppformat :
      display_format:(string Display.display_format) ->
      Format.formatter ->
      preproc_error ->
      unit =
  fun ~display_format f a ->
  match display_format with
    Human_readable | Dev -> (
      match a with
        `Preproc_generic reg ->
           Snippet.pp_lift f reg.Region.region;
           Format.pp_print_string f reg.Region.value
    )

let error_jsonformat : preproc_error -> Yojson.Safe.t =
  fun error ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error");
      ("stage",  `String stage);
      ("content", content )] in
  match error with
    `Preproc_generic reg ->
       let loc =
         Format.asprintf "%a" Location.pp_lift @@ reg.Region.region in
       let content = `Assoc [
         ("message",  `String reg.Region.value);
         ("location", `String loc)]
    in json_error ~stage ~content
