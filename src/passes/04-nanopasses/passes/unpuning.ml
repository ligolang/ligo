open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile =
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_pun_record fields ->
      let open Field in
      let fields =
        List.map fields ~f:(function
            | Complete x -> Complete x
            | Punned { wrap_content = label; location = loc } ->
              let pvar =
                p_var ~loc (Variable.of_input_var ~loc (Label.to_string label))
              in
              Complete (label, pvar))
      in
      p_pun_record ~loc fields
    | e -> make_p ~loc e
  in
  `Cata { idle_cata_pass with pattern }


let decompile =
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_pun_record fields ->
      let open Field in
      let fields =
        List.map fields ~f:(function
            | Complete (label, p) as c ->
              (match get_p_var p with
              | Some v ->
                if Variable.is_name v (Label.to_string label)
                then Punned (Location.wrap ~loc:(get_p_loc p) label)
                else c
              | None -> c)
            | Punned label -> Punned label)
      in
      p_pun_record ~loc fields
    | e -> make_p ~loc e
  in
  `Cata { idle_cata_pass with pattern }


let reduction ~raise =
  { Iter.defaults with
    pattern =
      (function
      | { wrap_content = P_pun_record lst; _ } when List.exists lst ~f:Field.is_pun ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  morph ~name:__MODULE__ ~compile ~decompile ~reduction_check:(reduction ~raise)
