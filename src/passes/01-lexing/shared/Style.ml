(* Checking style based on the lexical context *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Unit   = LexerLib.Unit

(* LIGO dependencies *)

module type TOKEN = Token.S

(* Functor *)

module Make (Token : TOKEN) =
  struct
    (* Finding the next token in a list of lexical units *)

    let rec next_token markup = function
      `Token token :: _     -> Some (List.rev markup, token)
    | `Markup m    :: units -> next_token (m::markup) units
    | `Directive _ :: units -> next_token markup units
    | []                    -> None

    let next_token units = next_token [] units

    (* Errors *)

    type error =
      Odd_lengthed_bytes
    | Missing_break

    let error_to_string = function
      Odd_lengthed_bytes ->
        "The length of the byte sequence is an odd number.\n\
         Hint: Add or remove a digit."
    | Missing_break ->
        "Missing break.\n\
         Hint: Insert some space."

    let fail acc region error =
      let units = List.rev acc
      and msg = error_to_string error in
      Stdlib.Error (units, Region.{value=msg; region})

    (* Checking the style *)

    type units = Token.t Unit.t list

    type message = string Region.reg

    type result = (units, units * message) Stdlib.result

    let rec filter ~add_warning acc = function
      [] -> Ok (List.rev acc)
    | (`Markup _ | `Directive _ as u) :: units ->
         filter ~add_warning (u::acc) units
    | `Token token as t :: units ->
         let pos    = (Token.to_region token)#stop in
         let region = Region.make ~start:pos ~stop:pos in
         match next_token units with
           Some ([], next) ->
             let open Token in
             if   is_int token || is_string token
             then if   is_sym next || is_eof next
                  then filter ~add_warning (t::acc) units
                  else fail acc region Missing_break
             else
               if   is_bytes token
               then if   is_int next || is_hex next
                    then fail acc region Odd_lengthed_bytes
                    else
                      if   is_sym next || is_eof next
                      then filter ~add_warning (t::acc) units
                      else fail acc region Missing_break
               else filter ~add_warning (t::acc) units
         | _ -> filter ~add_warning (t::acc) units

    let filter ?print_passes ~add_warning units : result =
      let () =
        match print_passes with
          Some std ->
            Std.(add_line std.out "Running common unit  self-pass: Checking style.")
        | None -> ()
      in filter ~add_warning [] units
  end
