(* Injecting virtual tokens BEGIN ("indent") and END ("kw_end") *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Std       = Simple_utils.Std
module Core      = LexerLib.Core
module Markup    = LexerLib.Markup
module Directive = Preprocessor.Directive
module Unit      = LexerLib.Unit

(* Local dependencies *)

module Token = Lx_py_self_tokens.Token

(* Result type *)

type item    = Token.t Unit.t
type units   = item list
type message = string Region.reg
type result  = (units, units * message) Stdlib.result

(* Filter *)

let mk_begin token = `Token Token.(mk_BEGIN (to_region token))
let mk_end   token = `Token Token.(mk_END   (to_region token))

let rec unwind kwd_end token tab acc = function
  [] -> (* Top-level *)
    let region = Token.to_region token in
    let value  = "Invalid indentation. [1]" in
    Error (List.rev acc, Region.{value; region})
| level :: sublevels as levels ->
    if tab = level then
      Ok (levels, acc)
    else if tab > level then (* In-between indentation *)
      let region = Token.to_region token in
      let value  = "Invalid indentation. [2]" in
      Error (List.rev acc, Region.{value; region})
    else
      unwind kwd_end token tab (kwd_end :: acc) sublevels

let unwind token = unwind (mk_end token) token

let filter (units : units) : result =
  let open Markup
  in
  let rec aux levels acc = function
    (`Markup (Newline _) as nl) ::
    (`Markup (Space _) as from_top) ::
    (`Token Token.(Else _ | Elif _ as token) as t) :: more ->
      let region = Token.to_region token in
      (match levels with
         [] -> (* Wrong: Indentation from the top-level *)
           let value = "Invalid indentation. [4]" in
           Error (List.rev (from_top::nl::acc), Region.{value; region})
       | _ :: sublevels ->
           let kwd_end = `Token Token.(mk_END region) in
           aux sublevels (t :: kwd_end :: from_top :: nl :: acc) more)

  | (`Markup (Newline _) as nl) ::
    (`Markup (Space n) as from_top) ::
    (`Token _ as t) :: more ->
       let indent = `Token (Token.mk_INDENT n) in
       aux levels (from_top :: nl :: acc) (indent :: t :: more)

  | (`Token (Token.INDENT n) as indent) ::
    (`Token token as t) :: more ->
      let tab = n#payload in
      (match levels with
         [] -> (* Wrong: Indentation from the top-level *)
           let region = Token.to_region token in
           let value  = "Invalid indentation. [3]" in
           Error (List.rev (indent::acc), Region.{value; region})
       | level :: sublevels ->
           if tab = level then (* Same indentation *)
             aux levels (t :: indent :: acc) more
           else
             if tab > level then (* Indentation *)
               let acc = t :: indent :: mk_begin token :: acc
               in aux (tab :: levels) acc more
             else (* Deindentation (possibly erroneous) *)
               let acc = mk_end token :: acc in
               match unwind token tab acc sublevels with
                 Ok (levels, acc) -> aux levels (t::indent::acc) more
               | Error _ as err   -> err)

  | (`Markup (Newline _) as nl) ::
    (`Token (Token.EOF _) as eof) :: more -> (* more = [] *)
        aux levels (nl :: acc) (eof :: more)

  | (`Markup (Newline _) as nl) ::
    (`Token token as t) :: more ->
       let region = Token.to_region token in
       let top    = `Token Token.(mk_TOP_LEVEL region)
       in aux levels (nl :: acc) (top :: t :: more)

  | (`Token (Token.TOP_LEVEL _) as top) ::
    (`Token token as t) :: more ->
       (* Back to the top-level with a token *)
       let region  = Token.to_region token in
       let kwd_end = `Token Token.(mk_END region) in
       let folded acc level = if level = 0 then acc else kwd_end::acc in
       let acc = List.fold_left ~f:folded ~init:acc levels
       in aux [0] (t :: top :: acc) more

  | (`Markup (Newline _) as nl) ::
    (`Markup (LineCom _ | BlockCom _ as c) as m) :: more ->
       (* Back to the top-level with a comment *)
       let kwd_end = `Token Token.(mk_END (Markup.to_region c)) in
       let folded acc level = if level = 0 then acc else kwd_end::acc in
       let acc = List.fold_left ~f:folded ~init:(nl::acc) levels
       in aux [] (m :: acc) more

  | [`Token (Token.EOF _ as eof) as token] ->
      let kwd_end = `Token Token.(mk_END (Token.to_region eof)) in
      let folded acc level = if level = 0 then acc else kwd_end::acc in
      let acc = List.fold_left ~f:folded ~init:acc levels
      in aux levels (token :: acc) []

  | (`Markup (Newline _) as nl) ::
    (`Markup (Space   _) as first) ::
     `Markup (Tabs    n) :: _
  | (`Markup (Newline _) as nl) ::
    (`Markup (Tabs    _) as first) ::
     `Markup (Space   n) :: _ ->
       let region = n.Region.region in
       let value  = "TAB and space do not mix in indentation." in
       let msg    = Region.{value; region} in
       let prefix = List.rev (first :: nl :: acc)
       in Error (prefix, msg)

  | (`Markup (Newline _) as nl) ::
    (`Markup (Tabs _) as from_top) ::
    (`Token token) :: _ ->
      (* TODO: We do not check that a file uses only tabs or only
         spaces *)
       let acc    = from_top :: nl :: acc in
       let region = Token.to_region token in
       let value  = "TABs are not valid indentations (yet)." in
       Error (List.rev acc, Region.{value; region})

  | unit :: units -> aux levels (unit :: acc) units

  | [] -> Ok (List.rev acc)

  in
  (* Dealing with the start of the input file *)
  let units =
    match units with
      [] -> []

    | (`Markup (Space n) as sp) :: (`Token _ as t) :: more ->
        let indent = `Token (Token.mk_INDENT n)
        in sp :: indent :: t :: more

    | (`Token Token.EOF _) :: _ -> units

    | (`Token token as t) :: more ->
         let region = Token.to_region token in
         let top    = `Token Token.(mk_TOP_LEVEL region)
         in top :: t :: more

    | _ -> units
  in
  aux [] [] units (* TODO: When starting with "[sp|tab] token". *)

(* Exported *)

let filter ?print_passes ~add_warning:_ units : result =
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
                      "Running PyLIGO unit self-pass: \
                       Insert and suppress tabulations.")
    | None -> ()
  in filter units
