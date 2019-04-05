module N = struct
  type 'a t = {
    content : 'a ;
    name : string ;
  }

  let name name content = { name ; content }
  let destruct {name ; content} = (name, content)
  let get_name x = x.name
  let get_content x = x.content
end

let list_filter_map f =
  let rec aux acc lst = match lst with
    | [] -> List.rev acc
    | hd :: tl -> aux (
        match f hd with
        | Some x -> x :: acc
        | None -> acc
      ) tl
  in
  aux []

module Ne_list = struct
  type 'a t = 'a * 'a list

  let of_list lst = List.(hd lst, tl lst)
  let iter f (hd, tl : _ t) = f hd ; List.iter f tl
  let map f (hd, tl : _ t) = f hd, List.map f tl
  let mapi f (hd, tl : _ t) =
    let lst = List.mapi f (hd::tl) in
    of_list lst
  let concat (hd, tl : _ t) = hd @ List.concat tl
  let rev (hd, tl : _ t) =
    match tl with
    | [] -> (hd, [])
    | lst ->
      let r = List.rev lst in
      (List.hd r, List.tl r @ [hd])
end

module PP = struct
  open Format
  let string : formatter -> string -> unit = fun ppf s -> fprintf ppf "%s" s
  let tag tag : formatter -> unit -> unit = fun ppf () -> fprintf ppf tag
  let new_line : formatter -> unit -> unit = tag "@;"
  let rec new_lines n ppf () =
    match n with
    | 0 -> new_line ppf ()
    | n -> new_line ppf () ; new_lines (n-1) ppf ()
  let const const : formatter -> unit -> unit = fun ppf () -> fprintf ppf "%s" const
  let comment : formatter -> string -> unit = fun ppf s -> fprintf ppf "(* %s *)" s
  let list_sep value separator = pp_print_list ~pp_sep:separator value
  let ne_list_sep value separator ppf (hd, tl) =
    value ppf hd ;
    separator ppf () ;
    pp_print_list ~pp_sep:separator value ppf tl
end

module Token = Lex.Token
type token = Token.token

module O = struct

  type element =
    | Named of string (* Named rule, like type_var *)
    | Token of token
    | List of ([`Trail | `Lead | `Separator] * token * token * token)
    | Current
    | Lower (* Lower precedence *)

  type operator = element list
  type n_operator = operator N.t

  type n_operators = n_operator list
  type level = n_operators N.t

  type hierarchy = level Ne_list.t
  type n_hierarchy = hierarchy N.t

  type singleton = {
    type_name : string ;
    type_expression : string ;
    menhir_rule : string ;
    menhir_code : string ;
  }

  type language = {
    entry_point : string ;
    singletons : singleton list ;
    hierarchies : n_hierarchy list ;
  }

  let get_op : n_operator -> operator = N.get_content

  let singleton type_name type_expression menhir_rule menhir_code =
    {type_name ; type_expression ; menhir_rule ; menhir_code}
  let language entry_point singletons hierarchies = {entry_point ; singletons ; hierarchies}

  let name_hierarchy name : n_operators list -> n_hierarchy = fun nopss ->
    let nopss' = Ne_list.of_list nopss in
    let name_i = fun i x -> N.name (name ^ "_" ^ (string_of_int i)) x in
    let levels : hierarchy = Ne_list.mapi name_i nopss' in
    N.name name levels

end

module Check = struct
  open O

  let well_formed : language -> unit = fun l ->
    let elements : element list -> unit = fun es ->
      let rec aux = fun es ->
        match es with
        | [] -> ()
        | [ _ ] -> ()
        | (List _ | Named _ | Current | Lower) :: (List _ | Named _ | Current | Lower) :: _ ->
          raise (Failure "two non-token separated ops in a row")
        | _ :: tl -> aux tl
      in
      (if (List.length es < 2) then raise (Failure "operator is too short")) ;
      aux es in
    let op : n_operator -> unit = fun x -> elements @@ N.get_content x in
    let level : level -> unit = fun l -> List.iter op @@ N.get_content l in
    let hierarchy : n_hierarchy -> unit = fun h -> Ne_list.iter level @@ N.get_content h in
    List.iter hierarchy l.hierarchies

  let associativity : language -> unit = fun l ->
    let level : level -> unit = fun l ->
      let aux : ([`Left | `Right | `None] as 'a) -> n_operator -> 'a = fun ass nop ->
        let op = N.get_content nop in
        match ass, List.hd op, List.nth op (List.length op - 1) with
        | _, Lower, Lower -> raise (Failure "double assoc")
        | `None, Lower, _ -> `Left
        | `None, _, Lower -> `Right
        | `Left, _, Lower -> raise (Failure "different assocs")
        | `Right, Lower, _ -> raise (Failure "different assocs")
        | m, _, _ -> m
      in
      let _assert = List.fold_left aux `None (N.get_content l) in
      ()
    in
    let hierarchy : n_hierarchy -> unit = fun h ->
      Ne_list.iter level (N.get_content h) in
    List.iter hierarchy l.hierarchies

end


module Print_AST = struct
  open Format

  let singleton : _ -> O.singleton -> _ = fun ppf s ->
    fprintf ppf "type %s = %s" s.type_name s.type_expression

  let n_operator level_name : _ -> O.n_operator -> _ = fun ppf nop ->
    let type_elements =
      let aux : O.element -> string option = fun e ->
        match e with
        | Named s -> Some (s ^ " Location.wrap")
        | List _ -> Some ("(" ^ level_name ^ " Location.wrap list)")
        | Token _ -> None
        | Current | Lower -> Some (level_name ^ " Location.wrap") in
      list_filter_map aux (N.get_content nop) in
    let type_element = fun ppf te -> fprintf ppf "%s" te in
    fprintf ppf "| %s of (%a)"
      (N.get_name nop)
      PP.(list_sep type_element (const " * ")) type_elements

  let n_hierarchy : _ -> O.n_hierarchy -> _ = fun ppf nh ->
    let levels = Ne_list.map N.get_content (N.get_content nh) in
    let nops = Ne_list.concat levels in
    let name = N.get_name nh in
    fprintf ppf "type %s =@.@[%a@]"
      name
      PP.(list_sep (n_operator name) new_line) nops

  let language : _ -> O.language -> _ = fun ppf l ->
    fprintf ppf "%a@.@." PP.comment "Language" ;
    fprintf ppf "  %a@.%a@.@." PP.comment "Singletons" PP.(list_sep singleton new_line) l.singletons ;
    fprintf ppf "  %a@.%a@." PP.comment "Hierarchies" PP.(list_sep n_hierarchy (new_lines 2)) l.hierarchies ;
    fprintf ppf "  %a@.type entry_point = %s@.@." PP.comment "Entry point" l.entry_point ;
    ()
end

module Print_Grammar = struct
  open Format

  let singleton : _ -> O.singleton -> _ = fun ppf s ->
    fprintf ppf "%s : %s@.  @[<v>{@;  @[<v>let loc = Location.make $startpos $endpos in@;Location.wrap ~loc %s@]@;}@;@]"
      s.type_name s.menhir_rule s.menhir_code

  let letters = [| "a" ; "b" ; "c" ; "d" ; "e" ; "f" ; "g" ; "h" ; "i" ; "j" |]

  let n_operator_rule prev_lvl_name cur_lvl_name : _ -> O.n_operator -> _ = fun ppf nop ->
    let i = ref 0 in
    let element : _ -> O.element -> _ = fun ppf element ->
      (match element with
       | Token t -> i := !i - 1 ; PP.string ppf @@ Token.to_string t
       | List (mode, beg, sep, end_) ->
         fprintf ppf "%s %s = %s(%s, %s) %s"
           (Token.to_string beg)
           letters.(!i)
           (match mode with | `Lead -> "lead_list" | `Trail -> "trail_list" | `Separator -> "separated_list")
           (Token.to_string sep)
           cur_lvl_name
           (Token.to_string end_)
       | Named n ->
           fprintf ppf "%s = %s" letters.(!i) n
       | Current ->
           fprintf ppf "%s = %s" letters.(!i) cur_lvl_name
       | Lower ->
         fprintf ppf "%s = %s" letters.(!i) prev_lvl_name
      ) ;
      i := !i + 1
    in
    PP.(list_sep element (const " ")) ppf (N.get_content nop)

  let n_operator_code : _ -> O.n_operator -> _ = fun ppf nop ->
    let (name, elements) = N.destruct nop in
    let elements' =
      let i = ref 0 in
      let aux : O.element -> _ = fun e ->
        let r =
          match e with
          | Token _ -> i := !i - 1 ; None
          | List _ | Named _ | Current | Lower -> Some letters.(!i)
        in i := !i + 1 ; r
      in
      list_filter_map aux elements in
    fprintf ppf "%s (%a)" name PP.(list_sep string (const " , ")) elements'

  let n_operator prev_lvl_name cur_lvl_name : _ -> O.n_operator -> _ = fun ppf nop ->
    let name = N.get_name nop in
    fprintf ppf "%a@;| %a@;  @[<v>{@;  @[let loc = Location.make $startpos $endpos in@;Location.wrap ~loc %@%@ %a@]@;}@]" PP.comment name
      (n_operator_rule prev_lvl_name cur_lvl_name) nop
      n_operator_code nop

  let level prev_lvl_name : _ -> O.level -> _ = fun ppf l ->
    let name = N.get_name l in
    match prev_lvl_name with
    | "" -> (
        fprintf ppf "%s :@.  @[<v>%a@]" name
          PP.(list_sep (n_operator prev_lvl_name name) new_line) (N.get_content l) ;
      )
    | _ -> (
        fprintf ppf "%s :@.  @[<v>%a@;| %s { $1 }@]" name
          PP.(list_sep (n_operator prev_lvl_name name) new_line) (N.get_content l)
          prev_lvl_name
      )

  let n_hierarchy : _ -> O.n_hierarchy -> _ = fun ppf nh ->
    let name = N.get_name nh in
    fprintf ppf "%a@.%%inline %s : %s_0 { $1 }@.@;" PP.comment ("Top-level for " ^ name) name name;
    let (hd, tl) = Ne_list.rev @@ N.get_content nh in
    fprintf ppf "%a" (level "") hd ;
    let aux prev_name lvl =
      PP.new_lines 2 ppf () ;
      fprintf ppf "%a" (level prev_name) lvl ;
      N.get_name lvl
    in
    let _last_name = List.fold_left aux (N.get_name hd) tl in
    ()

  let language : _ -> O.language -> _ = fun ppf l ->
    fprintf ppf "%a@.@." PP.comment "Generated Language" ;
    fprintf ppf "entry_point : %s EOF { $1 }@.@." l.entry_point ;
    fprintf ppf "%a@.@." PP.comment "Singletons" ;
    fprintf ppf "@[%a@]@.@." (PP.list_sep singleton PP.new_line) l.singletons ;
    fprintf ppf "%a@.@." PP.comment "Hierarchies" ;
    fprintf ppf "@[%a@]" (PP.list_sep n_hierarchy PP.new_line) l.hierarchies ;

end

let variable = O.singleton "variable" "string" "NAME" "$1"

let infix : string -> [`Left | `Right] -> token -> O.n_operator = fun name assoc t ->
  let open O in
  match assoc with
  | `Left -> N.name name [Current ; Token t ; Lower]
  | `Right -> N.name name [Current ; Token t ; Lower]

let list = N.name "List" [
    O.Token Token.LIST ; List (`Lead, Token.LSQUARE, Token.SEMICOLON, Token.RSQUARE) ;
]

let let_in : O.n_operator = N.name "Let_in" [
    O.Token Token.LET ; Named "variable" ;
    O.Token Token.EQUAL ; Current ;
    O.Token Token.IN ; Current ;
]

let addition = infix "Addition" `Left Token.PLUS
let substraction = infix "Substraction" `Left Token.MINUS

let multiplication = infix "Multiplication" `Left Token.TIMES
let division = infix "Division" `Left Token.DIV

let arith_variable : O.n_operator = N.name "Arith_variable" [ O.Named "variable" ]

let arith = O.name_hierarchy "arith" [
    [let_in] ;
    [addition ; substraction] ;
    [multiplication ; division] ;
    [list] ;
    [arith_variable] ;
]

let language = O.language "arith" [variable] [arith]

let () =
  let argn = Array.length Sys.argv in
  if argn = 1 then exit 1 ;
  let arg = Sys.argv.(1) in
  match arg with
  | "parser" -> (
    Format.printf "%a@.%a\n" PP.comment "Full Grammar" Print_Grammar.language language
  )
  | "ast" -> (
    Format.printf "%a@.%a\n" PP.comment "AST" Print_AST.language language
  )
  | _ -> exit 1

