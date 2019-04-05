type 'a name = {
  content : 'a ;
  name : string ;
}

let make_name name content = { name ; content }
let destruct {name ; content} = (name, content)
let get_name x = x.name
let get_content x = x.content

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
  type n_operator = operator name

  type n_operators = n_operator list
  type level = n_operators name

  type hierarchy = level List.Ne.t
  type n_hierarchy = hierarchy name

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

  let get_op : n_operator -> operator = get_content

  let singleton type_name type_expression menhir_rule menhir_code =
    {type_name ; type_expression ; menhir_rule ; menhir_code}
  let language entry_point singletons hierarchies = {entry_point ; singletons ; hierarchies}

  let name_hierarchy name : n_operators list -> n_hierarchy = fun nopss ->
    let nopss' = List.Ne.of_list nopss in
    let name_i = fun i x -> make_name (name ^ "_" ^ (string_of_int i)) x in
    let levels : hierarchy = List.Ne.mapi name_i nopss' in
    make_name name levels

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
    let op : n_operator -> unit = fun x -> elements @@ get_content x in
    let level : level -> unit = fun l -> List.iter op @@ get_content l in
    let hierarchy : n_hierarchy -> unit = fun h -> List.Ne.iter level @@ get_content h in
    List.iter hierarchy l.hierarchies

  let associativity : language -> unit = fun l ->
    let level : level -> unit = fun l ->
      let aux : ([`Left | `Right | `None] as 'a) -> n_operator -> 'a = fun ass nop ->
        let op = get_content nop in
        match ass, List.hd op, List.nth op (List.length op - 1) with
        | _, Lower, Lower -> raise (Failure "double assoc")
        | `None, Lower, _ -> `Left
        | `None, _, Lower -> `Right
        | `Left, _, Lower -> raise (Failure "different assocs")
        | `Right, Lower, _ -> raise (Failure "different assocs")
        | m, _, _ -> m
      in
      let _assert = List.fold_left aux `None (get_content l) in
      ()
    in
    let hierarchy : n_hierarchy -> unit = fun h ->
      List.Ne.iter level (get_content h) in
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
      List.filter_map aux (get_content nop) in
    let type_element = fun ppf te -> fprintf ppf "%s" te in
    fprintf ppf "| %s of (%a)"
      (get_name nop)
      PP.(list_sep type_element (const " * ")) type_elements

  let n_hierarchy : _ -> O.n_hierarchy -> _ = fun ppf nh ->
    let levels = List.Ne.map get_content (get_content nh) in
    let nops = List.Ne.concat levels in
    let name = get_name nh in
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
    PP.(list_sep element (const " ")) ppf (get_content nop)

  let n_operator_code : _ -> O.n_operator -> _ = fun ppf nop ->
    let (name, elements) = destruct nop in
    let elements' =
      let i = ref 0 in
      let aux : O.element -> _ = fun e ->
        let r =
          match e with
          | Token _ -> i := !i - 1 ; None
          | List _ | Named _ | Current | Lower -> Some letters.(!i)
        in i := !i + 1 ; r
      in
      List.filter_map aux elements in
    fprintf ppf "%s (%a)" name PP.(list_sep string (const " , ")) elements'

  let n_operator prev_lvl_name cur_lvl_name : _ -> O.n_operator -> _ = fun ppf nop ->
    let name = get_name nop in
    fprintf ppf "%a@;| %a@;  @[<v>{@;  @[let loc = Location.make $startpos $endpos in@;Location.wrap ~loc %@%@ %a@]@;}@]" PP.comment name
      (n_operator_rule prev_lvl_name cur_lvl_name) nop
      n_operator_code nop

  let level prev_lvl_name : _ -> O.level -> _ = fun ppf l ->
    let name = get_name l in
    match prev_lvl_name with
    | "" -> (
        fprintf ppf "%s :@.  @[<v>%a@]" name
          PP.(list_sep (n_operator prev_lvl_name name) new_line) (get_content l) ;
      )
    | _ -> (
        fprintf ppf "%s :@.  @[<v>%a@;| %s { $1 }@]" name
          PP.(list_sep (n_operator prev_lvl_name name) new_line) (get_content l)
          prev_lvl_name
      )

  let n_hierarchy : _ -> O.n_hierarchy -> _ = fun ppf nh ->
    let name = get_name nh in
    fprintf ppf "%a@.%%inline %s : %s_0 { $1 }@.@;" PP.comment ("Top-level for " ^ name) name name;
    let (hd, tl) = List.Ne.rev @@ get_content nh in
    fprintf ppf "%a" (level "") hd ;
    let aux prev_name lvl =
      PP.new_lines 2 ppf () ;
      fprintf ppf "%a" (level prev_name) lvl ;
      get_name lvl
    in
    let _last_name = List.fold_left aux (get_name hd) tl in
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
  | `Left -> make_name name [Current ; Token t ; Lower]
  | `Right -> make_name name [Current ; Token t ; Lower]

let list = make_name "List" [
    O.Token Token.LIST ; List (`Lead, Token.LSQUARE, Token.SEMICOLON, Token.RSQUARE) ;
]

let let_in : O.n_operator = make_name "Let_in" [
    O.Token Token.LET ; Named "variable" ;
    O.Token Token.EQUAL ; Current ;
    O.Token Token.IN ; Current ;
]

let addition = infix "Addition" `Left Token.PLUS
let substraction = infix "Substraction" `Left Token.MINUS

let multiplication = infix "Multiplication" `Left Token.TIMES
let division = infix "Division" `Left Token.DIV

let arith_variable : O.n_operator = make_name "Arith_variable" [ O.Named "variable" ]

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

