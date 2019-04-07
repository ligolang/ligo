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

  type 'a list_element = [`Trail | `Lead | `Separator] * token * 'a

  type basic_rhs_element = [
    | `Named of string
    | `Token of token
  ]

  type rhs_element = [
    | basic_rhs_element
    | `List of string list_element
  ]

  type rhs = rhs_element list
  type rule = rhs list name

  type manual_rule_content = {
    menhir_codes : string list ;
    ast_code : string ;
  }
  type manual_rule = manual_rule_content name

  type singleton =
    | Manual of manual_rule
    | Generated of rule

  type name_element = [
    | `Named of string
    | `Current
    | `Lower
  ]

  type element = [
    | `Named of string
    | `Token of token
    | `List of name_element list_element
    | `Current
    | `Lower
  ]


  type operator = element list
  type n_operator = operator name

  type n_operators = n_operator list
  type level = n_operators name
  type level_list = level list
  type levels = level List.Ne.t

  type hierarchy = {
    levels : levels ;
    auxiliary_rules : rule list ;
  }
  type n_hierarchy = hierarchy name
  let make_hierarchy levels auxiliary_rules : hierarchy = { levels ; auxiliary_rules }

  type language = {
    entry_point : string ;
    singletons : singleton list ;
    hierarchies : n_hierarchy list ;
  }

  let get_op : n_operator -> operator = get_content

  let manual_singleton name menhir_codes ast_code : singleton = Manual (make_name name {menhir_codes ; ast_code})
  let rule_singleton rule : singleton = Generated rule
  let language entry_point singletons hierarchies = {entry_point ; singletons ; hierarchies}


  let name_hierarchy name : n_operators list -> rule list -> n_hierarchy = fun nopss rules ->
    let nopss' = List.Ne.of_list nopss in
    let name_i = fun i x -> make_name (name ^ "_" ^ (string_of_int i)) x in
    let levels : levels = List.Ne.mapi name_i nopss' in
    make_name name @@ make_hierarchy levels rules

end

module Check = struct
  open O

  let well_formed : language -> unit = fun l ->
    let elements : element list -> unit = fun es ->
      let rec aux = fun es ->
        match es with
        | [] -> ()
        | [ _ ] -> ()
        | (`List _ | `Named _ | `Current | `Lower) :: (`List _ | `Named _ | `Current | `Lower) :: _ ->
          raise (Failure "two non-token separated ops in a row")
        | _ :: tl -> aux tl
      in
      (if (List.length es < 2) then raise (Failure "operator is too short")) ;
      aux es in
    let op : n_operator -> unit = fun x -> elements @@ get_content x in
    let level : level -> unit = fun l -> List.iter op @@ get_content l in
    let hierarchy : n_hierarchy -> unit = fun h -> List.Ne.iter level @@ h.content.levels in
    List.iter hierarchy l.hierarchies

  let associativity : language -> unit = fun l ->
    let level : level -> unit = fun l ->
      let aux : ([`Left | `Right | `None] as 'a) -> n_operator -> 'a = fun ass nop ->
        let op = get_content nop in
        match ass, List.hd op, List.nth op (List.length op - 1) with
        | _, `Lower, `Lower -> raise (Failure "double assoc")
        | `None, `Lower, _ -> `Left
        | `None, _, `Lower -> `Right
        | `Left, _, `Lower -> raise (Failure "different assocs")
        | `Right, `Lower, _ -> raise (Failure "different assocs")
        | m, _, _ -> m
      in
      let _assert = List.fold_left aux `None (get_content l) in
      ()
    in
    let hierarchy : n_hierarchy -> unit = fun h ->
      List.Ne.iter level h.content.levels in
    List.iter hierarchy l.hierarchies

end


module Print_AST = struct
  open Format

  let manual_rule : _ -> O.manual_rule -> _ = fun ppf mr ->
    fprintf ppf "%s = %s" mr.name mr.content.ast_code

  let generated_rule : _ -> O.rule -> _ = fun ppf gr ->
    let aux : _ -> O.rhs -> _ = fun ppf rhs ->
      let type_elements =
        let aux : O.rhs_element -> string option = fun e ->
          match e with
          | `Named s -> Some (s ^ " Location.wrap")
          | `List (_, _, s) -> Some ("(" ^ s ^ " Location.wrap list)")
          | `Token _ -> None
        in
        List.filter_map aux rhs in
      let type_element = fun ppf te -> fprintf ppf "%s" te in
      fprintf ppf "| %s of (%a)"
        (String.capitalize_ascii gr.name)
        PP.(list_sep type_element (const " * ")) type_elements
    in
    fprintf ppf "%s = @.  @[<v>%a@]" gr.name
      PP.(list_sep aux new_line) gr.content

  let singleton : _ -> O.singleton -> _ = fun ppf s ->
    match s with
    | Manual s -> manual_rule ppf s
    | Generated s -> generated_rule ppf s

  let singletons : _ -> O.singleton list -> _ = fun ppf ss ->
    match ss with
    | [] -> ()
    | hd :: tl ->
        fprintf ppf "%a\n" (PP.prepend "type " singleton) hd ;
        fprintf ppf "%a" PP.(list_sep (prepend "and " singleton) (const "\n")) tl

  let n_operator level_name : _ -> O.n_operator -> _ = fun ppf nop ->
    let type_elements =
      let aux : O.element -> string option = fun e ->
        match e with
        | `Named s -> Some (s ^ " Location.wrap")
        | `List _ -> Some ("(" ^ level_name ^ " Location.wrap list)")
        | `Token _ -> None
        | `Current | `Lower -> Some (level_name ^ " Location.wrap") in
      List.filter_map aux (get_content nop) in
    let type_element = fun ppf te -> fprintf ppf "%s" te in
    fprintf ppf "| %s of (%a)"
      (get_name nop)
      PP.(list_sep type_element (const " * ")) type_elements

  let n_hierarchy t : _ -> O.n_hierarchy -> _ = fun ppf nh ->
    let levels = List.Ne.map get_content ((get_content nh).levels) in
    let nops = List.Ne.concat levels in
    let name = get_name nh in
    fprintf ppf "%s %s =@.@[%a@]" t
      name
      PP.(list_sep (n_operator name) new_line) nops

  let n_hierarchies (first:bool) : _ -> O.n_hierarchy list -> _ = fun ppf ss ->
    match ss with
    | [] -> ()
    | hd :: tl ->
        fprintf ppf "%a\n" (n_hierarchy (if first then "type" else "and")) hd ;
        fprintf ppf "%a" PP.(list_sep (prepend "and " (n_hierarchy "and")) (const "\n")) tl

  let language : _ -> O.language -> _ = fun ppf l ->
    fprintf ppf "%a@.@." PP.comment "Language" ;
    let first = List.length l.singletons = 0 in
    fprintf ppf "  %a@.%a@.@." PP.comment "Singletons" singletons l.singletons ;
    fprintf ppf "  %a@.%a@." PP.comment "Hierarchies" (n_hierarchies first) l.hierarchies ;
    fprintf ppf "  %a@.type entry_point = %s Location.wrap@.@." PP.comment "Entry point" l.entry_point ;
    ()
end

module Print_Grammar = struct
  open Format

  let letters = [| "a" ; "b" ; "c" ; "d" ; "e" ; "f" ; "g" ; "h" ; "i" ; "j" |]


  let manual_rule : _ -> O.manual_rule -> _ = fun ppf mr ->
    let {name;content} = mr in
    fprintf ppf "%s:@.  @[<v>%a@]" name (PP.list_sep PP.string PP.new_line) content.menhir_codes

  let generated_rule : _ -> O.rule -> _ = fun ppf gr ->
    let aux_rule : _ -> O.rhs -> _ = fun ppf rhs ->
      let i = ref 0 in
      let aux : _ -> O.rhs_element -> _ = fun ppf e ->
          (match e with
          | `Named s -> fprintf ppf "%s = wrap(%s)" letters.(!i) s
          | `List (mode, sep, s) ->
              fprintf ppf "%s = %s(%s, wrap(%s))"
                letters.(!i)
                (match mode with | `Lead -> "lead_list" | `Trail -> "trail_list" | `Separator -> "separated_list")
                (Token.to_string sep)
                s
          | `Token t -> i := !i - 1 ; PP.string ppf @@ Token.to_string t) ;
          i := !i + 1
        in
        fprintf ppf "%a" PP.(list_sep aux (const " ")) rhs in
    let aux_code : _ -> O.rhs -> _ = fun ppf rhs ->
      let i = ref 0 in
      let aux : O.rhs_element -> _ = fun e ->
          let s = (match e with
          | `Named _ | `List _ -> Some (letters.(!i))
          | `Token _ -> i := !i - 1 ; None) in
          i := !i + 1 ; s
      in
      let content = List.filter_map aux rhs in
      fprintf ppf "%s (%a)" (String.capitalize_ascii gr.name) PP.(list_sep string (const " , ")) content
    in
    let aux : _ -> O.rhs -> _ = fun ppf rhs ->
      fprintf ppf "| %a { %a }"
        aux_rule rhs
        aux_code rhs in
    fprintf ppf "%s:@.%a" gr.name PP.(list_sep aux (const "\n")) gr.content

  let singleton : _ -> O.singleton -> _ = fun ppf s ->
    match s with
    | Manual s -> manual_rule ppf s
    | Generated s -> generated_rule ppf s


  let n_operator_rule prev_lvl_name cur_lvl_name : _ -> O.n_operator -> _ = fun ppf nop ->
    let i = ref 0 in
    let element : _ -> O.element -> _ = fun ppf element ->
      (match element with
       | `Token t -> i := !i - 1 ; PP.string ppf @@ Token.to_string t
       | `List (mode, sep, content) ->
         fprintf ppf "%s = %s(%s, wrap(%s))"
           letters.(!i)
           (match mode with | `Lead -> "lead_list" | `Trail -> "trail_list" | `Separator -> "separated_list")
           (Token.to_string sep)
           (match content with | `Lower -> prev_lvl_name | `Named s -> s | `Current -> cur_lvl_name)
       | `Named n ->
           fprintf ppf "%s = wrap(%s)" letters.(!i) n
       | `Current ->
           fprintf ppf "%s = wrap(%s)" letters.(!i) cur_lvl_name
       | `Lower ->
         fprintf ppf "%s = wrap(%s)" letters.(!i) prev_lvl_name
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
          | `Token _ -> i := !i - 1 ; None
          | `List _ | `Named _ | `Current | `Lower -> Some letters.(!i)
        in i := !i + 1 ; r
      in
      List.filter_map aux elements in
    fprintf ppf "%s (%a)" name PP.(list_sep string (const " , ")) elements'

  let n_operator prev_lvl_name cur_lvl_name : _ -> O.n_operator -> _ = fun ppf nop ->
    let name = get_name nop in
    fprintf ppf "%a@;| %a { %a }" PP.comment name
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
    let (hd, tl) = List.Ne.rev (get_content nh).levels in
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
    fprintf ppf "entry_point : wrap(%s) EOF { $1 }@.@." l.entry_point ;
    fprintf ppf "%a@.@." PP.comment "Singletons" ;
    fprintf ppf "@[%a@]@.@." (PP.list_sep singleton PP.new_line) l.singletons ;
    fprintf ppf "%a@.@." PP.comment "Hierarchies" ;
    fprintf ppf "@[%a@]" (PP.list_sep n_hierarchy PP.new_line) l.hierarchies ;

end


let infix : string -> [`Left | `Right] -> token -> O.n_operator = fun name assoc t ->
  match assoc with
  | `Left -> make_name name [`Current ; `Token t ; `Lower]
  | `Right -> make_name name [`Current ; `Token t ; `Lower]

let expression_name = "expression"
let type_expression_name = "type_expression"
let program_name = "program"
let variable_name = "variable"

let variable = O.manual_singleton variable_name ["| NAME { $1 }"] "string"


module Expression = struct

  open Token

  let list : O.n_operator = make_name "List" [
      `Token LIST ; `Token LSQUARE ; `List (`Lead, SEMICOLON, `Current) ; `Token RSQUARE ;
    ]

  let let_in : O.n_operator = make_name "Let_in" [
      `Token Token.LET ; `Named "variable" ;
      `Token Token.EQUAL ; `Current ;
      `Token Token.IN ; `Current ;
    ]

  let addition = infix "Addition" `Left Token.PLUS
  let substraction = infix "Substraction" `Left Token.MINUS

  let multiplication = infix "Multiplication" `Left Token.TIMES
  let division = infix "Division" `Left Token.DIV

  let arith_variable : O.n_operator = make_name "Arith_variable" [ `Named "variable" ]

  let arith = O.name_hierarchy "expression" [
      [let_in] ;
      [addition ; substraction] ;
      [multiplication ; division] ;
      [list] ;
      [arith_variable] ;
    ] []

end

module Program = struct

  open Token

  let statement_name = "statement"

  let program : O.rule = make_name program_name [[
      `List (`Trail, SEMICOLON, statement_name)
    ]]

  let statement : O.rule = make_name statement_name [
      [`Token CONST ; `Named variable_name ; `Token EQUAL ; `Named expression_name]
    ]

  let singletons = List.map O.rule_singleton [program ; statement]

end

let language = O.language program_name (variable :: Program.singletons) [Expression.arith]

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

