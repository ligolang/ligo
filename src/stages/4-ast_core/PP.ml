[@@@coverage exclude_file]
(* open Stage_common.Types *)
open Types
open Format
open PP_helpers
include Stage_common.PP

let operation ppf (o : Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation) : unit =
  let print_option f ppf o =
    match o with
      Some (s) -> fprintf ppf "%a" f s
    | None -> fprintf ppf "None"
  in
    let open Tezos_micheline.Micheline in
  let rec prim ppf (node : (_,Memory_proto_alpha.Protocol.Alpha_context.Script.prim) node)= match node with
    | Int (l , i) -> fprintf ppf "Int (%i, %a)" l Z.pp_print i
    | String (l , s) -> fprintf ppf "String (%i, %s)" l s
    | Bytes (l, b) -> fprintf ppf "B (%i, %s)" l (Bytes.to_string b)
    | Prim (l , p , nl, a) -> fprintf ppf "P (%i, %s, %a, %a)" l
        (Memory_proto_alpha.Protocol.Michelson_v1_primitives.string_of_prim p)
        (list_sep_d prim) nl
        (list_sep_d (fun ppf s -> fprintf ppf "%s" s)) a
    | Seq (l, nl) -> fprintf ppf "S (%i, %a)" l
        (list_sep_d prim) nl
  in
  let l ppf (l: Memory_proto_alpha.Protocol.Alpha_context.Script.lazy_expr) =
    let oo = Data_encoding.force_decode l in
    match oo with
      Some o -> fprintf ppf "%a" prim (Tezos_micheline.Micheline.root o)
    | None  -> fprintf ppf "Fail decoding"
  in

  let op ppf (type a) : a Memory_proto_alpha.Protocol.Alpha_context.manager_operation -> unit = function
    | Reveal (s: Tezos_protocol_environment_ligo006_PsCARTHA__Environment.Signature.Public_key.t) -> 
      fprintf ppf "R %a" Tezos_protocol_environment_ligo006_PsCARTHA__Environment.Signature.Public_key.pp s
    | Transaction {amount; parameters; entrypoint; destination} ->
      fprintf ppf "T {%a; %a; %s; %a}"
        Memory_proto_alpha.Protocol.Alpha_context.Tez.pp amount
        l parameters
        entrypoint
        Memory_proto_alpha.Protocol.Alpha_context.Contract.pp destination

    | Origination {delegate; script; credit; preorigination} ->
      fprintf ppf "O {%a; %a; %a; %a}" 
        (print_option Tezos_protocol_environment_ligo006_PsCARTHA__Environment.Signature.Public_key_hash.pp) delegate
        l script.code
        Memory_proto_alpha.Protocol.Alpha_context.Tez.pp credit
        (print_option Memory_proto_alpha.Protocol.Alpha_context.Contract.pp) preorigination
        
    | Delegation so ->
      fprintf ppf "D %a" (print_option Tezos_protocol_environment_ligo006_PsCARTHA__Environment.Signature.Public_key_hash.pp) so
  in
  let Internal_operation {source;operation;nonce} = o in
  fprintf ppf "{source: %s; operation: %a; nonce: %i"
    (Memory_proto_alpha.Protocol.Alpha_context.Contract.to_b58check source)
    op operation
    nonce

let type_variable ppf (t : type_variable) : unit = fprintf ppf "%a" Var.pp t

  let record_sep value sep ppf (m : 'a label_map) =
    let lst = LMap.to_kv_list m in
    let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
    let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
    fprintf ppf "%a" (list_sep new_pp sep) lst
  let variant_sep_d x = record_sep x (tag " ,@ ")

  let tuple_sep value sep ppf m =
    assert (Helpers.is_tuple_lmap m);
    let lst = Helpers.tuple_of_record m in
    let new_pp ppf (_, {associated_type;_}) = fprintf ppf "%a" value associated_type in
    fprintf ppf "%a" (list_sep new_pp sep) lst

  let record_sep_expr value sep ppf (m : 'a label_map) =
    let lst = LMap.to_kv_list m in
    let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
    let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
    fprintf ppf "%a" (list_sep new_pp sep) lst

  let tuple_sep_expr value sep ppf m =
    assert (Helpers.is_tuple_lmap m);
    let lst = Helpers.tuple_of_record m in
    let new_pp ppf (_,v) = fprintf ppf "%a" value v in
    fprintf ppf "%a" (list_sep new_pp sep) lst

(* Prints records which only contain the consecutive fields
  0..(cardinal-1) as tuples *)
let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m

let tuple_or_record_sep_expr value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep_expr value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep_expr value (tag sep_record)) m

let tuple_or_record_sep_expr value = tuple_or_record_sep_expr value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " ,@ "
let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " *@ "

let list_sep_d_par f ppf lst =
  match lst with 
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.content with
  | T_sum m -> fprintf ppf "@[<hv 4>sum[%a]@]" (variant_sep_d type_expression) m
  | T_record m -> fprintf ppf "%a" (tuple_or_record_sep_type type_expression) m
  | T_arrow a -> fprintf ppf "%a -> %a" type_expression a.type1 type_expression a.type2
  | T_variable tv -> type_variable ppf tv
  | T_wildcard -> fprintf ppf "_"
  | T_constant {type_constant=tc;arguments} -> fprintf ppf "%a%a" type_constant tc (list_sep_d_par type_expression) arguments

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev.wrap_content


let rec expression ppf (e : expression) =
  expression_content ppf e.content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "@[<hv>(%a)@@(%a)@]" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "@[%a(%a)@]" label c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "@[%a@[<hv 1>(%a)@]@]" constant c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "%a" (tuple_or_record_sep_expr expression) m
  | E_record_accessor ra ->
      fprintf ppf "@[%a.%a@]" expression ra.record label ra.path
  | E_record_update {record; path; update} ->
      fprintf ppf "@[{ %a@;<1 2>with@;<1 2>{ %a = %a } }@]" expression record label path expression update
  | E_lambda {binder; input_type; output_type; result} ->
      fprintf ppf "@[lambda (%a:%a) : %a@ return@ %a@]"
        expression_variable binder
        (PP_helpers.option type_expression)
        input_type
        (PP_helpers.option type_expression)
        output_type expression result
  | E_recursive { fun_name; fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )" 
        expression_variable fun_name 
        type_expression fun_type
        expression_content (E_lambda lambda)
  | E_matching {matchee; cases; _} ->
      fprintf ppf "@[match %a with@ %a@]" expression matchee (matching expression)
        cases
  | E_let_in { let_binder ;rhs ; let_result; inline } ->    
    fprintf ppf "@[let %a =@;<1 2>%a%a in@ %a@]" option_type_name let_binder expression rhs option_inline inline expression let_result
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_ascription {anno_expr; type_annotation} ->
      fprintf ppf "%a : %a" expression anno_expr type_expression
        type_annotation

and option_type_name ppf
    ({binder; ascr} : let_binder) =
  match ascr with
  | None ->
      fprintf ppf "%a" expression_variable binder
  | Some ty ->
      fprintf ppf "%a : %a" expression_variable binder type_expression ty

and assoc_expression ppf : expression * expression -> unit =
 fun (a, b) -> fprintf ppf "@[<2>%a ->@;<1 2>%a@]" expression a expression b

and single_record_patch ppf ((p, expr) : label * expression) =
  fprintf ppf "%a <- %a" label p expression expr

and matching_variant_case : (_ -> expression -> unit) -> _ -> match_variant -> unit =
  fun f ppf {constructor=c ; proj ; body } ->
  fprintf ppf "| %a %a ->@;<1 2>%a@ " label c expression_variable proj f body

and matching : (formatter -> expression -> unit) -> formatter -> matching_expr -> unit =
  fun f ppf m -> match m with
    | Match_variant lst ->
        fprintf ppf "@[<hv>%a@]" (list_sep (matching_variant_case f) (tag "@ ")) lst
    | Match_list {match_nil ; match_cons = {hd; tl; body}} ->
        fprintf ppf "@[<hv>| Nil ->@;<1 2>%a@ | %a :: %a ->@;<1 2>%a@]"
          f match_nil expression_variable hd expression_variable tl f body 
    | Match_option {match_none ; match_some = {opt; body}} ->
        fprintf ppf "@[<hv>| None ->@;<1 2>%a@ | Some %a ->@;<1 2>%a@]" f match_none expression_variable opt f body

(* Shows the type expected for the matched value *)
and matching_type ppf m = match m with
  | Match_variant lst ->
      fprintf ppf "variant %a" (list_sep matching_variant_case_type (tag "@.")) lst
  | Match_list _ ->
      fprintf ppf "list"
  | Match_option _ ->
      fprintf ppf "option"

and matching_variant_case_type ppf {constructor=c ; proj ; body=_ } =
  fprintf ppf "| %a %a" label c expression_variable proj

and option_mut ppf mut = 
  if mut then 
    fprintf ppf "[@@mut]"
  else
    fprintf ppf ""

and option_inline ppf inline = 
  if inline then 
    fprintf ppf "[@@inline]"
  else
    fprintf ppf ""

let declaration ppf (d : declaration) =
  match d with
  | Declaration_type {type_binder ; type_expr} ->
      fprintf ppf "@[<2>type %a =@ %a@]" type_variable type_binder type_expression type_expr
  | Declaration_constant {binder ; type_opt ; attr ; expr} ->
      fprintf ppf "@[<2>const %a =@ %a%a@]" option_type_name {binder; ascr = type_opt} expression
        expr
        option_inline attr.inline

let program ppf (p : program) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map Location.unwrap p)
