[@@@coverage exclude_file]
open Types
open Format
open Simple_utils.PP_helpers

include Stage_common.PP

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" expression_variable ev

let list_sep_d_par f ppf lst =
  match lst with
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.type_content with
  | T_variable        tv -> type_variable ppf tv
  | T_sum             sm -> sum           type_expression ppf sm.fields
  | T_record          rd -> type_record   type_expression ppf rd.fields
  | T_tuple            t -> type_tuple    type_expression ppf t
  | T_arrow            a -> arrow         type_expression ppf a
  | T_app            app -> type_app      type_expression ppf app
  | T_module_accessor ma -> module_access type_variable ppf ma
  | T_singleton       x  -> literal       ppf             x
  | T_abstraction     x  -> abstraction   type_expression ppf x
  | T_for_all         x  -> for_all       type_expression ppf x

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

let rec expression ppf (e : expression) =
  expression_content ppf e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal     l -> literal                ppf l
  | E_variable    n -> expression_variable    ppf n
  | E_application a -> application expression ppf a
  | E_constructor c -> constructor expression ppf c
  | E_constant    c -> constant expression ppf c
  | E_record      m ->
      fprintf ppf "{%a}" (record_sep_expr expression (const ";")) m
  | E_tuple       t -> tuple       expression ppf t
  | E_accessor    a -> accessor    expression ppf a
  | E_update      u -> update      expression ppf u
  | E_lambda      l -> lambda      expression type_expression ppf l
  | E_type_abstraction e -> type_abs expression ppf e
  | E_matching    m -> match_exp expression type_expression ppf m
  | E_recursive  r -> recursive expression type_expression ppf r
  | E_let_in { let_binder ; rhs ; let_result; attributes=attr; mut} ->
      fprintf ppf "let %a%a = %a%a in %a"
        option_type_name let_binder
        option_mut mut
        expression rhs
        attributes attr
        expression let_result
  | E_type_in   ti -> type_in expression type_expression ppf ti
  | E_mod_in    mi -> mod_in  expression type_expression attributes attributes attributes ppf mi
  | E_raw_code   r -> raw_code   expression ppf r
  | E_ascription a -> ascription expression type_expression ppf a
  | E_module_accessor ma -> module_access expression_variable ppf ma
  | E_cond       c -> cond       expression ppf c
  | E_sequence   s -> sequence   expression ppf s
  | E_skip         -> skip                  ppf ()
  | E_map        m -> map        expression ppf m
  | E_big_map    m -> big_map    expression ppf m
  | E_list       l -> lst        expression ppf l
  | E_set        s -> set        expression ppf s
  | E_assign     a -> assign     expression type_expression ppf a


and option_type_name ppf {var;ascr;attributes=_}=
  match ascr with
  | None ->
      fprintf ppf "%a" expression_variable var
  | Some ty ->
      fprintf ppf "%a : %a" expression_variable var type_expression ty


and option_mut ppf mut =
  if mut then
    fprintf ppf "[@mut]"
  else
    fprintf ppf ""

and attributes ppf attributes =
  let attr =
    List.map ~f:(fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat
  in fprintf ppf "%s" attr

let declaration ppf (d : declaration) = declaration expression type_expression attributes attributes attributes ppf d

let module_ ppf (p : module_) = declarations expression type_expression attributes attributes attributes ppf p
