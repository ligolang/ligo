[@@@coverage exclude_file]

module Location = Simple_utils.Location
module Var = Simple_utils.Var
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Int64 = Caml.Int64
open Ligo_prim
open Types
open Simple_utils.PP_helpers

let rec type_content : Format.formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum row -> Row.PP.sum_type type_expression (fun _ _ -> ()) ppf row
  | T_record row -> Row.PP.record_type type_expression (fun _ _ -> ()) ppf row
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_for_all x -> Abstraction.pp_forall type_expression ppf x


and type_injection ppf { language; injection; parameters } =
  ignore language;
  Format.fprintf
    ppf
    "%s%a"
    (Literal_types.to_string injection)
    (list_sep_d_par type_expression)
    parameters


and bool ppf : unit = Format.fprintf ppf "bool"

and option ppf (te : type_expression) : unit =
  let t = Combinators.get_t_option te in
  match t with
  | Some t -> Format.fprintf ppf "option (%a)" type_expression t
  | None -> Format.fprintf ppf "option ('a)"


and type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool te)
  then bool ppf
  else if Option.is_some (Combinators.get_t_option te)
  then option ppf te
  else Format.fprintf ppf "%a" type_content te.type_content


let rec type_content_orig : Format.formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum row -> Row.PP.sum_type type_expression (fun _ _ -> ()) ppf row
  | T_record row -> Row.PP.record_type type_expression (fun _ _ -> ()) ppf row
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_for_all x -> Abstraction.pp_forall type_expression ppf x


and type_expression_orig ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  match te.orig_var with
  | None ->
    if Option.is_some (Combinators.get_t_bool te)
    then bool ppf
    else if Option.is_some (Combinators.get_t_option te)
    then option ppf te
    else Format.fprintf ppf "%a" type_content_orig te.type_content
  | Some v -> Ast_core.(PP.type_expression ppf (t_variable ~loc:te.location v ()))


let type_expression_annot ppf (te : type_expression) =
  Format.fprintf ppf " : %a" type_expression te


let rec expression ppf (e : expression) =
  Format.fprintf ppf "%a" expression_content e.expression_content


and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l -> Literal_value.pp ppf l
  | E_variable n -> Value_var.pp ppf n
  | E_application a -> Application.pp expression ppf a
  | E_constructor c -> Constructor.pp expression ppf c
  | E_constant c -> Constant.pp expression ppf c
  | E_record m -> Record.pp expression ppf m
  | E_accessor a -> Types.Accessor.pp expression ppf a
  | E_update u -> Types.Update.pp expression ppf u
  | E_lambda l -> Lambda.pp expression type_expression_annot ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching m -> Types.Match_expr.pp expression type_expression ppf m
  | E_recursive r -> Recursive.pp expression type_expression_annot ppf r
  | E_let_in x when not x.attributes.hidden -> Let_in.pp expression type_expression ppf x
  | E_let_in x -> expression ppf x.let_result
  | E_raw_code r -> Raw_code.pp expression ppf r
  | E_type_inst ti -> type_inst ppf ti
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    Format.fprintf
      ppf
      "@[let mut %a =@;<1 2>%a%a in@ %a@]"
      (Pattern.pp type_expression_annot)
      let_binder
      expression
      rhs
      Types.ValueAttr.pp
      attributes
      expression
      let_result
  | E_assign a -> Assign.pp expression type_expression ppf a
  | E_deref var -> Format.fprintf ppf "!%a" Value_var.pp var
  | E_coerce a -> Ascription.pp expression type_expression ppf a
  | E_for for_loop -> For_loop.pp expression ppf for_loop
  | E_for_each for_each -> For_each_loop.pp expression ppf for_each
  | E_while while_loop -> While_loop.pp expression ppf while_loop


and type_inst ppf { forall; type_ } =
  Format.fprintf ppf "%a@@{%a}" expression forall type_expression type_


and declaration ppf (x : declaration) =
  match Location.unwrap x with
  | D_value x -> Value_decl.pp expression type_expression ppf x
  | D_irrefutable_match x -> Pattern_decl.pp expression type_expression ppf x


and context ppf (x : context) = list_sep declaration (tag "\n") ppf x

module With_name_tbl = struct
  module Type_var_name_tbl : sig
    type t

    (** [create ()] creates a new type variable table. *)
    val create : unit -> t

    (** [clear t] clears the table [t]. *)

    (** [name_of t tvar] returns the human readable name of [tvar]. *)
    val name_of : t -> Type_var.t -> string
  end = struct
    type t =
      { name_tbl : (Type_var.t, string) Hashtbl.t
            (* [name_tbl] is the mapping from type variables to names *)
      ; names : string Hash_set.t
            (* [names] is the set of existing names (superset of [Hashtbl.data name_tbl]) *)
      ; mutable name_counter : int
            (* [name_counter] is a counter used to generate unique variable names *)
      }

    let create () =
      { name_tbl = Hashtbl.create (module Type_var)
      ; names = Hash_set.create (module String)
      ; name_counter = 0
      }


    let is_used t name = Hash_set.mem t.names name
    let incr_name_counter t = t.name_counter <- t.name_counter + 1

    let rec create_name t =
      let name =
        if t.name_counter < 26
        then String.of_char (Char.of_int_exn (97 + t.name_counter))
        else
          String.of_char (Char.of_int_exn (97 + (t.name_counter mod 26)))
          ^ Int.to_string (t.name_counter / 26)
      in
      incr_name_counter t;
      if is_used t name then create_name t else name


    let add_name t tvar name =
      Hashtbl.add_exn t.name_tbl ~key:tvar ~data:name;
      Hash_set.add t.names name


    let name_of t tvar =
      match Hashtbl.find t.name_tbl tvar with
      | Some name -> name
      | None ->
        let name =
          if Type_var.is_generated tvar
          then create_name t
          else (
            (* User-defined name. We'd like to try keep the name. However
               a collision could occur if we've previously used this name.

               We resolve the collision by adding a number to the end until we reach 
               a unique name *)
            let name = Type_var.to_name_exn tvar in
            let curr_name = ref name in
            let i = ref 0 in
            while is_used t !curr_name do
              curr_name := name ^ Int.to_string !i;
              Int.incr i
            done;
            !curr_name)
        in
        add_name t tvar name;
        (* Invariant: [name] is unique (wrt table [t]) *)
        name
  end

  let rec pp ~name_of_tvar ppf t =
    let pp = pp ~name_of_tvar in
    if Option.is_some (Combinators.get_t_bool t)
    then bool ppf
    else if Option.is_some (Combinators.get_t_option t)
    then option ~name_of_tvar ppf t
    else (
      match t.type_content with
      | T_variable tvar -> Format.fprintf ppf "%s" (name_of_tvar tvar)
      | T_arrow arr -> Arrow.pp pp ppf arr
      | T_constant constant -> pp_constant ~name_of_tvar ppf constant
      | T_singleton lit -> Literal_value.pp ppf lit
      | T_for_all for_all -> pp_forall ~name_of_tvar ppf for_all
      | T_sum row -> Row.PP.sum_type pp (fun _ _ -> ()) ppf row
      | T_record row -> Row.PP.record_type pp (fun _ _ -> ()) ppf row)


  and pp_constant ~name_of_tvar ppf { injection; parameters; _ } =
    Format.fprintf
      ppf
      "%s%a"
      (Literal_types.to_string injection)
      (list_sep_d_par (pp ~name_of_tvar))
      parameters


  and pp_forall ~name_of_tvar ppf ({ ty_binder; kind = _; type_ } : _ Abstraction.t)
      : unit
    =
    Format.fprintf ppf "∀ %s . %a" (name_of_tvar ty_binder) (pp ~name_of_tvar) type_


  and option ~name_of_tvar ppf t : unit =
    match Combinators.get_t_option t with
    | Some t -> Format.fprintf ppf "option (%a)" (pp ~name_of_tvar) t
    | None -> Format.fprintf ppf "option ('a)"


  let pp_with_name_tbl ~tbl ppf t =
    let name_of_tvar = Type_var_name_tbl.name_of tbl in
    pp ~name_of_tvar ppf t


  let pp =
    let name_of tvar = Format.asprintf "%a" Type_var.pp tvar in
    pp ~name_of_tvar:name_of
end
