module Location = Simple_utils.Location
module Pair     = Simple_utils.Pair
module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Simple_utils.Trace
module VMap = Simple_utils.Map.Make(I.ValueVar)
module Maps = Stage_common.Maps


let rec add_to_end (expression: O.expression) to_add =
  match expression.expression_content with
  | O.E_let_in lt ->
    let lt = {lt with let_result = add_to_end lt.let_result to_add} in
    {expression with expression_content = O.E_let_in lt}
  | O.E_sequence seq ->
    let seq = {seq with expr2 = add_to_end seq.expr2 to_add} in
    {expression with expression_content = O.E_sequence seq}
  | _ -> O.e_sequence expression to_add

and store_mutable_variable (free_vars : Z.t VMap.t) =
  if (VMap.is_empty free_vars) then
    O.e_unit ()
  else
    O.e_tuple @@ List.map ~f:O.e_variable @@
     List.map ~f:fst @@ List.dedup_and_sort
     ~compare:(fun (_,a) (_,b) -> Z.compare a b) @@ VMap.to_kv_list free_vars

let rec compile_type_expression ~raise : I.type_expression -> O.type_expression =
  fun te ->
  (*
    At this point sum and record types becomes linear by their type (see previous pass in self_imperative)
    TODO: nano pass from non-linear to linear types (?)
  *)
  let self = compile_type_expression ~raise in
  let return tc = O.make_t ~loc:te.location tc in
  match te.type_content with
    | I.T_sum { attributes ; fields } ->
      let fields = O.LMap.(map (Maps.row_element self) (of_list fields)) in
      return @@ O.T_sum { attributes ; fields }
    | I.T_record { attributes ; fields } ->
      let fields = O.LMap.(map (Maps.row_element self) (of_list fields)) in
      return @@ O.T_record { attributes ; fields }
    | I.T_tuple tuple ->
      let tuple = List.map ~f:self tuple in
      return @@ O.T_tuple tuple
    | I.T_arrow arr ->
      let arr = Maps.arrow self arr in
      return @@ T_arrow arr
    | I.T_variable type_variable -> return @@ T_variable type_variable
    | I.T_app {type_operator;arguments=[l;r]} when I.TypeVar.equal Stage_common.Constant.v_michelson_or type_operator ->
      let (l, l_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let (r, r_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let (l,r) = Pair.map ~f:(compile_type_expression ~raise) (l,r) in
      let sum : (O.label * _ O.row_element) list = [
        (O.Label "M_left" , {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (O.Label "M_right", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 1}); ]
      in
      return @@ O.T_sum { fields = O.LMap.of_list sum ; attributes = [] }
    | I.T_app {type_operator;arguments=[l;r]} when I.TypeVar.equal Stage_common.Constant.v_michelson_pair type_operator ->
      let (l, l_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted l in
      let (r, r_ann) = trace_option ~raise (Errors.corner_case "not an annotated type") @@ I.get_t_annoted r in
      let (l,r) = Pair.map ~f:(compile_type_expression ~raise) (l,r) in
      let sum : (O.label * _ O.row_element) list = [
        (O.Label "0", {associated_type = l ; attributes = [ "annot:"^l_ann ] ; decl_pos = 0});
        (O.Label "1", {associated_type = r ; attributes = [ "annot:"^r_ann ] ; decl_pos = 1}); ]
      in
      return @@ O.T_record { fields = (O.LMap.of_list sum) ; attributes = [] }
    | I.T_app c ->
      let c = Maps.type_app self c in
      return @@ T_app c
    | I.T_module_accessor ma -> return @@ T_module_accessor ma
    | I.T_annoted (ty, _) -> self ty
    | I.T_singleton t -> return @@ O.T_singleton t
    | I.T_abstraction x ->
      let type_ = self x.type_ in
      return @@ O.T_abstraction { x with type_ }
    | I.T_for_all x ->
      let type_ = self x.type_ in
      return @@ O.T_for_all { x with type_ }


let rec compile_expression ~raise ~last : I.expression -> O.expression =
  fun e ->
  let e = compile_expression' ~raise ~last e in
  e None

and compile_expression' ~raise ~last : I.expression -> O.expression option -> O.expression =
  fun e ->
  let open Stage_common.Maps in
  let self = compile_expression ~raise ~last in
  let self_type = compile_type_expression ~raise in
  let return' expr = function
    | None -> expr
    | Some e -> O.e_sequence expr e
  in
  let return expr = return' @@ O.make_e ~loc:e.location expr in
  match e.expression_content with
    | I.E_literal literal   -> return @@ O.E_literal literal
    | I.E_constant {cons_name;arguments} ->
      let arguments = List.map ~f:(compile_expression ~raise ~last:true) arguments in
      return' @@ O.e_constant ~loc:e.location (Stage_common.Types.const_name cons_name) arguments
    | I.E_variable name     -> return @@ O.E_variable name
    | I.E_application app ->
      let app = application self app in
      return @@ O.E_application app
    | I.E_lambda lamb ->
      let lamb = lambda self self_type lamb in
      return @@ O.E_lambda lamb
    | I.E_type_abstraction ta ->
      let ta = type_abs self ta in
      return @@ O.E_type_abstraction ta
    | I.E_recursive recs ->
      let recs = recursive self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;attributes;rhs;let_result} ->
      let let_binder = binder self_type let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      return @@ O.E_let_in {let_binder;mut=false; attributes; rhs; let_result}
    | I.E_type_in ti ->
      let ti = type_in self self_type ti in
      return @@ O.E_type_in ti
    | I.E_mod_in mi ->
      let mi = mod_in self self_type (fun a -> a) (fun a -> a) (fun a -> a) mi in
      return @@ O.E_mod_in mi
    | I.E_raw_code rc ->
      let rc = raw_code self rc in
      return @@ O.E_raw_code rc
    | I.E_constructor const ->
      let const = constructor self const in
      return @@ O.E_constructor const
    | I.E_matching {matchee;cases} ->
      let matchee = self matchee in
      let aux I.{pattern;body} =
        let pattern = Stage_common.Helpers.map_pattern_t (binder (compile_type_expression ~raise)) pattern in
        O.{pattern;body = self body} in
      let cases   = List.map ~f:aux cases in
      return @@ O.E_matching {matchee;cases}
    | I.E_record recd ->
      (* at this point record expression become linear wrt labels *)
      let recd = List.map ~f:(fun (l,e) -> l, self e) recd in
      return @@ O.E_record (O.LMap.of_list recd)
    | I.E_accessor acc ->
      let acc = accessor self acc in
      return @@ O.E_accessor acc
    | I.E_update up ->
      let up = update self up in
      return @@ O.E_update up
    | I.E_map map ->
      let map = List.map ~f:(
        Pair.map ~f:self
      ) map
      in
      return @@ O.E_map map
    | I.E_big_map big_map ->
      let big_map = List.map ~f:(
        Pair.map ~f:self
      ) big_map
      in
      return @@ O.E_big_map big_map
    | I.E_list lst ->
      let lst = List.map ~f:self lst in
      return @@ O.E_list lst
    | I.E_set set ->
      let set = List.map ~f:self set in
      return @@ O.E_set set
    | I.E_ascription ascr ->
      let ascr = ascription self self_type ascr in
      return @@ O.E_ascription ascr
    | I.E_module_accessor ma -> return @@ O.E_module_accessor ma
    | I.E_cond {condition;then_clause;else_clause} ->
      let condition    = self condition in
      let then_clause = self then_clause in
      let else_clause = self else_clause in
      return @@ O.E_cond {condition;then_clause;else_clause}
    | I.E_sequence {expr1; expr2} ->
      let expr1 = compile_expression' ~raise ~last:false expr1 in
      let expr2 = compile_expression' ~raise ~last expr2 in
      fun e -> expr1 (Some (expr2 e))
    | I.E_skip -> return @@ O.E_skip
    | I.E_tuple tuple ->
      let tuple = List.map ~f:self tuple in
      return @@ O.E_tuple tuple
    | I.E_assign {binder=b; expression} ->
      let binder = binder self_type b in
      let expression = self expression in
      return @@ O.E_assign {binder; expression}
    | I.E_for {binder;start;final;incr;f_body} ->
      (*Make the cond and the step *)
      let final = compile_expression ~raise ~last final in
      let start = compile_expression ~raise ~last start in
      let step = compile_expression ~raise ~last incr in
      let for_body = compile_expression ~raise ~last f_body in

      let rec_func = I.ValueVar.fresh ~name:"fun_for_loop" () in
      let recursive_call arg = O.e_application (O.e_variable rec_func) arg in

      (* Modify the body loop*)
      let ctrl =
          O.e_let_in_ez binder false [] (O.e_constant C_ADD [ O.e_variable binder ; step ]) @@
          recursive_call (O.e_variable binder)
      in
      let cond = O.e_annotation (O.e_constant (C_LE) [O.e_variable binder ; final]) (O.t_bool ()) in
      let continue_expr = add_to_end for_body ctrl in
      let stop_expr      = (O.e_unit ()) in
        let loop = O.e_recursive rec_func (O.t_arrow (O.t_int ()) (O.t_unit ())) @@
          {
            binder={
              var=binder;
              ascr=Some(O.t_int ());
              attributes={const_or_var=None}};
            output_type=None;
            result=O.e_cond cond continue_expr stop_expr
          } in
        return @@
          O.E_let_in {let_binder={var=rec_func;ascr=None;attributes={const_or_var=None}}; rhs=loop; mut=false; let_result=recursive_call start;attributes=[]}
    | I.E_for_each {fe_binder;collection;collection_type; fe_body} ->
      let body = compile_expression ~raise ~last fe_body in
      let collection = compile_expression ~raise ~last collection in
      let op_name =
        match collection_type with
      | Map -> O.C_MAP_ITER | Set -> O.C_SET_ITER | List -> O.C_LIST_ITER | Any -> O.C_ITER
      in

      let args    = I.ValueVar.fresh ~name:"args" () in
      let k,v = fe_binder in
      let pattern : O.type_expression O.pattern = match v with
        | Some v -> Location.wrap @@ O.P_tuple [Location.wrap @@ O.P_var {var=k;ascr=None;attributes={const_or_var=None}}; Location.wrap @@ O.P_var {var=v;ascr=None;attributes={const_or_var=None}}]
        | None -> Location.wrap @@ O.P_var {var=k;ascr=None;attributes={const_or_var=None}}
      in

      let lambda = O.e_lambda_ez (args) None @@
                  O.e_matching (O.e_variable args) [
                    {pattern;body}
                  ] in
      return @@
        O.E_constant {cons_name=op_name;arguments=[lambda;collection]}

    | I.E_while {cond;body} ->
      (* compile while*)
      let cond = compile_expression ~raise ~last cond in
      let body = compile_expression ~raise ~last body in

      let rec_func = I.ValueVar.fresh ~name:"fun_while_loop" () in
      let recursive_call = O.e_application (O.e_variable rec_func) (O.e_unit ()) in
      let continue_expr  = add_to_end body recursive_call in
      let stop_expr      = (O.e_unit ()) in
      let loop = O.e_recursive rec_func (O.t_arrow (O.t_unit ()) (O.t_unit ())) @@
        {
          binder={
            var=I.ValueVar.fresh ~name:"()" ();
            ascr=Some(O.t_unit ());
            attributes={const_or_var=None}};
          output_type=None;
          result=O.e_cond cond continue_expr stop_expr
        } in
      return @@
        O.E_let_in {let_binder={var=rec_func;ascr=None;attributes={const_or_var=None}}; rhs=loop; mut=false; let_result=recursive_call;attributes=[]}

and compile_module ~raise : I.module_ -> O.module_ = fun m ->
  Maps.declarations (compile_expression ~raise ~last:true) (compile_type_expression ~raise) Fn.id Fn.id Fn.id m

let compile_declaration ~raise : I.declaration -> O.declaration =
  fun {wrap_content=declaration;location} ->
  let return decl = Location.wrap ~loc:location decl in
  match declaration with
  | I.Declaration_type dt ->
    let dt = Maps.declaration_type (compile_type_expression ~raise) Fn.id dt in
    return @@ O.Declaration_type dt
  | I.Declaration_constant dc ->
    let dc = Maps.declaration_constant (compile_expression ~raise ~last:true) (compile_type_expression ~raise) Fn.id dc in
    return @@ O.Declaration_constant dc
  | I.Declaration_module dm ->
    let dm = Maps.declaration_module (compile_expression ~raise ~last:true) (compile_type_expression ~raise) Fn.id Fn.id Fn.id dm in
    return @@ O.Declaration_module dm

let compile_program ~raise : I.program -> O.program = fun p ->
  Simple_utils.Trace.collect ~raise @@
  List.map ~f:(fun a ~raise -> compile_declaration ~raise a) p
