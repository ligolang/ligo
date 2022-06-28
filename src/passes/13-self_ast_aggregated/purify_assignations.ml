open Ast_aggregated

(* Problem we are solving
  A Ligo program contains multiple scopes. The main "global" scope, contains everyting
  that have been declared previously.
  Then every rhs, function body (= rhs), matching branches (kind of rhs also) define their own
  subscope. Which means every scope modification (i.e. variable assignations) will be contained inside
  the rhs and will disappear after processing the rhs, as it doesn't affect the global scope.

  The assignation effect propagate variable modification outside of the local scope, going up the scope
  until it reaches the scope where the variable was defined.

  When removing the assignation effect we need to take into account the backpropagation of the value change.
  A simple solution in theory is to transform all the code with a monad for assignation, each value is paired with the
  list of all mutable variable defined so far, with their current values (S).
  - And assingation will modified the value of the
  variable in the list.
  - All function will be lifted, every parameters is paired with S, and return S.
  - Every let in are transformed into a match expression, to get the new value of S.
  - Every reading of a variable will be transformed into an access to this variable in S.
  - Every declaration of a new variable add a field in S
  This method has several issue, we don't have good primitive for list of associative list access to implement
  S with a list, we can't add left field to a record to implement S with a record. We don't have a clear
  differenciation between mutablne and immutable variable or reading of mutable variable.
  such transformation make list.map not work as expected.
  Most important, the produce code will likely be highly inefficient.
  (unless this is done directly in michelson)

  In order to avoid all those issue, we will use a more complex solution, that rely on several hypothesis :
    1. With our current front-end state, we have either purely functional code or purely imperative dialect.
       ie. we can't have `List.map` on a funtion that is not pure (and this won't work)
    2. An impure function can't be partially applied
    3. An anonymous functions doesn't contains effect
    This hypotheis holds as long as we don't have partial evaluation in Pascaligo nor mutable variables in
    cameligo (OCaml `ref`)
  The solutin is :
  - Inspect rhs to detect purity.
  - If rhs is pure, do nothing.
  - If rhs is not pure, there is two cases
      a. We have a `let variable definition` :
        - create (Swrite) containing only the var that are write
        - make the rhs return a pair (Swrite, original result)
        - replace the `let var = rhs in e` by a `match rhs' with (Swrite, var) -> e`
      b. We have a `let function definition` :
        - create (Swrite) and also (Sread) the list of mutable variable that are read in the body of the function.
        - modify the body of the function to match on (Sread) and return (Swrite, result).
        - modify the signature to add (Sread) as parameter.
        - modify all call to the function to add the (Sread) parameter and match on `(Swrite, result)` after application.
      (this really on point hyp 1. and 2.)


  This algorithm works in two step :

  1. looks at rhs and detect those who are pure and those which produce an assignation effect
  store the set of modified variables per fonction

  2. Modify the code by adding an extra (Sread) parameter and
  returning an extra (Smin) result
*)

module ValueVarMap = Simple_utils.Map.Make(ValueVar)

module Effect = struct
  module ValueVarMap = Simple_utils.Map.Make(ValueVar)
  type payload = {effects : type_expression ValueVarMap.t; read : type_expression ValueVarMap.t option}
  type t = {env : payload ValueVarMap.t ; global : payload }

  let new_payload () : payload = {effects = ValueVarMap.empty; read = None}
  let empty : t = {env = ValueVarMap.empty; global = new_payload ()}

  let add_effect t (ev : expression_variable) (type_ : type_expression) =
  {t with global = {t.global with effects = ValueVarMap.add ev type_ t.global.effects}}

  let add_read_effect (t:t) (ev : expression_variable) (type_ : type_expression) =
    {t with global = {t.global with read = Some(match t.global.read with None -> ValueVarMap.singleton ev type_ | Some (read) -> ValueVarMap.add ev type_ read)}}

  let add_effects t ev_set =
    let current = t.global.effects in
    let new_    = ValueVarMap.union (fun _ a _ -> Some (a)) current ev_set in
    { t with global = {t.global with effects=new_}}

  let store_binders_effects_func (t : t) (binder : expression_variable) =
    let global = t.global in
    let t = {
      env = ValueVarMap.add binder t.global t.env;
      global = global;
    } in
    t

  let store_binders_effects_var (t : t) (binder : expression_variable) =
    let global = t.global in
    let store  = {effects=t.global.effects; read=None} in
    let t = {
      env = ValueVarMap.add binder store t.env;
      global = global;
    } in
    t

  let add a b : t =
    let payload a b =
      {effects = ValueVarMap.union (fun _ a _ -> Some (a)) a.effects b.effects;
      read = (fun a b -> match a,b with
        a, None -> a
      | None, b -> b
      | Some(a), Some(b) -> Some (ValueVarMap.union (fun _ a _ -> Some (a)) a b)
      ) a.read b.read;
      }
    in
    { env = ValueVarMap.union (fun _ a b -> Some (payload a b)) a.env b.env;
      global = payload a.global b.global}
  let rec concat (t : t list) : t = match t with
    [] -> empty
  | hd :: tl -> add hd @@ concat tl

  let pp ppf (t : t) =
    let pp_global ppf payload =
      Format.fprintf ppf "(%a),%a"
      (PP.list_sep_d ValueVar.pp) (ValueVarMap.keys payload.effects)
      (Format.pp_print_option ~none:(fun ppf () -> Format.fprintf ppf "None") (fun ppf -> Format.fprintf ppf "(%a)" (PP.list_sep_d ValueVar.pp))) (Option.map ~f:ValueVarMap.keys payload.read)
    in
    Format.fprintf ppf "Effects : @.
    @[{ env : %a @.
    global : %a } @]\n%!"
    PP.(list_sep_d (fun ppf (a,b) -> Format.fprintf ppf "%a => %a" ValueVar.pp a pp_global b) ) (ValueVarMap.to_kv_list t.env)
    pp_global t.global

  let make_tuple (ev_list : type_expression ValueVarMap.t) : expression =
    let _,ev_list = ValueVarMap.fold (
      fun ev t (i,t_list) ->
        let t_list = ((i,e_variable ev t),(i,t)) :: t_list in
        i+1,t_list
    ) ev_list (0,[]) in
    let tuple,tuple_type = List.unzip ev_list in
    (* For function with no read effect *)
    match tuple with [] -> e_a_unit ()
    | _ -> e_record (LMap.of_list @@ List.map ~f:(fun (i,e) -> Label (string_of_int i),e) tuple)
      @@ t_record ~layout:L_tree @@ LMap.of_list @@ List.map ~f:(fun (i,t) ->
       Label (string_of_int i),
       {associated_type=t ; michelson_annotation=None ; decl_pos = 0})tuple_type

  let get_read_effect (t:t) (ev : expression_variable) =
    match ValueVarMap.find_opt ev t.env with
      None -> None,false
    | Some ({effects=_; read=None}) -> None,false
    | Some {effects=_; read=Some(read)} ->
      if ValueVarMap.(compare Compare.type_expression read empty) = 0
        then None,false else Some (make_tuple read),true

  let get_effect_var (t:t) (ev : expression_variable) =
    (* Format.printf "get effect for %a in %a\n%!" PP.expression_variable ev pp t; *)
    match ValueVarMap.find_opt ev t.env with
      None -> (None,None)
    | Some ({effects;read}) ->
      let aux effects =
        let _,map = ValueVarMap.fold (
          fun ev t (i,map) ->
            let map = (i,(ev,t)) :: map in
            i+1,map
        ) effects (0,[]) in
        let effects' = LMap.of_list @@ List.map ~f:(fun (i,(var,ty)) -> Label (string_of_int i),{var;ascr=Some ty;attributes={const_or_var=None}}) map in
        effects', make_tuple effects
      in
      let empty_to_none f a = if ValueVarMap.is_empty a then None else Some (f a)  in
      empty_to_none aux effects, Option.map ~f:aux read

  let rm_var (ev : expression_variable) (t : t) =
    let read = match t.global.read with None -> None
    | Some (read) ->
      let read = ValueVarMap.remove ev read in
      Some (read)
    in
    let global = {effects = ValueVarMap.remove ev t.global.effects; read} in
    {t with global = global}
  let remove_read_effect (t:t) = {t with global = {t.global with read = None}}
end

module ValueVarSet = Caml.Set.Make(ValueVar)
let rec detect_effect_in_expression (mut_var : ValueVarSet.t) (e : expression) =
  (* Format.printf "detect_effect_in_expression %a with mut_var %a\n%!" PP.expression e (Format.pp_print_seq ~pp_sep:(fun ppf () -> Format.fprintf ppf ",") PP.expression_variable) (ValueVarSet.to_seq mut_var); *)
  let self ?(mut_var = mut_var) e = detect_effect_in_expression mut_var e in
  let return effect = (* Format.printf "effect_detected in %a : %a\n%!" PP.expression e Effect.pp effect; *) effect in
  match e.expression_content with
  | E_literal  _ -> return @@ Effect.empty
  | E_variable var when ValueVarSet.mem var mut_var -> return @@ Effect.add_read_effect Effect.empty var e.type_expression
  | E_variable _ -> return @@ Effect.empty
    (* Special case use for compilation of imperative for each loops *)
  | E_constant {cons_name=( C_LIST_ITER | C_MAP_ITER | C_SET_ITER | C_ITER) as opname ;
                arguments=[lambda ; collect ; ]} ->
      (* Effects could be modify by collect so we don't look up*)
      let _ = opname,collect in
      return @@ self lambda
  | E_constant {cons_name=_;arguments} -> return @@ Effect.concat @@ List.map ~f:self arguments
  | E_application {lamb;args} ->
      let effect1  = self lamb in
      let effect2  = self args in
    return @@ Effect.add effect1 effect2
  | E_lambda {binder;result} ->
      self result |> Effect.rm_var binder.var
  | E_recursive {fun_name;fun_type=_;lambda={binder;result}} ->
      self result |> Effect.rm_var binder.var |> Effect.rm_var fun_name
  | E_let_in {let_binder;rhs;let_result;attr=_} ->
      let effect = self rhs in
      let effect =
        (* check for fonction *)
        if is_t_arrow rhs.type_expression then
          Effect.store_binders_effects_func effect let_binder.var
        else (* The read effect needs to be propagated and not stored *)
          Effect.store_binders_effects_var effect let_binder.var
      in
      let mut_var = match let_binder.attributes.const_or_var with
        Some (`Var) -> ValueVarSet.add let_binder.var  mut_var
      | _ -> mut_var in
      let effect = Effect.add effect @@ self ~mut_var let_result in
      let effect = Effect.rm_var let_binder.var effect in
      return @@ effect
  | E_raw_code {language=_;code=_} -> Effect.empty
  | E_type_inst {type_=_;forall} -> self forall
  | E_type_abstraction {type_binder=_;result} -> self result
  (* Variant *)
  | E_constructor _ -> Effect.empty
  | E_matching {matchee;cases} ->
    (* Get all effect var from both branch*)
    (* return values at both ends *)
    (* handles value changing due to fonction calls *)
      let effect = self matchee in
      let effect = Effect.add effect @@ (match cases with
        Match_variant {cases;tv=_} ->
          let effect = List.map ~f:(fun {constructor=_;pattern;body} ->
            let effect = self body in
            let effect = Effect.rm_var pattern effect in
            effect
            ) cases in
          Effect.concat effect
      | Match_record {fields;body;tv=_}->
        let mut_var = LMap.fold (fun _ {var;ascr=_;attributes;} mut_var -> (match attributes.const_or_var with
          Some (`Var) -> ValueVarSet.add var mut_var | _ -> mut_var) ) fields mut_var in
        let effect = self ~mut_var body in
        let effect = LMap.fold (fun _ b -> Effect.rm_var b.var) fields effect in
        effect
      ) in
      effect
  (* Record *)
  | E_record map ->
    Effect.concat @@ List.map ~f:(fun e -> self e) @@ LMap.to_list map
  | E_record_accessor {record;path=_} -> self record
  | E_record_update {record;path=_;update} ->
    let effect = self record in
    let effect = Effect.add effect @@ self update in
    effect
  | E_assign {binder;access_path=_;expression} ->
    let effect = self expression in
    return @@ Effect.add_effect effect binder.var (Option.value_exn binder.ascr)

(*
    This function attend to transform expression that have effect in an rhs.
    We have two case, either the effect is in a variable declaration and propagate at declaration :
      ```
      let a = b := 1 in
      e
      ```
    the you want to add the effectful variable in the binder and in the return expression :
      ```
      let a,b = b := 1 in (e,b)
      ```

    (other morphing are possible let b = b:= 1 in b in let a = e, but this requires to handle the value in
    the rhs before and after the assignation )

    The effect is in a function body. We also need to return the new value but, in case the function is
    use in a loop, we also need to add the variables as extra parameters.

    There is several possible possition to add the variable. In first position, in last position
    or add it to every parameters.
    The simple case is to add it in the first position. but that will break partial application.
    Considering that we don't have syntax with both partial application and assignation, we will
    use this method

*)
let rec add_to_the_top_of_function_body binder effects effects_type e =
  let self = add_to_the_top_of_function_body binder effects effects_type in
  let return expression_content = { e with expression_content } in
  match e.expression_content with
    E_lambda {binder;result}->
      let result = self result in
      return @@ E_lambda {binder;result}
  | _  ->
    e_matching {matchee = e_variable binder effects_type;
      cases = Match_record {fields = effects; body = e; tv = effects_type}}
    e.type_expression

(* Change 'a -> 'b -> .... -> 'return into 'read_effect_type -> 'a -> 'b -> ... -> 'effect_type * 'return *)
let morph_function_type (read_effect_type : type_expression option) (effect_type : type_expression option) (fun_type : type_expression) =
  let rec add_to_retun_type effect_type fun_type =
  let self = add_to_retun_type effect_type in
  let return type_content = {fun_type with type_content} in
  match fun_type.type_content with
    T_variable _ | T_constant _ | T_sum _ | T_record _ | T_singleton _ ->
      (match effect_type with None -> fun_type | Some (e) -> t_pair e fun_type)
  | T_arrow {type1;type2} ->
    let type2 = self type2 in
    return @@ T_arrow {type1;type2}
  | T_for_all {ty_binder;kind;type_} ->
    let type_ = self type_ in
    return @@ T_for_all {ty_binder;kind;type_}
  in match read_effect_type with
    None -> add_to_retun_type effect_type fun_type
  | Some (read_effect_type) -> t_arrow read_effect_type (add_to_retun_type effect_type fun_type) ()

let add_parameter read_effect read_effects effects_type rhs =
  (* Format.printf "add_parameters to rhs: %a" PP.expression rhs; *)
  match rhs.expression_content with
    E_lambda _ ->
      let var = ValueVar.fresh ~name:"effect_binder" () in
      let binder = {var;ascr=None;attributes=Stage_common.Helpers.empty_attribute} in
      let result = add_to_the_top_of_function_body var read_effects read_effect.type_expression rhs in
      let lambda_type = morph_function_type (Some read_effect.type_expression) (Some effects_type) rhs.type_expression in
      e_lambda {binder;result} lambda_type
  | E_recursive {fun_name;fun_type;lambda={binder;result}} ->
      let var = ValueVar.fresh ~name:"effect_binder" () in
      let binder_eff = {var;ascr=None;attributes=Stage_common.Helpers.empty_attribute} in
      let result = add_to_the_top_of_function_body var read_effects read_effect.type_expression result in
      let result = e_lambda {binder;result} rhs.type_expression in
      let fun_type = morph_function_type (Some read_effect.type_expression) (Some effects_type) fun_type in
      e_recursive {fun_name;fun_type;lambda={binder=binder_eff;result}} fun_type
  | _ -> failwith "Add_parameters: not a function"

let rec morph_function_application (effect : Effect.t) (e: expression) : _ * expression =
  let self = morph_function_application effect in
  let return returned_effect type_expression expression_content = returned_effect, { e with expression_content ; type_expression } in
  match e.expression_content with
  | E_variable variable ->
      (match Effect.get_effect_var effect variable with
        returned_effect,None ->
          let type_ = morph_function_type None (Option.map ~f:(fun (_,x) -> x.type_expression) returned_effect) e.type_expression in
          return returned_effect type_ @@ E_variable variable
      | returned_effect,Some (_,read_effects) ->
          let type_ = morph_function_type (Some read_effects.type_expression) (Option.map ~f:(fun (_,x) -> x.type_expression) returned_effect) e.type_expression in
          let e     = e_variable variable type_ in
          let {type1=_;type2} = get_t_arrow_exn type_ in
          return returned_effect type2 @@ E_application {lamb=e;args=read_effects})
  | E_type_inst {forall;type_} ->
      let returned_effect,forall = self forall in
      let {ty_binder;kind=_;type_=ty} = get_t_for_all_exn forall.type_expression in
      let ty = Helpers.subst_type ty_binder type_ ty in
      return returned_effect ty @@ E_type_inst {forall;type_}
  | E_application {lamb;args} ->
      let returned_effect,lamb = self lamb in
      let {type1=_;type2} = get_t_arrow_exn lamb.type_expression in
      return returned_effect type2 @@ E_application {lamb;args}
  | _ -> failwith "Hypothesis 3 don't hold"

let match_on_write_effect let_binder rhs let_result effects effect_type =
  let effect_var = ValueVar.fresh ~name:"effect_binder" () in
  let effect_binder = {var=effect_var;ascr=Some effect_type;attributes={const_or_var=None}} in
  let let_result = e_matching {matchee=e_variable effect_var effect_type;cases=
    Match_record {fields=effects;body=let_result;tv=effect_type}} let_result.type_expression in
  let tv = t_pair effect_type rhs.type_expression in
  E_matching {matchee=rhs;cases=
    Match_record {
      fields=LMap.of_list
      [
        (Label "0",effect_binder);
        (Label "1",let_binder)
      ];
      body=let_result;tv}
    }

let rec morph_expression ?(returned_effect) (effect : Effect.t) (e: expression) : expression =
  (* Format.printf "Morph_expression %a with effect : (%a)\n%!" PP.expression e PP.(Stage_common.PP.option_type_expression expression) returned_effect; *)
  let return_1 ?returned_effect e =
    match returned_effect with None -> e
    | Some (ret_eff) -> e_a_pair ret_eff e  in
  let return ?returned_effect expression_content =
    let ret_expr = { e with expression_content } in
    return_1 ?returned_effect ret_expr in
  let self ?returned_effect = morph_expression ?returned_effect effect in
  match e.expression_content with
  | E_literal lit -> return ?returned_effect @@ E_literal lit
  | E_raw_code rc -> return ?returned_effect @@ E_raw_code rc
  | E_variable variable ->
      (match Effect.get_read_effect effect variable with
        None,false -> return ?returned_effect @@ E_variable variable
      | _ ->
          failwith "Hypothesis 2 failed"
      )
    (* Special case use for compilation of imperative for each loops *)
  | E_constant {cons_name=( C_LIST_ITER | C_MAP_ITER | C_SET_ITER | C_ITER) as cons_name ;
                arguments=[
                    ( { expression_content = (E_lambda { binder = {var ; ascr ; attributes=_};
                                                   result }) ;
                        location = _ }) ;
                 collect ] as arguments} ->
      (* The effect should be provided by the E_let_in otherwise there is a failure in the detection *)
      (match returned_effect with Some (effects) ->
      let cons_name = (match cons_name with
          C_LIST_ITER -> C_LIST_FOLD
        | C_MAP_ITER  -> C_MAP_FOLD
        | C_SET_ITER  -> C_SET_FOLD
        | C_ITER      -> C_FOLD
        | _ -> failwith "impossible") in
      (* This handle the fact the the fonction returns a unit and the `let () =` becomes `let (effect,()) =`*)
      let effects_binder = ValueVar.fresh ~name:"effects_binder" () in
      let result = self ?returned_effect result in
      let binder,result = (match get_e_record effects with Some (effects_rec) -> (* list of effect *)
      let effects_lmap = LMap.map (fun e -> let var = get_e_variable_exn e in  {var; ascr = Some e.type_expression;attributes={const_or_var=None}}) (effects_rec) in
        effects_binder, add_to_the_top_of_function_body effects_binder effects_lmap effects.type_expression result
      | None -> get_e_variable_exn effects, result (* single effect *)
      ) in
      let effects = e_a_pair effects (e_a_unit ()) in
      let effects_lmap = LMap.of_list @@ [(Label "0",{var=binder;ascr=Some(effects.type_expression);attributes={const_or_var=None}});(Label "1",{var=ValueVar.fresh ~name:"()" ();ascr=Some (t_unit ());attributes={const_or_var=None}})] in
      (* modify parameters of lambda *)
      let lambda = (
        (* match effect_binder with (...)*)
        let effecs_binder = ValueVar.fresh ~name:"effects_binder" () in
        let result = add_to_the_top_of_function_body effecs_binder effects_lmap effects.type_expression result in
        (* match iterm_param with (effect_binder,var) -> *)
        let iter_param = ValueVar.fresh ~name:"iter_param" () in
        let params = e_a_pair (e_variable effecs_binder effects.type_expression) (e_variable var (Option.value_exn ascr)) in
        let params_lmap = LMap.of_list @@ [(Label "0",{var=effecs_binder;ascr=Some(effects.type_expression);attributes={const_or_var=None}});(Label "1",{var;ascr;attributes={const_or_var=None}})] in
        let result = add_to_the_top_of_function_body iter_param params_lmap params.type_expression result in
        let ty = t_arrow (params.type_expression) result.type_expression () in
        e_lambda {binder={var=iter_param;ascr=Some(params.type_expression);attributes={const_or_var=None}};result} ty
      ) in
      return_1 @@ e_constant {cons_name;arguments=[lambda;collect;effects]} effects.type_expression
      (* Normal usage of ITERATORS *)
      | None ->
        let arguments = List.map ~f:self arguments in
        return ?returned_effect @@ E_constant {cons_name;arguments}
      )
  | E_constant {cons_name;arguments} ->
      let arguments = List.map ~f:self arguments in
      return ?returned_effect @@ E_constant {cons_name;arguments}
  | E_application {lamb;args} ->
      (match returned_effect with None -> return ?returned_effect @@ E_application {lamb;args}
      (* By hypothesis 3 we can't have a annonymous function after this *)
      | _ ->
      let ret_effect,e = morph_function_application effect e in
      match ret_effect with None -> return_1 ?returned_effect @@ e
      | Some (effects,ret_effect) ->
        let effect_type = ret_effect.type_expression in
        match returned_effect with
          (* optimize when ret_effect = returned_effect *)
          Some (returned_effect) when Compare.expression returned_effect ret_effect = 0 -> return_1 @@ e
        |  _ ->
        let func_ret = ValueVar.fresh ~name:"func_ret" () in
        let func_binder = {var=func_ret;ascr=Some e.type_expression;attributes={const_or_var=None}} in
        let res = return ?returned_effect @@ E_variable func_ret in
        return @@ match_on_write_effect func_binder e res effects effect_type
      )
  | E_lambda {binder;result} ->
      let result = self ?returned_effect result in
      return @@ E_lambda {binder;result}
  | E_recursive {fun_name;fun_type;lambda={binder;result}} ->
      let result = self ?returned_effect result in
      return @@ E_recursive {fun_name;fun_type;lambda={binder;result}}
      (*
    (* This detect let () = x := a in res, an assignation in a sequence *)
  | E_let_in {let_binder=_;rhs={expression_content=E_assign{variable;access_path=_;expression};_};let_result;attr} ->
      (* let effect = if DefinedVariable.mem def_vars variable then effect else *)
      let effect = Effect.add_var effect (variable,expression.type_expression) in
      let let_binder = variable in
      let _,_,rhs = self expression in
      let ret_effect,effect,let_result = self ~def_vars ~effect let_result in
      return ret_effect effect @@ E_let_in {let_binder;rhs;let_result;attr}
      *)
  | E_let_in {let_binder;rhs;let_result;attr} ->
      (match Effect.get_effect_var effect let_binder.var with
      (* No assignation in rhs *)
        None,_ ->
          let rhs = self rhs in
          let let_result = self ?returned_effect let_result in
          return @@ E_let_in {let_binder;rhs;let_result;attr}

      (* assignation in rhs and rhs is evaluated *)
      | Some (effects,returned_effect'),None ->
          let effect_type = returned_effect'.type_expression in
          let let_result = self ?returned_effect let_result in
          let rhs = self ~returned_effect:returned_effect' rhs in
          return @@ match_on_write_effect let_binder rhs let_result effects effect_type
      (* rhs is not evaluated (function definition) and contains assignation *)
      | Some (_,returned_effect'),Some (read_effects,read_effect) ->
        let let_result = self ?returned_effect let_result in
        let rhs = self ~returned_effect:returned_effect' rhs in
        let rhs = add_parameter read_effect read_effects returned_effect'.type_expression rhs in
        return @@ E_let_in {let_binder;rhs;let_result;attr}
      )
  | E_type_inst {type_;forall} ->
      let forall = self ?returned_effect forall in
      return @@ E_type_inst {type_;forall}
  | E_type_abstraction {type_binder;result} ->
      let result = self ?returned_effect result in
      return @@ E_type_abstraction {type_binder;result}
  (* Variant *)
  | E_constructor {constructor;element} ->
    let element = self element in
    return ?returned_effect @@ E_constructor {constructor;element}
  | E_matching {matchee;cases} ->
    let matchee = self matchee in
    let cases = (match cases with
        Match_variant {cases;tv} ->
          let cases = List.map ~f:(fun {constructor;pattern;body} ->
            let body = self ?returned_effect body in
            {constructor;pattern;body}
            ) cases in
          Match_variant {cases;tv}
      | Match_record {fields;body;tv}->
          let body = self ?returned_effect body in
          Match_record {fields;body;tv}
      ) in
    return @@ E_matching {matchee;cases}
  (* Record *)
  | E_record record ->
      let record = LMap.map self record in
      return ?returned_effect @@ E_record record
  | E_record_accessor {record;path} ->
      let record = self record in
      return ?returned_effect @@ E_record_accessor {record;path}
  | E_record_update {record;path;update} ->
      let record = self record in
      let update = self update in
      return ?returned_effect @@ E_record_update {record;path;update}
  (* Todo : check if we can replace by morphin directly let () = x := e in into let x = e in *)
  | E_assign {binder;access_path;expression} ->
      let expression = self expression in
      let let_binder = binder in
      let attr = {inline = false; no_mutation = false; view = false; public = false; thunk = false; hidden = false} in
      (* This part is similar to desugaring of fonctional update. But is kept here for not wanting to desugar it if we
        keep the effect in the backend *)
      let rhs =
        let accessor expr a : expression =
          match a with
            Access_tuple  i ->
              let _ty =
                let record_ty = get_t_record_exn expr.type_expression in
                LMap.find (Label (Z.to_string i)) record_ty.content
              in
              e_record_accessor {record=expr;path=(Label (Z.to_string i))} @@ t_unit ()
          | Access_record a ->
              let _ty =
                let record_ty = get_t_record_exn expr.type_expression in
                LMap.find (Label a) record_ty.content
              in
              e_record_accessor {record=expr;path=(Label a)} @@ t_unit ()
          | Access_map k ->
            let k = self k in
            let _ty = snd @@ get_t_map_exn expr.type_expression in
            e_constant {cons_name=C_MAP_FIND_OPT;arguments=[k;expr]} @@ t_unit ()
        in
        let updator s a expr : expression =
          match a with
            Access_tuple  i -> e_record_update {record=s;path=(Label (Z.to_string i));update=expr} s.type_expression
          | Access_record a -> e_record_update {record=s;path=(Label a);update=expr} s.type_expression
          | Access_map k ->
            let k = self k in
            e_constant {cons_name=C_MAP_ADD;arguments=[k;expr;s]} s.type_expression
        in
        let aux (s, e : expression * _) lst =
          let s' = accessor s lst in
          let e' = fun expr ->
            let u = updator s lst expr in
            e u
          in
          (s',e')
        in
        let (_,rhs) = List.fold ~f:aux ~init:(e_variable binder.var (Option.value_exn binder.ascr), fun e -> e) access_path in
        rhs @@ expression in
      (* Todo : Check for correct use *)
      let let_result = return ?returned_effect @@ e_unit () in
      return @@ E_let_in {let_binder;rhs;let_result;attr}

let rec silent_cast_top_level_var_to_const ~add_warning e =
  let self = silent_cast_top_level_var_to_const ~add_warning in
  match e.expression_content with
    E_let_in {let_binder;rhs;let_result;attr} ->
    let let_result = self let_result in
    (match let_binder.attributes.const_or_var with
      Some `Var -> add_warning @@ `Jsligo_deprecated_toplevel_let (ValueVar.get_location let_binder.var)
    | _ -> ()
    );
    let let_binder = {let_binder with attributes={const_or_var = Some `Const}} in
    let expression_content = E_let_in {let_binder;rhs;let_result;attr} in
    {e with expression_content}
  | _ -> e

let expression ~add_warning e =
  (* Pretreatement especialy for JSLigo replace top-level let to const *)
  let e = silent_cast_top_level_var_to_const ~add_warning e in
  let e = Deduplicate_binders.program e in
  let effect = detect_effect_in_expression ValueVarSet.empty e in
  let e = morph_expression effect e in
  e
