open Ligo_prim
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

  let add_effect t (ev : ValueVar.t) (type_ : type_expression) =
  {t with global = {t.global with effects = ValueVarMap.add ev type_ t.global.effects}}

  let add_read_effect (t:t) (ev : ValueVar.t) (type_ : type_expression) =
    {t with global = {t.global with read = Some(match t.global.read with None -> ValueVarMap.singleton ev type_ | Some (read) -> ValueVarMap.add ev type_ read)}}

  let add_effects t ev_set =
    let current = t.global.effects in
    let new_    = ValueVarMap.union (fun _ a _ -> Some (a)) current ev_set in
    { t with global = {t.global with effects=new_}}

  let store_binders_effects_func (t : t) (binder : ValueVar.t) =
    let global = t.global in
    let t = {
      env = ValueVarMap.add binder t.global t.env;
      global = global;
    } in
    t

  let store_binders_effects_var (t : t) (binder : ValueVar.t) =
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
      (PP_helpers.list_sep_d ValueVar.pp) (ValueVarMap.keys payload.effects)
      (Format.pp_print_option ~none:(fun ppf () -> Format.fprintf ppf "None") (fun ppf -> Format.fprintf ppf "(%a)" (PP_helpers.list_sep_d ValueVar.pp))) (Option.map ~f:ValueVarMap.keys payload.read)
    in
    Format.fprintf ppf "Effects : @.
    @[{ env : %a @.
    global : %a } @]\n%!"
    PP_helpers.(list_sep_d (fun ppf (a,b) -> Format.fprintf ppf "%a => %a" ValueVar.pp a pp_global b) ) (ValueVarMap.to_kv_list t.env)
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
    | _ -> e_record (Record.of_list @@ List.map ~f:(fun (i,e) -> Label.of_int i,e) tuple)
      @@ t_record ~layout:L_tree @@ Record.of_list @@ List.map ~f:(fun (i,t) ->
       (Label.of_int i,
       (Rows.{associated_type=t ; michelson_annotation=None ; decl_pos = 0} : row_element)))tuple_type

  let get_read_effect (t:t) (ev : ValueVar.t) =
    match ValueVarMap.find_opt ev t.env with
      None -> None
    | Some ({effects=_; read=None}) -> None
    | Some {effects=_; read=Some(read)} ->
      if ValueVarMap.(compare compare_type_expression read empty) = 0
        then None else Some (make_tuple read)

  let get_effect_var (t:t) (ev : ValueVar.t) =
    (* Format.printf "get effect for %a in %a\n%!" PP.ValueVar.t ev pp t; *)
    match ValueVarMap.find_opt ev t.env with
      None -> (None,None)
    | Some ({effects;read}) ->
      let aux effects =
        let _,map = ValueVarMap.fold (
          fun ev t (i,map) ->
            let map = (i,(ev,t)) :: map in
            i+1,map
        ) effects (0,[]) in
        let effects' = Record.of_list @@ List.map ~f:(fun (i,(var,ascr)) -> Label.of_int i,({var;ascr;attributes={const_or_var=None}}: _ Binder.t)) map in
        effects', make_tuple effects
      in
      let empty_to_none f a = if ValueVarMap.is_empty a then None else Some (f a)  in
      empty_to_none aux effects, Option.map ~f:aux read

  let rm_var (ev : ValueVar.t) (t : t) =
    let read = match t.global.read with None -> None
    | Some (read) ->
      let read = ValueVarMap.remove ev read in
      Some (read)
    in
    let global = {effects = ValueVarMap.remove ev t.global.effects; read} in
    {t with global = global}
  let remove_read_effect (t:t) = {t with global = {t.global with read = None}}
  let load_write_effect_in_read_effect (t: t) =
    let read = Option.value ~default:ValueVarMap.empty t.global.read in
    let global = {t.global with read = Some (ValueVarMap.union (fun _ a _ -> Some a) t.global.effects read)} in
    {t with global = global}
end

module ValueVarSet = Caml.Set.Make(ValueVar)
let rec detect_effect_in_expression (mut_var : ValueVarSet.t) (e : expression) =
  (* Format.printf "detect_effect_in_expression %a with mut_var %a\n%!" PP.expression e (Format.pp_print_seq ~pp_sep:(fun ppf () -> Format.fprintf ppf ",") PP.ValueVar.t) (ValueVarSet.to_seq mut_var); *)
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
      let effect = self result in
      let effect =  Effect.load_write_effect_in_read_effect effect in
      Effect.rm_var binder.var @@ Effect.rm_var fun_name effect
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
        let mut_var = Record.fold (fun mut_var ({var;ascr=_;attributes;}:_ Binder.t) -> (match attributes.const_or_var with
          Some (`Var) -> ValueVarSet.add var mut_var | _ -> mut_var) ) mut_var fields in
        let effect = self ~mut_var body in
        let effect = Record.fold (fun e b -> Effect.rm_var b.Binder.var e) effect fields in
        effect
      ) in
      effect
  (* Record *)
  | E_record map ->
    Effect.concat @@ List.map ~f:(fun e -> self e) @@ Record.LMap.to_list map
  | E_accessor {record;path=_} -> self record
  | E_update {record;path=_;update} ->
    let effect = self record in
    let effect = Effect.add effect @@ self update in
    effect
  | E_assign {binder;expression} ->
    let effect = self expression in
    return @@ Effect.add_effect effect binder.var binder.ascr

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
    E_lambda {binder;output_type;result}->
      let result = self result in
      return @@ E_lambda {binder;output_type;result}
  | _  ->
    e_matching {matchee = e_variable binder effects_type;
      cases = Match_record {fields = effects; body = e; tv = effects_type}}
    e.type_expression

(* Change 'a -> 'b -> .... -> 'return into 'read_effect_type -> 'a -> 'b -> ... -> 'effect_type * 'return *)
let morph_function_type (read_effect_type : type_expression option) (effect_type : type_expression option) (fun_type : type_expression) =
  let rec add_to_retun_type effect_type fun_type =
    let self = add_to_retun_type effect_type in
    let return ascr ret type_content = {fun_type with type_content},ascr,ret in
    match fun_type.type_content with
      T_variable _ | T_constant _ | T_sum _ | T_record _ | T_singleton _ ->
        (match effect_type with None -> fun_type,t_unit(),fun_type | Some (e) -> let t = t_pair e fun_type in t,t_unit(),t)
    | T_arrow {type1;type2} ->
      let type2,_,_ = self type2 in
      return type1 type2 @@ T_arrow {type1;type2}
    | T_for_all {ty_binder;kind;type_} ->
      let type_,ascr,ret = self type_ in
      return ascr ret @@ T_for_all {ty_binder;kind;type_}
  in
  match read_effect_type with
    None -> add_to_retun_type effect_type fun_type
  | Some (read_effect_type) ->
    let ret,_,_ = (add_to_retun_type effect_type fun_type) in
    t_arrow read_effect_type ret (), read_effect_type, ret

let add_parameter read_effect read_effects effects_type rhs =
  (* Format.printf "add_parameters to rhs: %a" PP.expression rhs; *)
  match rhs.expression_content with
    E_lambda _ ->
      let lambda_type,ascr,output_type = morph_function_type (Some read_effect.type_expression) (Some effects_type) rhs.type_expression in
      let var = ValueVar.fresh ~name:"effect_binder" () in
      let binder : _ Binder.t = {var;ascr;attributes=Binder.empty_attribute} in
      let result = add_to_the_top_of_function_body var read_effects read_effect.type_expression rhs in
      e_lambda {binder;output_type;result} lambda_type
  | E_recursive {fun_name;fun_type;lambda={binder;result}} ->
      let fun_type,ascr,output_type = morph_function_type (Some read_effect.type_expression) (Some effects_type) fun_type in
      let var = ValueVar.fresh ~name:"effect_binder" () in
      let binder_eff : _ Binder.t = {var;ascr;attributes=Binder.empty_attribute} in
      let result = add_to_the_top_of_function_body var read_effects read_effect.type_expression result in
      let result = e_lambda {binder;output_type;result} rhs.type_expression in
      e_recursive {fun_name;fun_type;lambda={binder=binder_eff;output_type;result}} fun_type
  | _ -> failwith "Add_parameters: not a function"

let rec morph_function_application (effect : Effect.t) (e: expression) : _ * expression =
  let self = morph_function_application effect in
  let return returned_effect type_expression expression_content = returned_effect, { e with expression_content ; type_expression } in
  match e.expression_content with
  | E_variable variable ->
      (match Effect.get_effect_var effect variable with
        returned_effect,None ->
          let type_,_,_ = morph_function_type None (Option.map ~f:(fun (_,x) -> x.type_expression) returned_effect) e.type_expression in
          return returned_effect type_ @@ E_variable variable
      | returned_effect,Some (_,read_effects) ->
          let type_,_,type2 = morph_function_type (Some read_effects.type_expression) (Option.map ~f:(fun (_,x) -> x.type_expression) returned_effect) e.type_expression in
          let e     = e_variable variable type_ in
          return returned_effect type2 @@ E_application {lamb=e;args=read_effects})
  | E_type_inst {forall;type_} ->
      let returned_effect,forall = self forall in
      let {Abstraction.ty_binder;kind=_;type_=ty} = get_t_for_all_exn forall.type_expression in
      let ty = Helpers.subst_type ty_binder type_ ty in
      return returned_effect ty @@ E_type_inst {forall;type_}
  | E_application {lamb;args} ->
      let returned_effect,lamb = self lamb in
      let Arrow.{type1=_;type2} = get_t_arrow_exn lamb.type_expression in
      return returned_effect type2 @@ E_application {lamb;args}
  | _ -> failwith "Hypothesis 3 don't hold"

let match_on_write_effect let_binder rhs let_result effects effect_type =
  let effect_var = ValueVar.fresh ~name:"effect_binder" () in
  let effect_binder : _ Binder.t = {var=effect_var;ascr=effect_type;attributes={const_or_var=None}} in
  let let_result = e_matching {matchee=e_variable effect_var effect_type;cases=
    Match_record {fields=effects;body=let_result;tv=effect_type}} let_result.type_expression in
  let tv = t_pair effect_type rhs.type_expression in
  E_matching {matchee=rhs;cases=
    Match_record {
      fields=Record.of_list
      [
        (Label.of_int 0,effect_binder);
        (Label.of_int 1,let_binder)
      ];
      body=let_result;tv}
    }

let rec morph_expression ?(returned_effect) (effect : Effect.t) (e: expression) : expression =
  (* Format.printf "Morph_expression %a with effect : (%a)\n%!" PP.expression e PP.(Ligo_prim.PP.option_type_expression expression) returned_effect; *)
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
        None -> return ?returned_effect @@ E_variable variable
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
      let cons_name : Constant.constant' = (match cons_name with
          C_LIST_ITER -> C_LIST_FOLD
        | C_MAP_ITER  -> C_MAP_FOLD
        | C_SET_ITER  -> C_SET_FOLD
        | C_ITER      -> C_FOLD
        | _ -> failwith "impossible") in
      (* This handle the fact the the fonction returns a unit and the `let () =` becomes `let (effect,()) =`*)
      let effects_binder = ValueVar.fresh ~name:"effects_binder" () in
      let result = self ?returned_effect result in
      let binder,result = (match get_e_record effects with Some (effects_rec) -> (* list of effect *)
      let effects_lmap = Record.map (fun e -> let var = get_e_variable_exn e in  ({var; ascr =e.type_expression;attributes={const_or_var=None}}: _ Binder.t)) (effects_rec) in
        effects_binder, add_to_the_top_of_function_body effects_binder effects_lmap effects.type_expression result
      | None -> get_e_variable_exn effects, result (* single effect *)
      ) in
      let effects = e_a_pair effects (e_a_unit ()) in
      let effects_lmap = Record.of_list @@ [(Label "0",({var=binder;ascr=(effects.type_expression);attributes={const_or_var=None}}: _ Binder.t));(Label "1",{var=ValueVar.fresh ~name:"()" ();ascr=(t_unit ());attributes={const_or_var=None}})] in
      (* modify parameters of lambda *)
      let lambda = (
        (* match effect_binder with (...)*)
        let effecs_binder = ValueVar.fresh ~name:"effects_binder" () in
        let result = add_to_the_top_of_function_body effecs_binder effects_lmap effects.type_expression result in
        (* match iterm_param with (effect_binder,var) -> *)
        let iter_param = ValueVar.fresh ~name:"iter_param" () in
        let params = e_a_pair (e_variable effecs_binder effects.type_expression) (e_variable var ascr) in
        let params_lmap = Record.of_list @@ [(Label "0",({var=effecs_binder;ascr=(effects.type_expression);attributes={const_or_var=None}}: _ Binder.t));(Label "1",{var;ascr;attributes={const_or_var=None}})] in
        let result = add_to_the_top_of_function_body iter_param params_lmap params.type_expression result in
        let ty = t_arrow (params.type_expression) result.type_expression () in
        e_lambda {binder={var=iter_param;ascr=(params.type_expression);attributes={const_or_var=None}};output_type=result.type_expression;result} ty
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
          Some (returned_effect) when equal_expression returned_effect ret_effect -> return_1 @@ e
        |  _ ->
        let func_ret = ValueVar.fresh ~name:"func_ret" () in
        let func_binder : _ Binder.t = {var=func_ret;ascr=e.type_expression;attributes={const_or_var=None}} in
        let res = return ?returned_effect @@ E_variable func_ret in
        return @@ match_on_write_effect func_binder e res effects effect_type
      )
  | E_lambda {binder;output_type;result} ->
      let result = self ?returned_effect result in
      return @@ E_lambda {binder;output_type;result}
  | E_recursive {fun_name;fun_type;lambda={binder;output_type;result}} ->
      let result = self ?returned_effect result in
      return @@ E_recursive {fun_name;fun_type;lambda={binder;output_type;result}}
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
      let record = Record.map self record in
      return ?returned_effect @@ E_record record
  | E_accessor {record;path} ->
      let record = self record in
      return ?returned_effect @@ E_accessor {record;path}
  | E_update {record;path;update} ->
      let record = self record in
      let update = self update in
      return ?returned_effect @@ E_update {record;path;update}
  (* Todo : check if we can replace by morphin directly let () = x := e in into let x = e in *)
  | E_assign {binder;expression} ->
      let expression = self expression in
      let let_binder = binder in
      let attr = ValueAttr.{inline = false; no_mutation = false; view = false; public = false; hidden = false; thunk = false} in
      let rhs = expression in
      (* Todo : Check for correct use *)
      let let_result = return ?returned_effect @@ e_unit () in
      return @@ E_let_in {let_binder;rhs;let_result;attr}

let rec silent_cast_top_level_var_to_const ~raise e =
  let self = silent_cast_top_level_var_to_const ~raise in
  match e.expression_content with
    E_let_in {let_binder;rhs;let_result;attr} ->
    (match let_binder.attributes.const_or_var with
      Some `Var -> raise.Trace.warning @@ `Jsligo_deprecated_toplevel_let (ValueVar.get_location let_binder.var)
    | _ -> ()
    );
    let let_result = self let_result in
    let let_binder = {let_binder with attributes={const_or_var = Some `Const}} in
    let expression_content = E_let_in {let_binder;rhs;let_result;attr} in
    {e with expression_content}
  | _ -> e

let expression ~raise e =
  (* Pretreatement especialy for JSLigo replace top-level let to const *)
  let e = silent_cast_top_level_var_to_const ~raise e in
  let e = Deduplicate_binders.program e in
  let effect = detect_effect_in_expression ValueVarSet.empty e in
  let e = morph_expression effect e in
  e
