module I = Mini_c
module O = Ligo_coq_ocaml.Compiler
module Micheline = Tezos_micheline.Micheline
module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Option = Simple_utils.Option
module Var = Ligo_prim.Value_var
module Errors = Errors

type meta = Mini_c.meta
type binder_meta = Mini_c.binder_meta

(* We should use this less: *)
let nil : meta =
  { location = Location.generated
  ; env = []
  ; binder = None
  ; source_type = None
  ; application = None
  }


type base_type = (meta, string) Micheline.node
type oty = (meta, base_type) O.ty

let binder_meta (var : Var.t option) (source_type : I.type_expression)
    : binder_meta option
  =
  Option.map var ~f:(fun var : binder_meta ->
      { location = Var.get_location var
      ; name = (if Var.is_generated var then None else Some (Var.to_name_exn var))
      ; source_type = source_type.source_type
      })


(* Next stage uses Micheline for its types: *)
let rec translate_type ?var : I.type_expression -> oty =
 fun a ->
  let nil : meta =
    { location = Location.generated
    ; env = []
    ; binder = binder_meta var a
    ; source_type = None
    ; application = None
    }
  in
  match a.type_content with
  | I.T_tuple ts -> tuple_comb nil ts
  | I.T_or ((ann1, a1), (ann2, a2)) ->
    O.T_or (nil, ann1, ann2, translate_type a1, translate_type a2)
  | I.T_function (a1, a2) -> O.T_func (nil, translate_type a1, translate_type a2)
  | I.T_base I.TB_unit -> T_base (nil, Prim (nil, "unit", [], []))
  | I.T_base I.TB_bool -> T_base (nil, Prim (nil, "bool", [], []))
  | I.T_base I.TB_string -> T_base (nil, Prim (nil, "string", [], []))
  | I.T_base I.TB_bytes -> T_base (nil, Prim (nil, "bytes", [], []))
  | I.T_base I.TB_nat -> T_base (nil, Prim (nil, "nat", [], []))
  | I.T_base I.TB_int -> T_base (nil, Prim (nil, "int", [], []))
  | I.T_base I.TB_mutez -> T_base (nil, Prim (nil, "mutez", [], []))
  | I.T_base I.TB_operation -> T_base (nil, Prim (nil, "operation", [], []))
  | I.T_base I.TB_address -> T_base (nil, Prim (nil, "address", [], []))
  | I.T_base I.TB_key -> T_base (nil, Prim (nil, "key", [], []))
  | I.T_base I.TB_key_hash -> T_base (nil, Prim (nil, "key_hash", [], []))
  | I.T_base I.TB_chain_id -> T_base (nil, Prim (nil, "chain_id", [], []))
  | I.T_base I.TB_signature -> T_base (nil, Prim (nil, "signature", [], []))
  | I.T_base I.TB_timestamp -> T_base (nil, Prim (nil, "timestamp", [], []))
  | I.T_base I.TB_baker_hash -> T_base (nil, Prim (nil, "baker_hash", [], []))
  | I.T_base I.TB_pvss_key -> T_base (nil, Prim (nil, "pvss_key", [], []))
  | I.T_base I.TB_baker_operation -> T_base (nil, Prim (nil, "baker_operation", [], []))
  | I.T_base I.TB_bls12_381_g1 -> T_base (nil, Prim (nil, "bls12_381_g1", [], []))
  | I.T_base I.TB_bls12_381_g2 -> T_base (nil, Prim (nil, "bls12_381_g2", [], []))
  | I.T_base I.TB_bls12_381_fr -> T_base (nil, Prim (nil, "bls12_381_fr", [], []))
  | I.T_base I.TB_never -> T_base (nil, Prim (nil, "never", [], []))
  | I.T_base I.TB_tx_rollup_l2_address ->
    T_base (nil, Prim (nil, "tx_rollup_l2_address", [], []))
  | I.T_base (I.TB_type_int memo_size) -> T_base (nil, Int (nil, memo_size))
  | I.T_ticket x -> T_ticket (nil, translate_type x)
  | I.T_sapling_transaction memo_size ->
    T_base (nil, Prim (nil, "sapling_transaction", [ Int (nil, memo_size) ], []))
  | I.T_sapling_state memo_size ->
    T_base (nil, Prim (nil, "sapling_state", [ Int (nil, memo_size) ], []))
  | I.T_map (a1, a2) -> T_map (nil, translate_type a1, translate_type a2)
  | I.T_big_map (a1, a2) -> T_big_map (nil, translate_type a1, translate_type a2)
  | I.T_list a -> T_list (nil, translate_type a)
  | I.T_set a -> T_set (nil, translate_type a)
  | I.T_contract a -> T_contract (nil, translate_type a)
  | I.T_option a -> T_option (nil, translate_type a)


(* could consider delaying this to the next pass, in Coq, but
   currently the Coq pass type translation is the identity *)
and tuple_comb_ann nil ts =
  match ts with
  | [] -> None, O.T_unit nil
  | [ (ann, t) ] -> ann, translate_type t
  | (ann1, t1) :: ts ->
    let t1 = translate_type t1 in
    let ann, ts = tuple_comb_ann nil ts in
    None, O.T_pair (nil, ann1, ann, t1, ts)


and tuple_comb nil ts = snd (tuple_comb_ann nil ts)

let rec int_to_nat (x : int) : Ligo_coq_ocaml.Datatypes.nat =
  if x <= 0 then O else S (int_to_nat (x - 1))


let translate_var (x : I.var_name) (env : I.environment) =
  let _, idx =
    match I.Environment.Environment.get_i_opt x env with
    | Some v -> v
    | None ->
      failwith
      @@ Format.asprintf "Corner case: %a not found in env" Ligo_prim.Value_var.pp x
  in
  int_to_nat idx


(* probably should use result monad for conformity? but all errors
   here are supposed to be impossible, under the assumption that the
   input program is well-typed *)
let internal_error loc msg =
  failwith
    (Format.asprintf
       "@[<v>Internal error, please report this as a bug@ %s@ %s@ @]"
       loc
       msg)


let extract_applications (expr : I.expression) (env : I.environment)
    : I.application_meta option
  =
  match expr.type_expression.source_type, expr.content with
  (* If an expression is a function and it's an application
     then we're interested in it *)
  | Some { type_content = T_arrow _; _ }, E_application (f, args) ->
    let applied_function =
      match f.content with
      | E_variable var_name | E_deref var_name -> Some var_name
      | _ -> None
    in
    let args =
      (* After uncurrying in self_mini_c some applications
         are changed from `f a b c` to `f (a, b, c)`.
         We can differ them by location
         (location for uncurried tuple would be generated) *)
      if Location.is_dummy_or_generated args.location
      then (
        match args.content with
        | E_tuple args -> args
        | _ -> [ args ])
      else [ args ]
    in
    let to_applied_argument (expr : I.expression) : I.applied_argument =
      match expr.content with
      | E_variable var | E_deref var ->
        let orig_var =
          (* We want to have a location of binded variable *)
          Option.value ~default:var (I.Environment.Environment.get_var_opt var env)
        in
        Var orig_var
      | _ -> Expression_location expr.location
    in
    let arguments =
      List.map args ~f:(fun expr ->
          expr.type_expression.source_type, to_applied_argument expr)
    in
    Some { I.applied_function; I.arguments }
  | _ -> None


(* The translation. Given an expression in an environment, returns a
   "co-de Bruijn" expression with an embedding (`list usage`) showing
   which things in the environment were used. *)

(* Let |-I and |-O be the input and output typing judgments. If
   env |-I expr : a, and translate_expression expr env = (expr', us), then
   select us env |-O expr' : a. *)
let rec translate_expression ~raise ~proto (expr : I.expression) (env : I.environment)
    : (meta, base_type, Ligo_prim.Literal_value.t, (meta, string) Micheline.node) O.expr
  =
  let meta : meta =
    { location = expr.location
    ; env = []
    ; binder = None
    ; source_type = expr.type_expression.source_type
    ; application = extract_applications expr env
    }
  in
  let ty = expr.type_expression in
  let translate_expression = translate_expression ~raise ~proto in
  let translate_args = translate_args ~raise ~proto in
  let translate_binder = translate_binder ~raise ~proto in
  let translate_binder2 = translate_binder2 ~raise ~proto in
  let translate_binderN = translate_binderN ~raise ~proto in
  match expr.content with
  | E_literal lit -> O.E_literal (meta, lit)
  | E_variable x -> E_var (meta, translate_var x env)
  | E_closure { binder; body } ->
    let binder_type, return_type =
      (* TODO move binder type to the binder, like all other binders? *)
      (* at the moment, this is the only error here! so I am not
         bothering with error machinery... *)
      match Mini_c.get_t_function expr.type_expression with
      | None -> internal_error __LOC__ "type of lambda is not a function type"
      | Some t -> t
    in
    let binder = binder, binder_type in
    let binder = translate_binder (binder, body) env in
    O.E_lam (meta, binder, translate_type return_type)
  | E_rec { rec_binder; func = { binder; body } } ->
    let binder_type, return_type =
      (* TODO move binder type to the binder, like all other binders? *)
      (* at the moment, this is the only error here! so I am not
         bothering with error machinery... *)
      match Mini_c.get_t_function expr.type_expression with
      | None -> internal_error __LOC__ "type of lambda is not a function type"
      | Some t -> t
    in
    let binder = binder, binder_type in
    let rec_binder = rec_binder, expr.type_expression in
    let binder = translate_binder2 ((binder, rec_binder), body) env in
    O.E_rec (meta, binder, translate_type return_type)
  | E_constant constant ->
    let mich, args =
      translate_constant ~raise ~proto meta constant expr.type_expression env
    in
    O.E_inline_michelson (meta, mich, args)
  | E_application (f, x) ->
    let args = translate_args [ f; x ] env in
    E_app (meta, args)
  | E_iterator (name, body, expr) ->
    let body = translate_binder body env in
    let expr = translate_expression expr env in
    (match name with
    | C_ITER -> O.E_iter (meta, body, expr)
    | C_MAP -> O.E_map (meta, body, expr)
    | C_LOOP_LEFT ->
      let b = translate_type ty in
      O.E_loop_left (meta, body, b, expr)
    | _ -> internal_error __LOC__ "invalid iterator constant")
  | E_fold (body, coll, init) ->
    let body = translate_binder body env in
    let coll = translate_expression coll env in
    let init = translate_expression init env in
    O.E_fold (meta, init, coll, body)
  | E_fold_right (body, (coll, elem_type), init) ->
    let elem_type = translate_type elem_type in
    let body = translate_binder body env in
    let coll = translate_expression coll env in
    let init = translate_expression init env in
    E_fold_right (meta, elem_type, init, coll, body)
  | E_if_bool (e1, e2, e3) ->
    let e1 = translate_expression e1 env in
    let e2 = translate_expression e2 env in
    let e3 = translate_expression e3 env in
    E_if_bool (meta, e1, e2, e3)
  | E_if_none (e1, e2, e3) ->
    let e1 = translate_expression e1 env in
    let e2 = translate_expression e2 env in
    let e3 = translate_binder e3 env in
    E_if_none (meta, e1, e2, e3)
  (* NB: flipping around because it is backwards in Mini_c *)
  | E_if_cons (e1, e3, e2) ->
    let e1 = translate_expression e1 env in
    let e2 = translate_binder2 e2 env in
    let e3 = translate_expression e3 env in
    E_if_cons (meta, e1, e2, e3)
  | E_if_left (e1, e2, e3) ->
    let e1 = translate_expression e1 env in
    let e2 = translate_binder e2 env in
    let e3 = translate_binder e3 env in
    E_if_left (meta, e1, e2, e3)
  | E_let_in (e1, _inline, e2) ->
    let e1 = translate_expression e1 env in
    let e2 = translate_binder e2 env in
    E_let_in (meta, e1, e2)
  | E_tuple exprs ->
    let exprs = translate_args exprs env in
    E_tuple (meta, exprs)
  | E_let_tuple (e1, e2) ->
    let e1 = translate_expression e1 env in
    let e2 = translate_binderN e2 env in
    E_let_tuple (meta, e1, e2)
  | E_proj (e, i, n) ->
    let e = translate_expression e env in
    E_proj (meta, e, int_to_nat i, int_to_nat n)
  | E_update (e1, i, e2, n) ->
    let args = translate_args [ e2; e1 ] env in
    E_update (meta, args, int_to_nat i, int_to_nat n)
  | E_raw_michelson code ->
    (* maybe should move type into syntax? *)
    let a, b =
      match Mini_c.get_t_function ty with
      | None ->
        internal_error
          __LOC__
          (Format.asprintf
             "type of Michelson insertion ([%%Michelson ...]) is not a function type = %a"
             Mini_c.PP.type_expression
             ty)
      | Some (a, b) -> a, b
    in
    let wipe_locations l e =
      Tezos_micheline.Micheline.(inject_locations (fun _ -> l) (strip_locations e))
    in
    let code = List.map ~f:(wipe_locations nil) code in
    E_raw_michelson (meta, translate_type a, translate_type b, code)
  | E_inline_michelson (code, args') ->
    let args =
      List.map
        ~f:(fun e ->
          ( translate_expression e env
          , Stacking.To_micheline.translate_type (translate_type e.type_expression) ))
        args'
    in
    let wipe_locations l e =
      Tezos_micheline.Micheline.(inject_locations (fun _ -> l) (strip_locations e))
    in
    let code = List.map ~f:(wipe_locations nil) code in
    let used = ref [] in
    let replace m =
      let open Tezos_micheline.Micheline in
      match m with
      | Prim (_, s, [], [ id ])
        when String.equal "typeopt" s && String.is_prefix ~prefix:"$" id ->
        let id = String.chop_prefix_exn ~prefix:"$" id in
        let id = Int.of_string id in
        used := id :: !used;
        (match List.nth args id with
        | Some (_, Prim (_, "option", [ t ], _)) -> t
        | _ -> internal_error __LOC__ (Format.sprintf "could not resolve (typeopt %d)" id))
      | Prim (_, s, [], [ id ])
        when String.equal "type" s && String.is_prefix ~prefix:"$" id ->
        let id = String.chop_prefix_exn ~prefix:"$" id in
        let id = Int.of_string id in
        used := id :: !used;
        (match List.nth args id with
        | None -> internal_error __LOC__ (Format.sprintf "could not resolve (type %d)" id)
        | Some (_, t) -> t)
      | Prim (_, s, [], [ id ])
        when String.equal "litstr" s && String.is_prefix ~prefix:"$" id ->
        let id = String.chop_prefix_exn ~prefix:"$" id in
        let id = Int.of_string id in
        used := id :: !used;
        (match List.nth args id with
        | Some (E_literal (m, Literal_string s), _) -> String (m, Ligo_string.extract s)
        | _ -> internal_error __LOC__ (Format.sprintf "could not resolve (litstr %d)" id))
      | Prim (_, s, [], [ id ])
        when String.equal "codestr" s && String.is_prefix ~prefix:"$" id ->
        let id = String.chop_prefix_exn ~prefix:"$" id in
        let id = Int.of_string id in
        used := id :: !used;
        (match List.nth args id with
        | Some (E_literal (m, Literal_string s), _) ->
          let open Tezos_micheline in
          let code = Ligo_string.extract s in
          let code, errs = Micheline_parser.tokenize code in
          (match errs with
          | _ :: _ ->
            internal_error __LOC__ (Format.sprintf "could not parse raw michelson")
          | [] ->
            let code, errs = Micheline_parser.parse_expression ~check:false code in
            (match errs with
            | _ :: _ ->
              internal_error __LOC__ (Format.sprintf "could not parse raw michelson")
            | [] ->
              let code = Micheline.strip_locations code in
              (* hmm *)
              let code = Micheline.inject_locations (fun _ -> Location.generated) code in
              map_node (fun _ -> m) (fun x -> x) code))
        | _ -> internal_error __LOC__ (Format.sprintf "could not resolve (litstr %d)" id))
      | Prim (a, b, c, d) ->
        let open Tezos_micheline.Micheline in
        let f arg (c, d) =
          match arg with
          | Prim (_, s, [], [ id ])
            when String.equal "annot" s && String.is_prefix ~prefix:"$" id ->
            let id = String.chop_prefix_exn ~prefix:"$" id in
            let id = Int.of_string id in
            used := id :: !used;
            let annot =
              match List.nth args id with
              | Some (E_literal (_, Literal_string s), _) -> Ligo_string.extract s
              | _ ->
                internal_error __LOC__ (Format.sprintf "could not resolve (annot %d)" id)
            in
            c, annot :: d
          | m -> m :: c, d
        in
        let c, d = List.fold_right ~f ~init:([], d) c in
        Prim (a, b, c, d)
      | m -> m
    in
    let code = List.map ~f:(Tezos_utils.Michelson.map replace) code in
    let args' =
      List.filter_mapi
        ~f:(fun i v -> if not (List.mem !used i ~equal:Caml.( = )) then Some v else None)
        args'
    in
    let args' = translate_args args' env in
    E_inline_michelson (meta, code, args')
  | E_global_constant (hash, args) ->
    let args = translate_args args env in
    let output_ty = translate_type ty in
    E_global_constant (meta, output_ty, hash, args)
  | E_create_contract (p, s, code, args) ->
    let p = translate_type p in
    let s = translate_type s in
    let code = translate_binder code [] in
    let args = translate_args args env in
    E_create_contract (meta, p, s, code, args)
  | E_let_mut_in (e1, e2) ->
    let e1 = translate_expression e1 env in
    let e2 = translate_binder e2 env in
    E_let_mut_in (meta, e1, e2)
  | E_deref x -> E_deref (meta, translate_var x env)
  | E_assign (x, e) ->
    let x = translate_var x env in
    let e = translate_expression e env in
    E_assign (meta, x, e)
  | E_for (start, final, incr, body) ->
    let args = translate_args [ start; final; incr ] env in
    let body = translate_binder body env in
    E_for (meta, args, body)
  | E_for_each (coll, _coll_type, body) ->
    let coll = translate_expression coll env in
    let body = translate_binderN body env in
    E_for_each (meta, coll, body)
  | E_while (cond, body) ->
    let cond = translate_expression cond env in
    let body = translate_expression body env in
    E_while (meta, cond, body)


and translate_binder ~raise ~proto (binder, body) env =
  let env' = I.Environment.add binder env in
  let body = translate_expression ~raise ~proto body env' in
  let binder, binder_type = binder in
  O.Binds (nil, [ translate_type ~var:binder binder_type ], body)


and translate_binder2 ~raise ~proto ((binder1, binder2), body) env =
  let env' = I.Environment.add binder1 (I.Environment.add binder2 env) in
  let body = translate_expression ~raise ~proto body env' in
  let binder1, binder1_type = binder1 in
  let binder2, binder2_type = binder2 in
  O.Binds
    ( nil
    , [ translate_type ~var:binder1 binder1_type
      ; translate_type ~var:binder2 binder2_type
      ]
    , body )


and translate_binderN ~raise ~proto (vars, body) env =
  let env' = List.fold_right ~f:I.Environment.add vars ~init:env in
  let body = translate_expression ~raise ~proto body env' in
  O.Binds (nil, List.map ~f:(fun (var, ty) -> translate_type ~var ty) vars, body)


and translate_args ~raise ~proto (arguments : I.expression list) env : _ O.args =
  let arguments = List.rev arguments in
  let arguments =
    List.map
      ~f:(fun argument -> translate_expression ~raise ~proto argument env)
      arguments
  in
  List.fold_right
    ~f:(fun arg args -> O.Args_cons (nil, arg, args))
    arguments
    ~init:(O.Args_nil nil)


and translate_constant
    ~raise
    ~proto
    (meta : meta)
    (expr : I.constant)
    (ty : I.type_expression)
    env
    : _ Micheline.node list * _ O.args
  =
  let module Let_syntax = struct
    let bind : 'a option -> f:('a -> 'b option) -> 'b option =
     fun x ~f ->
      match x with
      | Some x -> f x
      | None -> None
  end
  in
  let ( let* ) x f = Let_syntax.bind ~f x in
  (* Here we will handle some special predefined operators which take
     some "static args". These are mostly types from the typing
     judgment, but also annotations (for SELF, CONTRACT) or scripts
     (for CREATE_CONTRACT.)

     First we translate any static args and return the rest of the
     non-static arguments, if any: *)
  let translate_type t = Stacking.To_micheline.translate_type (translate_type t) in
  let translate_args = translate_args ~raise ~proto in
  (* This is for compatibility with the existing stuff in
     Predefined.Michelson and below. I believe this stuff should be
     simplified away but don't want to do it right now. *)
  let module O = struct
    type static_args = Type_args of string option * (meta, string) Micheline.node list

    let apply_static_args : string -> static_args -> _ Micheline.node =
     fun prim args ->
      match args with
      | Type_args (annot, types) -> Prim (nil, prim, types, Option.to_list annot)


    let wipe_locations l e =
      Tezos_micheline.Micheline.(inject_locations (fun _ -> l) (strip_locations e))
  end
  in
  let open O in
  let special : (static_args * I.expression list) option =
    let return (x : static_args * I.expression list) : _ = Some x in
    match expr.cons_name with
    | C_GLOBAL_CONSTANT ->
      (match expr.arguments with
      | { content = E_literal (Literal_string hash); type_expression = _; _ } :: arguments
        ->
        let hash = Ligo_string.extract hash in
        return
          ( Type_args
              ( None
              , [ translate_type ty; Prim (nil, "constant", [ String (nil, hash) ], []) ]
              )
          , arguments )
      | _ -> None)
    | C_NONE ->
      let* a = Mini_c.get_t_option ty in
      return (Type_args (None, [ translate_type a ]), expr.arguments)
    | C_NIL | C_LIST_EMPTY ->
      let* a = Mini_c.get_t_list ty in
      return (Type_args (None, [ translate_type a ]), expr.arguments)
    | C_LOOP_CONTINUE | C_LEFT ->
      let b =
        match ty.type_content with
        | T_or (_, (_, b)) -> b
        | _ -> failwith (Format.asprintf "WRONG TYPE %a\n%!" Mini_c.PP.type_expression ty)
      in
      (* let* (_, b) = Mini_c.get_t_or ty in *)
      return (Type_args (None, [ translate_type b ]), expr.arguments)
    | C_LOOP_STOP | C_RIGHT ->
      let* a, _ = Mini_c.get_t_or ty in
      return (Type_args (None, [ translate_type a ]), expr.arguments)
    | C_SET_EMPTY ->
      let* a = Mini_c.get_t_set ty in
      return (Type_args (None, [ translate_type a ]), expr.arguments)
    | C_MAP_EMPTY | C_BIG_MAP_EMPTY ->
      let* a, b = Option.(map_pair_or (Mini_c.get_t_map, Mini_c.get_t_big_map) ty) in
      return (Type_args (None, [ translate_type a; translate_type b ]), expr.arguments)
    | C_MAP_REMOVE ->
      let* _, b = Option.(map_pair_or (Mini_c.get_t_map, Mini_c.get_t_big_map) ty) in
      return (Type_args (None, [ translate_type b ]), expr.arguments)
    (* TODO handle CREATE_CONTRACT sooner *)
    (* | C_CREATE_CONTRACT -> *)
    (*   (match expr.arguments with *)
    (*    | { content= E_closure body ; type_expression = closure_ty ; location =_ } :: arguments -> *)
    (*      let* (input_ty, _) = Mini_c.get_t_function closure_ty in *)
    (*      let* (p, s) = Mini_c.get_t_pair input_ty in *)
    (*      let body = translate_closed_function ~raise ~proto body input_ty in *)
    (*      return (Script_arg (O.Script (translate_type p, translate_type s, body)), arguments) *)
    (*    | _ -> None) *)
    | _ -> None
  in
  (* Either we got static args, or none: *)
  let static_args =
    match special with
    | Some (static_args, _) -> static_args
    | None -> O.Type_args (None, [])
  in
  (* Remaining/all non-static args: *)
  let arguments =
    match special with
    | Some (_, arguments) -> arguments
    | None -> expr.arguments
  in
  let arguments = translate_args arguments env in
  match Predefined.Michelson.get_operators proto expr.cons_name with
  | Some x ->
    ( [ (* Handle predefined (and possibly special) operators, applying
         any type/annot/script args using apply_static_args. *)
        Predefined.Michelson.unpredicate
          meta
          (fun prim -> wipe_locations () (apply_static_args prim static_args))
          x
      ]
    , arguments )
  | None ->
    let open Simple_utils.Trace in
    raise.error (Errors.unsupported_primitive expr.cons_name proto)


and translate_closed_function
    ~raise
    ~proto
    ?(env = [])
    ({ binder; body } : I.anon_function)
    input_ty
    : _ O.binds
  =
  let body =
    translate_expression
      ~raise
      ~proto
      body
      (Mini_c.Environment.add (binder, input_ty) env)
  in
  Binds (nil, [ translate_type input_ty ], body)
