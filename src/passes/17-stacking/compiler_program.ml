open Tezos_micheline.Micheline
module Location = Simple_utils.Location

type meta = Mini_c.meta

type compiled_expression =
  { expr_ty : (meta, string) node
  ; expr : (meta, string) node
  }

let null = Mini_c.dummy_meta
let rec repeat x n = if n <= 0 then [] else x :: repeat x (n - 1)

type base_type = (meta, string) Tezos_micheline.Micheline.node
type oty = (meta, base_type) Ligo_coq_ocaml.Compiler.ty

let get_ty_meta : oty -> meta =
 fun x ->
  match x with
  | T_base (meta, _) -> meta
  | T_unit meta -> meta
  | T_pair (meta, _, _, _, _) -> meta
  | T_or (meta, _, _, _, _) -> meta
  | T_func (meta, _, _) -> meta
  | T_lambda (meta, _, _) -> meta
  | T_option (meta, _) -> meta
  | T_list (meta, _) -> meta
  | T_set (meta, _) -> meta
  | T_map (meta, _, _) -> meta
  | T_big_map (meta, _, _) -> meta
  | T_ticket (meta, _) -> meta
  | T_contract (meta, _) -> meta
  | T_bool meta -> meta
  | T_int meta -> meta
  | T_nat meta -> meta
  | T_mutez meta -> meta
  | T_string meta -> meta
  | T_bytes meta -> meta
  | T_address meta -> meta
  | T_key_hash meta -> meta
  | T_operation meta -> meta


(* returns a list `ret` such that `select r ret = env` by inserting
   None entries, probably should do this in Coq instead but
   whatever *)
let rec unselect (r : bool list) (env : 'a option list) : 'a option list =
  match r with
  | [] -> []
  | false :: r -> None :: unselect r env
  | true :: r ->
    (match env with
    | [] -> None :: unselect r []
    | x :: env -> x :: unselect r env)


let with_var_names (r : bool list) (env : oty list) (m : meta) : meta =
  { m with env = unselect r (List.map ~f:(fun ty -> (get_ty_meta ty).binder) env) }


let compile_binds env expr =
  Ligo_coq_ocaml.Compiler.compile_binds
    null
    with_var_names
    To_micheline.literal_code
    To_micheline.global_constant
    (repeat true (List.length env))
    env
    expr


let compile_expr env expr =
  Ligo_coq_ocaml.Compiler.compile_expr
    null
    with_var_names
    To_micheline.literal_code
    To_micheline.global_constant
    (repeat true (List.length env))
    env
    expr


let compile_expr env e =
  let open Lwt.Let_syntax in
  let p = compile_expr env e in
  let rs, p = To_micheline.strengthen_prog p [ true ] in
  (* TODO *)
  let () =
    if List.length (List.filter ~f:Fn.id rs) > 0
    then failwith "TODO expr used something"
    else ()
  in
  (* hmm, why did this end up here *)
  let drops = Ligo_coq_ocaml.Compiler.compile_ope null rs in
  let%bind drops = To_micheline.translate_prog drops in
  let%map p = To_micheline.translate_prog p in
  Seq (null, drops @ p)


let compile_function_body e =
  let open Lwt.Let_syntax in
  let p = compile_binds [] e in
  let rs, p = To_micheline.strengthen_prog p [ true ] in
  let%map p = To_micheline.translate_prog p in
  (* hmm, why did this end up here *)
  if Option.value ~default:false (List.hd rs)
  then Seq (null, p)
  else Seq (null, Prim (null, "DROP", [], []) :: p)
