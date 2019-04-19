open Trace
open Types
open Combinators

let basic_int_quote_env : environment =
  let e = Environment.empty in
  Environment.add ("input", t_int) e

let statement s' env : statement =
  match s' with
  | S_environment_extend -> s', environment_wrap env (Environment.extend env)
  | S_environment_restrict -> s', environment_wrap env (Environment.restrict env)
  | S_environment_add (name, tv) -> s' , environment_wrap env (Environment.add (name , tv) env)
  | S_cond _ -> s' , id_environment_wrap env
  | S_if_none _ -> s' , id_environment_wrap env
  | S_while _ -> s' , id_environment_wrap env
  | S_patch _ -> s' , id_environment_wrap env
  | S_declaration (name , e) -> s', environment_wrap env (Environment.add (name , (Expression.get_type e)) env)
  | S_assignment (name , e) -> s', environment_wrap env (Environment.add (name , (Expression.get_type e)) env)

let block (statements:statement list) : block result =
  match statements with
  | [] -> simple_fail "no statements in block"
  | lst ->
      let first = List.hd lst in
      let last = List.(nth lst (length lst - 1)) in
      ok (lst, environment_wrap (snd first).pre_environment (snd last).post_environment)

let append_statement' : block -> statement' -> block = fun b s' ->
  let b_wrap = snd b in
  let s = statement s' b_wrap.post_environment in
  let s_wrap = snd s in
  let b_wrap' = { b_wrap with post_environment = s_wrap.post_environment } in
  let b_content = fst b in
  (b_content @ [s], b_wrap')

let prepend_statement : statement -> block -> block = fun s b ->
  let s_wrap = snd s in
  let b_wrap = snd b in
  let b_wrap' = { b_wrap with pre_environment = s_wrap.pre_environment } in
  let b_content = fst b in
  (s :: b_content, b_wrap')

let statements (lst:(environment -> statement) list) e : statement list =
  let rec aux lst e = match lst with
    | [] -> []
    | hd :: tl ->
        let s = hd e in
        s :: aux tl (snd s).post_environment
  in
  aux lst e
