module AST = Ast_aggregated
open Ligo_prim

type 'err ty_exp_mapper = AST.type_expression -> unit

let rows : ('a -> unit) -> AST.rows -> unit = fun g {fields; _} ->
  let _ = Record.LMap.map (fun ({associated_type ; _} : AST.row_element) ->
              let () = g associated_type in
              ()) fields in
  ()

let rec traverse_type_expression : 'err ty_exp_mapper -> AST.type_expression -> unit  = fun f te ->
  let open Ligo_prim in
  let self = traverse_type_expression f in
  let () = f te in
  match te.type_content with
  | T_sum temap -> rows self temap
  | T_for_all x -> self x.type_
  | T_record temap -> rows self temap
  | T_arrow arr ->
     let _ = Arrow.map self arr in
     ()
  | T_variable _ -> ()
  | T_singleton _ -> ()
  | T_constant { parameters } ->
     let _ = List.map ~f:self parameters in
     ()

(* Adapted from lib_protocol/script_string_repr.ml *)
let check_string v =
  let rec check_printable_ascii i =
    if Int.(i < 0) then true
    else
      match v.[i] with
      | '\n' | '\x20' .. '\x7E' -> check_printable_ascii (i - 1)
      | _ -> false
  in
  check_printable_ascii (String.length v - 1)

(*
  check_obj_ligo [blacklist] [t] fails if t hold meta-ligo types or meta-ligo constants sub-terms
  [blacklist] is a list of binder and location which refer to meta-ligo terms, when
  encountering a variable matching an element of this list, it fails
*)
let check_obj_ligo ~raise ?(blacklist = []) (t : AST.expression) : unit =
  let folder_constant () expr = match expr.AST.expression_content with
    | E_variable v -> (
      let b_opt = List.find ~f:(fun (x,_loc) -> Value_var.equal v (Binder.get_var x)) blacklist in
      match b_opt with
      | Some (_,loc) -> raise.Trace.error @@ Errors.expected_obj_ligo loc
      | None -> ()
      )
    | E_constant {cons_name}
         when Constant.ppx_is_only_interpreter cons_name ->
       raise.Trace.error @@ Errors.expected_obj_ligo expr.location
    | E_literal (Literal_string s) when not (check_string @@ Ligo_string.extract s) ->
       raise.Trace.error @@ Errors.expected_obj_ligo expr.location
    | _ -> () in
  let traverser_types loc expr = match expr.AST.type_content with
    | T_constant { injection = Literal_types.Michelson_program ; _ }
    | T_constant { injection = Literal_types.Typed_address     ; _ }
    | T_constant { injection = Literal_types.Mutation          ; _ }
        -> raise.error @@ Errors.expected_obj_ligo loc
    | _ -> () in
  let folder_types () (expr : AST.expression) =
    traverse_type_expression (traverser_types expr.location) expr.type_expression in
  let () = Helpers.fold_expression folder_constant () t in
  let () = Helpers.fold_expression folder_types () t in
  ()


(*
    [purge_meta_ligo] [t] remove any "top-level" let-in bindings holding meta-ligo terms in [t]
    it __strongly__ rely on the fact that an aggregated expression has the following form:

    ```
    let <x_0> = <rhs_0> in
    ...
    let <x_N> = <rhs_N> in
    <rest>
    ```

    [purge_meta_ligo] will check every right-hand sides (<rhs_0> ... <rhs_N>) for meta-ligo
    constructs (primitives or types) in which case it will purge the let-in binding from [t] while
    keeping a list of all "meta-binders" along with their location (blacklist)

    e.g.

    ```
    (* blacklist = [] *)
    let x = Test.log "hello" in
    (* blacklist = [ (x,LOCATION(Test.log "hello")) ] *)
    let y = 1 in
    y + 1
    ```
    |->

    ```
    let y = 1 in
    y + 1
    ```

    when encountering the <rest>, [purge_meta_ligo] will fail on any meta-ligo constructors
    
    e.g.

    ```
    let x = Test.log "hello" in
    let y = 1 in
    x
    ```
    |-> FAIL

    of

    ```
    let y = "hello" in
    (fun _ -> 2) (Test.log y)
    ```
    | -> FAIL
    
*)
let purge_meta_ligo ~raise (t: AST.expression) : AST.expression =
  let rec aux :
      (AST.type_expression Binder.t * Location.t) list ->AST.expression -> (AST.type_expression Binder.t * Location.t) list * AST.expression = fun blacklist expr ->
    match expr.expression_content with
    | E_let_in { let_binder ; rhs ; let_result ; attr } ->
      let rhs_is_meta = not (Trace.to_bool (check_obj_ligo ~blacklist rhs)) in
      let blacklist = if rhs_is_meta then (let_binder, rhs.location)::blacklist else blacklist in
      let _, let_result = aux blacklist let_result in
      let () =
        match let_result.expression_content with
        | E_let_in _ -> ()
        | _ ->
          (* at this point, we reach the let-in "rest", which must not contain any meta-ligo *)
          check_obj_ligo ~raise ~blacklist let_result in
      if rhs_is_meta then
        blacklist,let_result
      else
        blacklist,{ expr with expression_content = E_let_in { let_binder ; rhs ; let_result ; attr }}
    |  _ -> blacklist,expr
  in
  let purged = snd (aux [] t) in
  purged
