open Trace
module I = Ast_core
module O = Ast_typed
module Environment = O.Environment
type environment = Environment.t

let unbound_type_variable (e:environment) (tv:I.type_variable) (loc:Location.t) () =
  let title = (thunk "unbound type variable") in
  let message () = "" in
  let data = [
      ("variable" , fun () -> Format.asprintf "%a" I.PP.type_variable tv) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc) ;
      ("in" , fun () -> Format.asprintf "%a" Environment.PP.environment e)
    ] in
  error ~data title message ()

let unbound_variable (e:environment) (n:I.expression_variable) (loc:Location.t) () =
  let name () = Format.asprintf "%a" I.PP.expression_variable n in
  let title = (thunk ("unbound variable "^(name ()))) in
  let message () = "" in
  let data = [
      ("variable" , name) ;
      ("environment" , fun () -> Format.asprintf "%a" Environment.PP.environment e) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
  error ~data title message ()

let match_empty_variant : I.matching_expr -> Location.t -> unit -> _ =
  fun matching loc () ->
  let title = (thunk "match with no cases") in
  let message () = "" in
  let data = [
      ("variant" , fun () -> Format.asprintf "%a" I.PP.matching_type matching) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
  error ~data title message ()

let match_missing_case : I.matching_expr -> Location.t -> unit -> _ =
  fun matching loc () ->
  let title = (thunk "missing case in match") in
  let message () = "" in
  let data = [
      ("variant" , fun () -> Format.asprintf "%a" I.PP.matching_type matching) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
  error ~data title message ()

let match_redundant_case : I.matching_expr -> Location.t -> unit -> _ =
  fun matching loc () ->
  let title = (thunk "redundant case in match") in
  let message () = "" in
  let data = [
      ("variant" , fun () -> Format.asprintf "%a" I.PP.matching_type matching) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
  error ~data title message ()

let unbound_constructor (e:environment) (c:I.constructor') (loc:Location.t) () =
  let title = (thunk "unbound constructor") in
  let message () = "" in
  let data = [
      ("constructor" , fun () -> Format.asprintf "%a" I.PP.constructor c) ;
      ("environment" , fun () -> Format.asprintf "%a" Environment.PP.environment e) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
  error ~data title message ()

let wrong_arity (n:string) (expected:int) (actual:int) (loc : Location.t) () =
  let title () = "wrong arity" in
  let message () = "" in
  let data = [
      ("function" , fun () -> Format.asprintf "%s" n) ;
      ("expected" , fun () -> Format.asprintf "%d" expected) ;
      ("actual" , fun () -> Format.asprintf "%d" actual) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
  error ~data title message ()

let match_tuple_wrong_arity (expected:'a list) (actual:'b list) (loc:Location.t) () =
  let title () = "matching tuple of different size" in
  let message () = "" in
  let data = [
      ("expected" , fun () -> Format.asprintf "%d" (List.length expected)) ;
      ("actual" , fun () -> Format.asprintf "%d" (List.length actual)) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
  error ~data title message ()

(* TODO: this should be a trace_info? *)
let program_error (p:I.program) () =
  let message () = "" in
  let title = (thunk "typing program") in
  let data = [
      ("program" , fun () -> Format.asprintf "%a" I.PP.program p)
    ] in
  error ~data title message ()

let constant_declaration_error (name: I.expression_variable) (ae:I.expr) (expected: O.type_expression option) () =
  let title = (thunk "typing constant declaration") in
  let message () = "" in
  let data = [
      ("constant" , fun () -> Format.asprintf "%a" I.PP.expression_variable name) ; (* Todo : remove Stage_common*)
      ("expression" , fun () -> Format.asprintf "%a" I.PP.expression ae) ;
      ("expected" , fun () ->
                    match expected with
                      None -> "(no annotation for the expected type)"
                    | Some expected -> Format.asprintf "%a" O.PP.type_expression expected) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp ae.location)
    ] in
  error ~data title message ()

let match_error : ?msg:string -> expected: I.matching_expr -> actual: O.type_expression -> Location.t -> unit -> _ =
  fun ?(msg = "") ~expected ~actual loc () ->
  let title = (thunk "typing match") in
  let message () = msg in
  let data = [
      ("expected" , fun () -> Format.asprintf "%a" I.PP.matching_type expected);
      ("actual" , fun () -> Format.asprintf "%a" O.PP.type_expression actual) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
  error ~data title message ()

(* let needs_annotation (e : I.expression) (case : string) () =
 *   let title = (thunk "this expression must be annotated with its type") in
 *   let message () = Format.asprintf "%s needs an annotation" case in
 *   let data = [
 *     ("expression" , fun () -> Format.asprintf "%a" I.PP.expression e) ;
 *     ("location" , fun () -> Format.asprintf "%a" Location.pp e.location)
 *   ] in
 *   error ~data title message () *)

(* let type_error_approximate ?(msg="") ~(expected: string) ~(actual: O.type_value) ~(expression : I.expression) (loc:Location.t) () =
 *   let title = (thunk "type error") in
 *   let message () = msg in
 *   let data = [
 *     ("expected"   , fun () -> Format.asprintf "%s" expected);
 *     ("actual"     , fun () -> Format.asprintf "%a" O.PP.type_value actual);
 *     ("expression" , fun () -> Format.asprintf "%a" I.PP.expression expression) ;
 *     ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
 *   ] in
 *   error ~data title message () *)

let type_error ?(msg="") ~(expected: O.type_expression) ~(actual: O.type_expression) ~(expression : I.expression) (loc:Location.t) () =
  let title = (thunk "type error") in
  let message () = msg in
  let data = [
      ("expected"   , fun () -> Format.asprintf "%a" O.PP.type_expression expected);
      ("actual"     , fun () -> Format.asprintf "%a" O.PP.type_expression actual);
      ("expression" , fun () -> Format.asprintf "%a" I.PP.expression expression) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
  error ~data title message ()

let bad_type_operator type_op =
  let title () = Format.asprintf "bad type operator %a" I.PP.type_expression type_op in
  let message () = "" in
  error title message
