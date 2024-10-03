module I = Mini_c
module O = Lltz_ir
module Micheline = Tezos_micheline.Micheline
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
module Ligo_string = Simple_utils.Ligo_string
module Ligo_option = Simple_utils.Ligo_option
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

let binder_meta (var : Var.t option) (source_type : I.type_expression)
    : binder_meta option
  =
  Option.map var ~f:(fun var : binder_meta ->
      { location = Var.get_location var
      ; name = (if Var.is_generated var then None else Some (Var.to_name_exn var))
      ; source_type = source_type.source_type
      })


(* Next stage uses Micheline for its types: *)
(* TODO: what about this var? *)
let translate_type ?var : I.type_expression -> O.Type.t =
  Ligo_lltz_codegen.compile_type_expression


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
