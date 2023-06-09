open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* Throw an error upon module openening that can't be converted to module access *)
include Flag.No_arg ()

let rec dig_proj_until_var ~raise ~f proj =
  match get_e_proj proj with
  | Some { struct_; path } ->
    let struct_ =
      match get_e_variable struct_ with
      | Some v -> f v
      | None -> dig_proj_until_var ~raise ~f struct_
    in
    e_proj ~loc:(get_e_loc proj) { struct_; path }
  | None -> raise.error (unsupported_module_access (`Expr proj))


let compile ~raise =
  let expr : (expr, _, _, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_module_open_in { field; field_as_open; _ } when field_as_open ->
      raise.error (unsupported_module_access (`Expr field))
    | E_module_open_in
        { module_path; field = { fp = { wrap_content = E_variable v; _ } }; _ } ->
      e_module_access ~loc { module_path; field = v; field_as_open = false }
    | E_module_open_in
        { module_path = mpl
        ; field =
            { fp = { wrap_content = E_module_access { module_path = mpr; field; _ }; _ } }
        ; _
        } ->
      e_module_access
        ~loc
        { module_path = List.Ne.append mpl mpr; field; field_as_open = false }
    | E_module_open_in { module_path; field; _ } ->
      let f v = e_module_access ~loc { module_path; field = v; field_as_open = false } in
      dig_proj_until_var ~raise ~f field
    | e -> make_e ~loc e
  in
  let ty_expr : _ ty_expr_ -> ty_expr =
   fun ty ->
    let loc = Location.get_location ty in
    match Location.unwrap ty with
    | T_module_open_in { field; field_as_open; _ } when field_as_open ->
      raise.error (unsupported_module_access (`Type field))
    | T_app { constr = ({ fp } : ty_expr); type_args } as t ->
      (match Location.unwrap fp with
      | T_module_access { module_path; field; _ } ->
        t_module_app
          ~loc
          { type_args; constr = { module_path; field; field_as_open = false } }
      | _ -> make_t ~loc t)
    | T_module_open_in { module_path; field; _ } ->
      (match get_t_var field with
      | Some v ->
        t_module_access
          ~loc
          { module_path = List.Ne.singleton module_path
          ; field = v
          ; field_as_open = false
          }
      | None ->
        (match get_t field with
        | T_module_access { module_path = module_path'; field; _ } ->
          t_module_access
            ~loc
            { module_path = List.Ne.cons module_path module_path'
            ; field
            ; field_as_open = false
            }
        | T_app { type_args; constr } ->
          let field' =
            trace_option ~raise (unsupported_module_access (`Type field))
            @@ get_t_var constr
          in
          let module_path = List.Ne.singleton module_path in
          t_module_app
            ~loc
            { type_args; constr = { module_path; field = field'; field_as_open = false } }
        | _ -> raise.error (unsupported_module_access (`Type field))))
    | t -> make_t ~loc t
  in
  Fold { idle_fold with expr; ty_expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_module_open_in _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; ty_expr =
      (function
      | { wrap_content = T_module_open_in _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
