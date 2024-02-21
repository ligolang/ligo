open Errors
open Simple_utils.Trace

let mapper ~raise =
  Helpers.Declaration_mapper.map_module (fun decl ->
      match Location.unwrap decl with
      | Ast_typed.D_module
          { module_binder
          ; module_attr = { deprecated = Some deprecated; _ } as module_attr
          ; module_ = { module_content = M_struct module_; module_location; signature }
          ; annotation
          } ->
        let module_ =
          let f (d : Ast_typed.declaration) =
            match Location.unwrap d with
            | D_value vdecl ->
              Location.wrap ~loc:(Location.get_location d)
              @@ Ast_typed.D_value
                   { vdecl with attr = { vdecl.attr with deprecated = Some deprecated } }
            | D_irrefutable_match vdecl ->
              Location.wrap ~loc:(Location.get_location d)
              @@ Ast_typed.D_irrefutable_match
                   { vdecl with attr = { vdecl.attr with deprecated = Some deprecated } }
            | _ -> d
          in
          List.map ~f module_
        in
        Location.wrap ~loc:(Location.get_location decl)
        @@ Ast_typed.D_module
             { module_binder
             ; module_attr = { module_attr with deprecated = None }
             ; module_ = { module_content = M_struct module_; module_location; signature }
             ; annotation
             }
      | _ -> decl)


let program
    :  raise:(Errors.self_ast_typed_error, _) raise -> Ast_typed.program
    -> Ast_typed.program
  =
 fun ~raise { pr_module; pr_sig } ->
  let pr_module = mapper ~raise pr_module in
  { pr_module; pr_sig }
