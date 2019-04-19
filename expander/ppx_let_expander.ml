open Base
open Ppxlib
open Ast_builder.Default

module List = struct
  include List

  let reduce_exn l ~f =
    match l with
    | [] -> invalid_arg "List.reduce_exn"
    | hd :: tl -> fold_left tl ~init:hd ~f
  ;;
end

let let_syntax ~modul : Longident.t =
  match modul with
  | None -> Lident "Let_syntax"
  | Some id -> Ldot (id.txt, "Let_syntax")
;;

let open_on_rhs ~loc ~modul ~extension_name_s =
  Located.mk ~loc (Longident.Ldot (let_syntax ~modul, "Open_on_rhs_" ^ extension_name_s))
;;

let eoperator ~loc ~modul func =
  let lid : Longident.t = Ldot (let_syntax ~modul, func) in
  pexp_ident ~loc (Located.mk ~loc lid)
;;

let expand_with_tmp_vars ~loc bindings expr ~f =
  match bindings with
  | [ _ ] -> f ~loc bindings expr
  | _ ->
    let tmp_vars =
      List.map bindings ~f:(fun _ -> gen_symbol ~prefix:"__let_syntax" ())
    in
    let s_rhs_tmp_var (* s/rhs/tmp_var *) =
      List.map2_exn bindings tmp_vars ~f:(fun vb var ->
        { vb with pvb_expr = evar ~loc:vb.pvb_expr.pexp_loc var })
    in
    let s_lhs_tmp_var (* s/lhs/tmp_var *) =
      List.map2_exn bindings tmp_vars ~f:(fun vb var ->
        { vb with pvb_pat = pvar ~loc:vb.pvb_pat.ppat_loc var })
    in
    pexp_let ~loc Nonrecursive s_lhs_tmp_var (f ~loc s_rhs_tmp_var expr)
;;

let bind_apply ~loc ~modul extension_name_s ~arg ~fn =
  pexp_apply
    ~loc
    (eoperator ~loc ~modul extension_name_s)
    [ Nolabel, arg; Labelled "f", fn ]
;;

(* Change by Georges: Always open for all extension names. *)
let maybe_open ~to_open:module_to_open expr =
  let loc = expr.pexp_loc in
  pexp_open ~loc Override (module_to_open ~loc) expr
;;

let expand_let extension_name_s ~loc ~modul bindings body =
  if List.is_empty bindings
  then invalid_arg "expand_let: list of bindings must be non-empty";
  (* Build expression [both E1 (both E2 (both ...))] *)
  let nested_boths =
    let rev_boths = List.rev_map bindings ~f:(fun vb -> vb.pvb_expr) in
    List.reduce_exn rev_boths ~f:(fun acc e ->
      let loc = e.pexp_loc in
      eapply ~loc (eoperator ~loc ~modul "both") [ e; acc ])
  in
  (* Build pattern [(P1, (P2, ...))] *)
  let nested_patterns =
    let rev_patts = List.rev_map bindings ~f:(fun vb -> vb.pvb_pat) in
    List.reduce_exn rev_patts ~f:(fun acc p ->
      let loc = p.ppat_loc in
      ppat_tuple ~loc [ p; acc ])
  in
  bind_apply
    ~loc
    ~modul
    extension_name_s
    ~arg:nested_boths
    ~fn:(pexp_fun ~loc Nolabel None nested_patterns body)
;;

let expand_match extension_name_s ~loc ~modul expr cases =
  bind_apply
    ~loc
    ~modul
    extension_name_s
    ~arg:(maybe_open ~to_open:(open_on_rhs ~modul ~extension_name_s) expr)
    ~fn:(pexp_function ~loc cases)
;;

let expand_if extension_name ~loc expr then_ else_ =
  expand_match
    extension_name
    ~loc
    expr
    [ case ~lhs:(pbool ~loc true) ~guard:None ~rhs:then_
    ; case ~lhs:(pbool ~loc false) ~guard:None ~rhs:else_
    ]
;;

let expand ~modul extension_name_s expr =
  let loc = expr.pexp_loc in
  let expansion =
    match expr.pexp_desc with
    | Pexp_let (Nonrecursive, bindings, expr) ->
      let bindings =
        List.map bindings ~f:(fun vb ->
          let pvb_pat =
            (* Temporary hack tentatively detecting that the parser
               has expanded `let x : t = e` into `let x : t = (e : t)`.

               For reference, here is the relevant part of the parser:
               https://github.com/ocaml/ocaml/blob/4.07/parsing/parser.mly#L1628 *)
            match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
            | ( Ppat_constraint (p, { ptyp_desc = Ptyp_poly ([], t1); _ })
              , Pexp_constraint (_, t2) )
              when phys_equal t1 t2 -> p
            | _ -> vb.pvb_pat
          in
          { vb with
            pvb_pat
          ; pvb_expr =
              maybe_open ~to_open:(open_on_rhs ~modul ~extension_name_s) vb.pvb_expr
          })
      in
      expand_with_tmp_vars ~loc bindings expr ~f:(expand_let extension_name_s ~modul)
    | Pexp_let (Recursive, _, _) ->
      Location.raise_errorf
        ~loc
        "'let%%%s' may not be recursive"
        extension_name_s
    | Pexp_match (expr, cases) -> expand_match extension_name_s ~loc ~modul expr cases
    | Pexp_ifthenelse (expr, then_, else_) ->
      let else_ =
        match else_ with
        | Some else_ -> else_
        | None ->
          Location.raise_errorf
            ~loc
            "'if%%%s' must include an else branch"
            extension_name_s
      in
      expand_if extension_name_s ~loc ~modul expr then_ else_
    | _ ->
      Location.raise_errorf
        ~loc
        "'%%%s' can only be used with 'let', 'match', and 'if'"
        extension_name_s
  in
  { expansion with pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes }
;;
