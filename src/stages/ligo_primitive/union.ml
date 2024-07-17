module Type = struct
  module T = struct
    type 'typ t = { summands : 'typ list }
    [@@unboxed] [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]
  end

  include T

  let make summands = { summands }
  let summands { summands } = summands
  let number_of_summands { summands } = List.length summands
  let nth_summand { summands } i = List.nth summands i

  let pp g ppf union =
    let open Simple_utils.PP_helpers in
    Format.fprintf ppf "@[<hv 7>%a@]" (list_sep g (tag " |@ ")) (summands union)


  let fold_map f acc { summands } =
    let acc, summands = List.fold_map ~init:acc ~f summands in
    acc, { summands }


  let for_all f union = union |> summands |> List.for_all ~f
end

include Type
module Union = Type

module Injection = struct
  module T = struct
    type 'typ t =
      { source : 'typ
      ; target : 'typ Union.t
      ; source_index_in_target : int
      }
    [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]
  end

  include T

  let source inj = inj.source
  let target inj = inj.target
  let source_index_in_target inj = inj.source_index_in_target

  let make ~source_index_in_target ~target =
    let open Option.Let_syntax in
    let%bind source = Union.nth_summand target source_index_in_target in
    return { source; target; source_index_in_target }


  let injections_of_union union =
    union
    |> Union.summands
    |> List.mapi ~f:(fun i summand ->
           { source = summand; target = union; source_index_in_target = i })


  let constructor_name inj =
    Format.sprintf "Union.Injection_%i" inj.source_index_in_target


  let pp _pp_type ppf inj = Format.pp_print_string ppf (constructor_name inj)

  let pp_with_type pp_type ppf inj =
    Format.fprintf
      ppf
      "%a : %a -> %a"
      (pp pp_type)
      inj
      pp_type
      inj.source
      (Union.pp pp_type)
      inj.target


  let fold_map f_type acc inj =
    let { source; target; source_index_in_target } = inj in
    let acc, source = f_type acc source in
    let acc, target = Union.fold_map f_type acc target in
    let inj = { source; target; source_index_in_target } in
    acc, inj
end

module Injected = struct
  module T = struct
    type ('expr, 'typ) t =
      { expr_in_source : 'expr
      ; injection : 'typ Injection.t
      }
    [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]
  end

  include T

  let expr_in_source inj = inj.expr_in_source
  let injection inj = inj.injection
  let make ~expr_in_source ~injection = { expr_in_source; injection }

  let pp pp_expr pp_type ppf inj =
    Format.fprintf
      ppf
      "(%a : %a)"
      pp_expr
      inj.expr_in_source
      (Union.pp pp_type)
      (Injection.target inj.injection)


  let fold_map f_expr f_type acc inj =
    let { expr_in_source; injection } = inj in
    let acc, expr_in_source = f_expr acc expr_in_source in
    let acc, injection = Injection.fold_map f_type acc injection in
    let inj = { expr_in_source; injection } in
    acc, inj
end

module Match = struct
  module Pattern = struct
    module T = struct
      type 'typ t = (Var.Value_var.t, 'typ) Injected.t
      [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]
    end

    include T

    let var pattern = Injected.expr_in_source pattern
    let injection pattern = Injected.injection pattern
    let make ~var ~injection = Injected.make ~expr_in_source:var ~injection

    let to_binder pattern =
      let var = var pattern in
      let typ = pattern |> injection |> Injection.source in
      Binder.make var typ


    let pp pp_type ppf pattern = Injected.pp Var.Value_var.pp pp_type ppf pattern
    let fold_map f_type acc pattern = Injected.fold_map Tuple2.create f_type acc pattern
  end

  module Branch = struct
    module T = struct
      type ('expr, 'typ) t =
        { pattern : 'typ Pattern.t
        ; body : 'expr
        }
      [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]
    end

    include T

    let pattern branch = branch.pattern
    let body branch = branch.body
    let make ~pattern ~body = { pattern; body }

    let pp pp_expr pp_type ppf { pattern; body } =
      Format.fprintf ppf "@[<h>%a => %a@]" (Pattern.pp pp_type) pattern pp_expr body


    let fold_map f_expr f_type acc { pattern; body } =
      let acc, pattern = Pattern.fold_map f_type acc pattern in
      let acc, body = f_expr acc body in
      acc, { pattern; body }
  end

  module T = struct
    type ('expr, 'typ) t =
      { matchee : 'expr
      ; branches : ('expr, 'typ) Branch.t list
      }
    [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]
  end

  include T

  let matchee match_ = match_.matchee
  let branches match_ = match_.branches
  let make ~matchee ~branches = { matchee; branches }

  let pp pp_expr pp_type ppf match_ =
    let { matchee; branches } = match_ in
    Format.fprintf
      ppf
      "@[<hv>match %a with {@.@[<hv 2>%a]@.}@]"
      pp_expr
      matchee
      (Format.pp_print_list ~pp_sep:Format.pp_print_newline (Branch.pp pp_expr pp_type))
      branches


  let fold_map f_expr f_type acc { matchee; branches } =
    let acc, matchee = f_expr acc matchee in
    let acc, branches =
      branches |> List.fold_map ~init:acc ~f:(Branch.fold_map f_expr f_type)
    in
    acc, { matchee; branches }
end

module Use = struct
  type 'expr t =
    { before_expansion : 'expr
    ; after_expansion : 'expr
    }
  [@@deriving compare, eq, fold, hash, iter, map, sexp, yojson]

  let make ~before_expansion ~after_expansion = { before_expansion; after_expansion }
  let before_expansion use = use.before_expansion
  let after_expansion use = use.after_expansion
  let pp pp_expr ppf use = pp_expr ppf use.before_expansion

  let fold_map f_expr acc { before_expansion; after_expansion } =
    let acc, before_expansion = f_expr acc before_expansion in
    let acc, after_expansion = f_expr acc after_expansion in
    acc, { before_expansion; after_expansion }
end
