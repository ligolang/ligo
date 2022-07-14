module Errors = Errors

let all_expression_mapper ~raise ~js_style_no_shadowing = [
  Vars.capture_expression ~raise ;
  Consts.assign_expression ~raise ;
  Tezos_type_annotation.peephole_expression ~raise ;
  Literals.peephole_expression ~raise ;
  Expression_soundness.linearity ~raise ;
  Expression_soundness.reserved_names_exp ~raise ;
  External.replace ;
  Deprecated_constants.warn ~raise ;
] @
  (if js_style_no_shadowing
  then [ No_shadowing.peephole_expression ~raise ]
  else [])

let all_type_expression_mapper ~raise =
  [
    Entrypoints_length_limit.peephole_type_expression ~raise ;
    Type_soundness.predefined_names ~raise ;
    Type_soundness.linearity ~raise ;
    Layout_check.layout_type_expression ~raise ;
  ]

let all_module_mapper ~raise ~js_style_no_shadowing =
  [ Expression_soundness.reserved_names_mod ~raise ]
  @
    if js_style_no_shadowing then [ No_shadowing.peephole_module ~raise ] else []

let all_module ~raise ~js_style_no_shadowing =
  List.map
    ~f:(fun el -> Helpers.Module el)
    (all_module_mapper ~raise ~js_style_no_shadowing)

let all_exp ~raise ~js_style_no_shadowing =
  List.map
    ~f:(fun el -> Helpers.Expression el)
    (all_expression_mapper ~raise ~js_style_no_shadowing)

let all_ty ~raise = List.map ~f:(fun el -> Helpers.Type_expression el) @@ all_type_expression_mapper ~raise

let all_module ~raise ~js_style_no_shadowing init =
  let all_p  = List.map ~f:Helpers.map_module @@ all_exp ~raise ~js_style_no_shadowing in
  let all_p2 = List.map ~f:Helpers.map_module @@ all_ty ~raise in
  let all_p3 = List.map ~f:Helpers.map_module @@ all_module ~raise ~js_style_no_shadowing in
  List.fold ~f:(|>) (all_p @ all_p2 @ all_p3) ~init

let all_expression ~raise ~js_style_no_shadowing init =
  let all_p = List.map ~f:Helpers.map_expression @@
    (all_expression_mapper ~raise ~js_style_no_shadowing) in
  List.fold ~f:(|>) all_p ~init

let decompile_imperative init =
  let all_p = List.map ~f:Helpers.map_module @@
    List.map ~f:(fun el -> Helpers.Expression el) [
    Assign_heuristic.peephole_expression ;
  ] in
  List.fold ~f:(|>) all_p ~init

let decompile_imperative_expression init =
  let all_p = List.map ~f:Helpers.map_expression @@ [
    Assign_heuristic.peephole_expression ;
  ] in
  List.fold ~f:(|>) all_p ~init
