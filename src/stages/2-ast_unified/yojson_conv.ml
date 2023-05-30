open Types

type 'a t = 'a Ppx_deriving_yojson_runtime.error_or

let rec ty_expr_to_yojson (x : ty_expr) = ty_expr__to_yojson ty_expr_to_yojson x.fp

and pattern_to_yojson (x : pattern) =
  pattern__to_yojson pattern_to_yojson ty_expr_to_yojson x.fp


and instruction_to_yojson (x : instruction) =
  instruction__to_yojson
    instruction_to_yojson
    expr_to_yojson
    pattern_to_yojson
    statement_to_yojson
    block_to_yojson
    x.fp


and statement_to_yojson (x : statement) =
  statement__to_yojson
    statement_to_yojson
    instruction_to_yojson
    declaration_to_yojson
    x.fp


and block_to_yojson (x : block) =
  block__to_yojson block_to_yojson statement_to_yojson x.fp


and declaration_to_yojson (x : declaration) =
  declaration__to_yojson
    declaration_to_yojson
    expr_to_yojson
    ty_expr_to_yojson
    pattern_to_yojson
    mod_expr_to_yojson
    sig_expr_to_yojson
    x.fp


and mod_expr_to_yojson (x : mod_expr) =
  mod_expr__to_yojson mod_expr_to_yojson program_to_yojson x.fp


and expr_to_yojson (x : expr) =
  expr__to_yojson
    expr_to_yojson
    ty_expr_to_yojson
    pattern_to_yojson
    block_to_yojson
    mod_expr_to_yojson
    x.fp


and program_entry_to_yojson (x : program_entry) =
  program_entry__to_yojson
    program_entry_to_yojson
    declaration_to_yojson
    instruction_to_yojson
    x.fp


and program_to_yojson (x : program) =
  program__to_yojson program_to_yojson program_entry_to_yojson x.fp


and sig_expr_to_yojson (x : sig_expr) =
  sig_expr__to_yojson sig_expr_to_yojson sig_entry_to_yojson ty_expr_to_yojson x.fp


and sig_entry_to_yojson (x : sig_entry) =
  sig_entry__to_yojson sig_expr_to_yojson sig_entry_to_yojson ty_expr_to_yojson x.fp


open Ppx_deriving_yojson_runtime

let rec ty_expr_of_yojson x =
  ty_expr__of_yojson ty_expr_of_yojson x >|= fun fp : ty_expr -> { fp }


and pattern_of_yojson x =
  pattern__of_yojson pattern_of_yojson ty_expr_of_yojson x >|= fun fp : pattern -> { fp }


and instruction_of_yojson x =
  instruction__of_yojson
    instruction_of_yojson
    expr_of_yojson
    pattern_of_yojson
    statement_of_yojson
    block_of_yojson
    x
  >|= fun fp : instruction -> { fp }


and statement_of_yojson x =
  statement__of_yojson statement_of_yojson instruction_of_yojson declaration_of_yojson x
  >|= fun fp : statement -> { fp }


and block_of_yojson x =
  block__of_yojson block_of_yojson statement_of_yojson x >|= fun fp : block -> { fp }


and declaration_of_yojson x =
  declaration__of_yojson
    declaration_of_yojson
    expr_of_yojson
    ty_expr_of_yojson
    pattern_of_yojson
    mod_expr_of_yojson
    sig_expr_of_yojson
    x
  >|= fun fp : declaration -> { fp }


and mod_expr_of_yojson x =
  mod_expr__of_yojson mod_expr_of_yojson program_of_yojson x
  >|= fun fp : mod_expr -> { fp }


and expr_of_yojson x =
  expr__of_yojson
    expr_of_yojson
    ty_expr_of_yojson
    pattern_of_yojson
    block_of_yojson
    mod_expr_of_yojson
    x
  >|= fun fp : expr -> { fp }


and program_entry_of_yojson x =
  program_entry__of_yojson
    program_entry_of_yojson
    declaration_of_yojson
    instruction_of_yojson
    x
  >|= fun fp : program_entry -> { fp }


and program_of_yojson x =
  program__of_yojson program_of_yojson program_entry_of_yojson x
  >|= fun fp : program -> { fp }


and sig_expr_of_yojson x =
  sig_expr__of_yojson sig_expr_of_yojson sig_entry_of_yojson ty_expr_of_yojson x
  >|= fun fp : sig_expr -> { fp }


and sig_entry_of_yojson x =
  sig_entry__of_yojson sig_expr_of_yojson sig_entry_of_yojson ty_expr_of_yojson x
  >|= fun fp : sig_entry -> { fp }
