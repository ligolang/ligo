open Types

module type OBSCURER = sig
  val ty_expr : bool
  val pattern : bool
  val instruction : bool
  val statement : bool
  val block : bool
  val declaration : bool
  val mod_expr : bool
  val expr : bool
  val program : bool
  val program_entry : bool
  val sig_expr : bool
  val sig_entry : bool
end

module M (H : OBSCURER) = struct
  let rec sexp_of_ty_expr (x : ty_expr) : Sexp.t =
    if H.ty_expr then Sexp.Atom "TY_EXPR" else Types.sexp_of_ty_expr_ sexp_of_ty_expr x.fp


  and sexp_of_pattern (x : pattern) : Sexp.t =
    if H.pattern
    then Sexp.Atom "PATTERN"
    else Types.sexp_of_pattern_ sexp_of_pattern sexp_of_ty_expr x.fp


  and sexp_of_instruction (x : instruction) : Sexp.t =
    if H.instruction
    then Sexp.Atom "INSTRUCTION"
    else
      Types.sexp_of_instruction_
        sexp_of_instruction
        sexp_of_expr
        sexp_of_pattern
        sexp_of_statement
        sexp_of_block
        x.fp


  and sexp_of_statement (x : statement) : Sexp.t =
    if H.statement
    then Sexp.Atom "STATEMENT"
    else
      Types.sexp_of_statement_
        sexp_of_statement
        sexp_of_instruction
        sexp_of_declaration
        x.fp


  and sexp_of_block : block -> Sexp.t =
   fun b ->
    if H.block
    then Sexp.Atom "BLOCK"
    else Types.sexp_of_block_ sexp_of_block sexp_of_statement b.fp


  and sexp_of_declaration (x : declaration) : Sexp.t =
    if H.declaration
    then Sexp.Atom "DECLARATION"
    else
      Types.sexp_of_declaration_
        sexp_of_declaration
        sexp_of_expr
        sexp_of_ty_expr
        sexp_of_pattern
        sexp_of_mod_expr
        sexp_of_sig_expr
        x.fp


  and sexp_of_mod_expr (x : mod_expr) : Sexp.t =
    if H.mod_expr
    then Sexp.Atom "MOD_EXPR"
    else Types.sexp_of_mod_expr_ sexp_of_mod_expr sexp_of_program x.fp


  and sexp_of_expr (x : expr) : Sexp.t =
    if H.expr
    then Sexp.Atom "EXPR"
    else
      Types.sexp_of_expr_
        sexp_of_expr
        sexp_of_ty_expr
        sexp_of_pattern
        sexp_of_block
        sexp_of_mod_expr
        x.fp


  and sexp_of_program_entry : program_entry -> Sexp.t =
   fun e ->
    if H.program_entry
    then Sexp.Atom "PROGRAM_ENTRY"
    else
      Types.sexp_of_program_entry_
        sexp_of_program_entry
        sexp_of_declaration
        sexp_of_instruction
        e.fp


  and sexp_of_program : program -> Sexp.t =
   fun e ->
    if H.program
    then Sexp.Atom "PROGRAM"
    else Types.sexp_of_program_ sexp_of_program sexp_of_program_entry e.fp


  and sexp_of_sig_expr : sig_expr -> Sexp.t =
   fun e ->
    if H.sig_expr
    then Sexp.Atom "SIG_EXPR"
    else Types.sexp_of_sig_expr_ sexp_of_sig_expr sexp_of_sig_entry sexp_of_ty_expr e.fp


  and sexp_of_sig_entry : sig_entry -> Sexp.t =
   fun e ->
    if H.sig_entry
    then Sexp.Atom "SIG_ENTRY"
    else Types.sexp_of_sig_entry_ sexp_of_sig_expr sexp_of_sig_entry sexp_of_ty_expr e.fp
end

(* default function do not hide anything *)
include M (struct
  let ty_expr = false
  let pattern = false
  let instruction = false
  let statement = false
  let block = false
  let declaration = false
  let mod_expr = false
  let expr = false
  let program = false
  let program_entry = false
  let sig_expr = false
  let sig_entry = false
end)

let rec ty_expr_of_sexp (s : Sexp.t) : ty_expr =
  { fp = Types.ty_expr__of_sexp ty_expr_of_sexp s }


and pattern_of_sexp (s : Sexp.t) : pattern =
  { fp = Types.pattern__of_sexp pattern_of_sexp ty_expr_of_sexp s }


and instruction_of_sexp (s : Sexp.t) : instruction =
  { fp =
      Types.instruction__of_sexp
        instruction_of_sexp
        expr_of_sexp
        pattern_of_sexp
        statement_of_sexp
        block_of_sexp
        s
  }


and statement_of_sexp (s : Sexp.t) : statement =
  { fp =
      Types.statement__of_sexp statement_of_sexp instruction_of_sexp declaration_of_sexp s
  }


and block_of_sexp (s : Sexp.t) : block =
  { fp = Types.block__of_sexp block_of_sexp statement_of_sexp s }


and declaration_of_sexp (s : Sexp.t) : declaration =
  { fp =
      Types.declaration__of_sexp
        declaration_of_sexp
        expr_of_sexp
        ty_expr_of_sexp
        pattern_of_sexp
        mod_expr_of_sexp
        sig_expr_of_sexp
        s
  }


and mod_expr_of_sexp (s : Sexp.t) : mod_expr =
  { fp = Types.mod_expr__of_sexp mod_expr_of_sexp program_of_sexp s }


and expr_of_sexp : Sexp.t -> expr =
 fun s ->
  let fp =
    Types.expr__of_sexp
      expr_of_sexp
      ty_expr_of_sexp
      pattern_of_sexp
      block_of_sexp
      mod_expr_of_sexp
      s
  in
  { fp }


and program_entry_of_sexp : Sexp.t -> program_entry =
 fun s ->
  let fp =
    Types.program_entry__of_sexp
      program_entry_of_sexp
      declaration_of_sexp
      instruction_of_sexp
      s
  in
  { fp }


and program_of_sexp : Sexp.t -> program =
 fun s ->
  let fp = Types.program__of_sexp program_of_sexp program_entry_of_sexp s in
  { fp }


and sig_expr_of_sexp : Sexp.t -> sig_expr =
 fun s ->
  let fp = Types.sig_expr__of_sexp sig_expr_of_sexp sig_entry_of_sexp ty_expr_of_sexp s in
  { fp }


and sig_entry_of_sexp : Sexp.t -> sig_entry =
 fun s ->
  let fp =
    Types.sig_entry__of_sexp sig_expr_of_sexp sig_entry_of_sexp ty_expr_of_sexp s
  in
  { fp }
