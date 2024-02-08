module Utils = Simple_utils.Utils
module Region = Simple_utils.Region
module Location = Simple_utils.Location
module AST = Ast_unified

let r_split = Location.r_split
let r_fst x = fst (r_split x)
let r_snd x = snd (r_split x)

let w_split (x : 'a Lexing_shared.Wrap.t) : 'a * Location.t =
  x#payload, Location.lift x#region


let w_fst x = fst (w_split x)
let w_snd x = snd (w_split x)

(* Define equivalences between Ast_unified & CST 'sub-languages' *)
module type EQUIVALENCES = sig
  type expr
  type ty_expr
  type pattern
  type statement
  type block
  type mod_expr
  type instruction
  type declaration
  type program_entry
  type program
  type sig_expr
  type sig_entry
end

(* Define the type of Ast_unified nodes while being built *)
module Folding (X : EQUIVALENCES) = struct
  type ty_expr = X.ty_expr Ast_unified.ty_expr_
  type pattern = (X.pattern, X.ty_expr) Ast_unified.pattern_

  type instruction =
    (X.instruction, X.expr, X.pattern, X.statement, X.block) Ast_unified.instruction_

  type statement = (X.statement, X.instruction, X.declaration) Ast_unified.statement_
  type block = (X.block, X.statement) Ast_unified.block_

  type declaration =
    ( X.declaration
    , X.expr
    , X.ty_expr
    , X.pattern
    , X.mod_expr
    , X.sig_expr )
    Ast_unified.declaration_

  type mod_expr = (X.mod_expr, X.program) Ast_unified.mod_expr_
  type expr = (X.expr, X.ty_expr, X.pattern, X.block, X.mod_expr) Ast_unified.expr_

  type program_entry =
    (X.program_entry, X.declaration, X.instruction) Ast_unified.program_entry_

  type program = (X.program, X.program_entry) Ast_unified.program_
  type sig_expr = (X.sig_expr, X.sig_entry, X.ty_expr) Ast_unified.sig_expr_
  type sig_entry = (X.sig_expr, X.sig_entry, X.ty_expr) Ast_unified.sig_entry_
end

(* define types of unification function (for each sub-lang) *)
module type UNIF = sig
  module Eq : EQUIVALENCES
  open Folding(Eq)

  val ty_expr : Eq.ty_expr -> ty_expr
  val pattern : Eq.pattern -> pattern
  val instruction : Eq.instruction -> instruction
  val statement : Eq.statement -> statement
  val block : Eq.block -> block
  val declaration : Eq.declaration -> declaration
  val mod_expr : Eq.mod_expr -> mod_expr
  val expr : Eq.expr -> expr
  val program_entry : Eq.program_entry -> program_entry
  val program : Eq.program -> program
  val sig_expr : Eq.sig_expr -> sig_expr
  val sig_entry : Eq.sig_entry -> sig_entry
end

(* apply anamorphism on expression/pattern (can easily be extended) *)
module Make_unification (C : UNIF) = struct
  open C

  let unfolder =
    Ast_unified.Anamorphism.
      { expr
      ; ty_expr
      ; pattern
      ; statement
      ; block
      ; mod_expr
      ; instruction
      ; declaration
      ; program_entry
      ; program
      ; sig_expr
      ; sig_entry
      }


  let compile_expression e = Ast_unified.Anamorphism.ana_expr ~f:unfolder e
  let compile_program prg = Ast_unified.Anamorphism.ana_program ~f:unfolder prg
  let compile_type_expression t = Ast_unified.Anamorphism.ana_ty_expr ~f:unfolder t
end

let failwith_not_initial_node_decompiler node =
  failwith
  @@ "Decompiler: ty_expr: not_initial nodes are not supported, make sure \
      Nanopasses.decompile<...> was called before decompiling Ast_unified to CST. Node \
      is:\n"
  ^ Sexp.to_string_hum
  @@
  let d _ = Sexp.Atom "xx" in
  match node with
  | `Expr e -> AST.sexp_of_expr_ d d d d d e
  | `Ty_expr t -> AST.sexp_of_ty_expr_ d t
  | `Pattern p -> AST.sexp_of_pattern_ d d p
