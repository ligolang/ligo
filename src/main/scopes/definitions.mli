module AST = Ast_core

(** Contains functions for traversing the {!Ast_core} to fetch its definitions.

    During the traversal, some fields will be left blank or filled with a dummy value,
    they are meant to be filled in later passes. *)
module Of_Ast : sig
  module Waivers : sig
    (** Waivers are used to skip the traversal of some parts of the AST. While collecting
        definitions from the stdlib, for example, it may be redundant to traverse the body
        of top-level declarations, while they may be useful for the current file.

        Setting a value to [false] will traverse it, while a [true] value will skip its
        traversal. *)
    type t =
      { (* Useful for Stdlib AST traversal, when declaration rhs are unwanted *)
        d_value_expr : bool
      ; d_type_expr : bool (* TODO: unused *)
      ; d_irrefutable_match_expr : bool
      }

    (** Waivers that traverse everything by default. *)
    val default : t
  end

  (** Collects definitions from the provided AST. The new definitions will be consed into
      the given definition list. Returns the names of inlined mangled names collect during
      the traversal as well as all definitions. The string map is used to create a mapping
      from file paths to module names. *)
  val definitions
    :  ?waivers:Waivers.t
    -> AST.program
    -> string Map.Make(String).t
    -> Types.def list
    -> Inline_mangled_modules_pass.t * Types.def list
end

module Of_Stdlib_Ast : sig
  (** Like [Of_Ast.definitions], but used to collect definitions from the stdlib. *)
  val definitions : AST.program -> string Map.Make(String).t -> Types.def list
end
