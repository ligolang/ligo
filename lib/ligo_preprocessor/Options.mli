(* CLI options *)

module type S =
  sig
    val input        : string option (* input file            *)
    val dirs         : string list   (* -I                    *)
    val define       : string list   (* -D                    *)
    val project_root : string option (* --project-root        *)
    val show_pp      : bool          (* --show-pp             *)
    val no_colour    : bool          (* --no-colour           *)
    val offsets      : bool          (* negation of --columns *)
  end

(* Default values for plugging the preprocessor into the compiler *)

module Default : S
