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

module Default =
  struct
    let input        = None
    let dirs         = []
    let define       = []
    let project_root = None
    let show_pp      = false
    let no_colour    = false
    let offsets      = true
  end
