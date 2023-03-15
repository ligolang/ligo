module type S =
  sig
    include Preprocessor.Options.S

    val postprocess  : int option
    val preprocess   : bool
    val string       : string option
    val print_passes : bool
    val jsligo       : string option option
    val mode         : [`Byte | `Point]
    val command      : [`Copy | `Units | `Tokens] option
  end

module MakeDefault (Options : Preprocessor.Options.S) =
  struct
    include Options

    let postprocess  = None (* All passes *)
    let preprocess   = true
    let string       = None
    let print_passes = false
    let jsligo       = None
    let mode         = `Point
    let command      = None
  end
