module type S =
  sig
    include Preprocessor.Options.S

    val postprocess  : int option
    val preprocess   : bool
    val print_passes : bool
    val mode         : [`Byte | `Point]
    val command      : [`Copy | `Units | `Tokens] option
  end

module MakeDefault (Options : Preprocessor.Options.S) =
  struct
    include Options

    let postprocess  = None (* All passes *)
    let preprocess   = false
    let print_passes = false
    let mode         = `Point
    let command      = None
  end
