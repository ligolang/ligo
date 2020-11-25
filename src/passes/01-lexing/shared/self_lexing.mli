module type S =
  sig
    type token

    val process : token list -> token list
  end
