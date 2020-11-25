module type S =
  sig
    type token
    val process : token list -> token list
  end

type token = Token.t
let process (tokens: token list) = tokens
