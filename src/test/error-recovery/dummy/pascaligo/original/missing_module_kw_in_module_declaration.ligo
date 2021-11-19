module EURO is {
  type t is nat
  
  function add (const gen__191 : t * t) is
    case gen__191 of [
      (a, b) -> Operator.add (a, b)
    ]
  
  module CONST is {
    const zero : t = 0n
    
    const one : t = 1n}}

module US_DOLLAR EURO

type storage is EURO.t

function main (const gen__192 : unit * storage) is
  case gen__192 of [
    (action, store) ->
      ((list [] : list (operation)),
       EURO.add (store, EURO.CONST.one))
  ]
