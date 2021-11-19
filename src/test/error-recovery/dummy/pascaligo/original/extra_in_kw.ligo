type storage is int

type parameter is list (int)

type return is list (operation) * storage

function hd (const x : list (int)) is
block {
  const err = Operator.neg (1)
} with
    case x of [
      nil -> err
    | x #
        xs -> x
    ]

function main (const gen__191 : parameter * storage) is
  case gen__191 of [
    (a, b) ->
      ((list [] : list (operation)),
       Operator.add
         (hd (a), Operator.times (Operator.add (b, 8), 11)))
  ]
