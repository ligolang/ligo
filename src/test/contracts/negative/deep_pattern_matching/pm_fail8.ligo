type myt is Nil | Cons of (int * int)

function t (const x: myt ; const y: myt) is
  case x of [
  | Nil -> (
    case y of [
    | Nil -> 1
    | Cons (a,b) ->
      {
        const a = "a" ;
      } with int(String.length (a)) + b
    ]
  )
  | Cons (a,b) ->
    {
      const old_b = b ;
      const b =
        case y of [
        | Nil ->
          {
            const f = function (const b:int) is b + a ;
          } with f (b+1)
        | Cons (a,b) -> "invalid"
        ] ;
    } with a + old_b + b
  ]