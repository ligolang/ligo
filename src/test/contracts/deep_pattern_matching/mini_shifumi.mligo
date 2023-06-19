type player = address

type action =
| Stone
| Paper
| Cisor

type 'a an_action =
  {
   player : player;
   action : 'a
  }

type decoded_player_action = action an_action

[@inline]
let resolve (first, second : decoded_player_action * decoded_player_action)
: player option =
  let result : player option =
    match first.action, second.action with
      Stone, Stone -> None
    | Stone, Paper -> Some (second.player)
    | Stone, Cisor -> Some (first.player)
    | Paper, Stone -> Some (first.player)
    | Paper, Paper -> None
    | Paper, Cisor -> Some (second.player)
    | Cisor, Stone -> Some (second.player)
    | Cisor, Paper -> Some (first.player)
    | Cisor, Cisor -> None in
  result

let main
  ((first, second) : decoded_player_action * decoded_player_action)
  (_ : player option)
: operation list * player option = [], resolve (first, second)
