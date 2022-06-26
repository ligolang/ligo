#include "./lib/multi_asset/main.mligo"

type extension = string

let main ((p,s):(parameter * extension storage)): operation list * extension storage = match p with
   Transfer         p -> transfer   p s
|  Balance_of       p -> balance_of p s
|  Update_operators p -> update_ops p s
