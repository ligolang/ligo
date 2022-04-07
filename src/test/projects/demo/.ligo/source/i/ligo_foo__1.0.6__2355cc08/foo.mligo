#import "ligo-set-helpers/set.mligo" "SetX"
#include "ligo-list-helpers/list.mligo" 

let uniq_concat (xs : int list) (ys : int list) =
    let zs = concat xs ys in
    let s = SetX.of_list zs in
    SetX.to_list s