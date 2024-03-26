#import "counter_types.mligo" "Types"

[@entry] let increment (n : int) (store : Types.storage) : Types.result = [], store + n
[@entry] let decrement (n : int) (store : Types.storage) : Types.result = [], store - n