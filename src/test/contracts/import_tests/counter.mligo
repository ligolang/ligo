module Types = Counter_types

[@entry] let increment (n : int) (store : Types.storage) : Types.result = [], store + n
[@entry] let decrement (n : int) (store : Types.storage) : Types.result = [], store - n
