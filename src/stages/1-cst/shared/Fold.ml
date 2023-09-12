open Simple_utils.Function

type 'a fold_control = Stop | Skip | Continue of 'a | Last of 'a

let map_fold_control ~(f : 'a -> 'b) : 'a fold_control -> 'b fold_control =
  function
    Stop -> Stop
  | Skip -> Skip
  | Continue a -> Continue (f a)
  | Last a -> Last (f a)

type 'a monoid = {empty : 'a; append : 'a -> 'a -> 'a}

let endo_monoid   : ('a -> 'a) monoid = {empty = Fun.id; append = (<@)}
let list_monoid   :    'a list monoid = {empty = [];     append = (@)}
let int_monoid    :        int monoid = {empty = 0;      append = (+)}
let string_monoid :     string monoid = {empty = "";     append = (^)}
let first_monoid  :  'a option monoid = {empty = None;   append = Option.first_some}
let last_monoid   :  'a option monoid = {empty = None;   append = fun x y -> Option.first_some y x}
