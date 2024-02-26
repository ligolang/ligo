type ('a, 'b) func = Func of 'a -> 'b

let incr = Func (fun x -> x + 1)

let apply_func (Func f) x = f x

let apply_func f x =
  match f with
  | Func f -> f x

let apply_func (type dom codom) (f : (dom, codom) func) x =
  match f with
  | Func f -> f x

type 'a t = { x : 'a; y : string }

let f (type a) (r : a t) = { r with y = "42" }

let ticket = Tezos.create_ticket 42n 42n

let lst = List.map (fun c -> c + 42) [1; 2]

let opt = Some 42

let big_map = Big_map.literal [42, 42n; 100, 100n]

module M = struct
  type 'a t = Aaa of 'a
end

let x = Aaa "aaa"

#import "poly_types_common.mligo" "Common"

let foo = Foo 42

include Common

let ok_result : (int, string) result = Ok 42

let error_result : (int, string) result = Error "42"

module B = struct
  module X = struct
    type 'a x = 'a
  end

  type 'a b = B of 'a X.x
end

type t = int list

let f (x : t) = 42 :: x
