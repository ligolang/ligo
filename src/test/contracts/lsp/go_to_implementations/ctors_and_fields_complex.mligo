type t = Foo

let x =
  type g = Foo in
  Foo

let y : t =
  type g = Foo in
  Foo

let z =
  match x with
  | Foo -> 42

module M = struct
  module N = struct
    type t = Bar
  end
end

let a = Bar

type 'a k = Aaa of { a : 'a; b : bool }

let a = Aaa { a = 42; b = false }

module type T = sig
  type r = { x : int; y : bool }
end

module K : T = struct
  type r = { x : int; y : bool }
end

let x = { x = 42; y = false }

module Aaa = struct
  type t = Nnn
end

let f (x : C of int) : C of int =
  match x with
  | C x -> C (42 + x)
