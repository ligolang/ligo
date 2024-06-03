open Core

(* Syntax *)

let ( let* ) x f = Result.bind ~f x
