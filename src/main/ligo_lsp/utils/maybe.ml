open Linol_lwt.Jsonrpc2
open Linol_lwt

(* TODO: use Option from Core *)
include Caml.Option

let return x = IO.return (Some x)

let bind_return_io_option : 'a Option.t -> ('a -> 'b Option.t IO.t) -> 'b Option.t IO.t =
 fun value f ->
  match value with
  | None -> IO.return None
  | Some value -> f value


let bind_io_option : 'a Option.t IO.t -> ('a -> 'b Option.t IO.t) -> 'b Option.t IO.t =
 fun value f ->
  let* value in
  bind_return_io_option value f


let ( let< ) = bind_return_io_option
let ( let> ) = bind_io_option
let ( let@ ) = bind

let from_list : 'a list IO.t -> 'a list option IO.t =
 fun value ->
  let* value in
  match value with
  | [] -> IO.return None
  | l -> IO.return @@ Some l


let traverse : ('a -> 'b IO.t) -> 'a t -> 'b t IO.t =
 fun f -> function
  | None -> IO.return None
  | Some x ->
    IO.(
      let* t = f x in
      return (Some t))


let sequence : 'a IO.t t -> 'a t IO.t = fun x -> traverse Fun.id x
