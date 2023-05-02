module Location = Simple_utils.Location
open QCheck

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val get_one : ?n:int -> 'a t -> (Location.t * Ast_typed.expression) list * 'a
  val oneof : 'a t list -> 'a t
  val mutate_int : int -> int t
  val mutate_nat : int -> int t
  val mutate_string : string -> string t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Rnd : Monad = struct
  type 'a t = ((Location.t * Ast_typed.expression) list * 'a) Gen.t

  let return x : 'a t = Gen.return ([], x)

  let ( let* ) (x : 'a t) (f : 'a -> 'b t) : 'b t =
    Gen.(x >>= fun (l, x) -> f x >>= fun (l', r) -> return (l @ l', r))


  let get_one ?n (x : 'a t) =
    let rand =
      match n with
      | None -> Caml.Random.State.make_self_init ()
      | Some seed ->
        let curr = Caml.Random.get_state () in
        Random.init seed;
        let rand = Caml.Random.get_state () in
        Caml.Random.set_state curr;
        rand
    in
    Gen.generate1 ~rand x


  let oneof (l : 'a t list) : 'a t = Gen.oneof l
  let mutate_int z = Gen.oneof [ Gen.return ([], z) ] (* ; Gen.small_int *)
  let mutate_nat n = Gen.oneof [ Gen.return ([], n) ] (* n; Gen.big_nat *)
  let mutate_string s = Gen.oneof [ Gen.return ([], s) ]
  (* [Gen.return s; Gen.map String.escaped (Gen.small_string ~gen:Gen.printable)] *)
end

module Lst : Monad = struct
  type 'a t = ((Location.t * Ast_typed.expression) list * 'a) list

  let return x = [ [], x ]

  let ( let* ) x f =
    List.concat
      (List.map
         ~f:(fun (l, x) -> List.concat (List.map ~f:(fun (l', x) -> [ l @ l', x ]) (f x)))
         x)


  let get_one ?(n = 0) l = Option.value (List.nth l n) ~default:(List.last_exn l)
  let oneof l = List.concat l
  let mutate_int n = [ [], n ]
  let mutate_nat n = [ [], n ]
  let mutate_string s = [ [], s ]
end

module Monad_context (M : Monad) = struct
  include M

  let rec bind_list = function
    | [] -> return []
    | hd :: tl ->
      let* hd in
      let* tl = bind_list tl in
      return @@ (hd :: tl)


  let bind_map_list f lst = bind_list (List.map ~f lst)

  let bind_map_option f = function
    | None -> return None
    | Some s ->
      let* x = f s in
      return (Some x)


  let bind_ne_list (hd, tl) =
    let* hd in
    let* tl = bind_list tl in
    return @@ (hd, tl)


  let bind_map_ne_list : _ -> 'a Simple_utils.List.Ne.t -> 'b Simple_utils.List.Ne.t t =
   fun f lst -> bind_ne_list (Simple_utils.List.Ne.map f lst)


  let map f x =
    let* x in
    return (f x)


  let ( let+ ) x f = map f x

  let ( and+ ) x y =
    let* x in
    let* y in
    return @@ (x, y)
end
