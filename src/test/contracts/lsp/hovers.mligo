type 'a endo = Endo of 'a -> 'a

let compose_endo (Endo f) (Endo g) = Endo (fun x -> f (g x))

let compose_endo_with_type_annotation (type a) : a endo -> a endo -> a endo =
  fun (Endo f) (Endo g) -> Endo (fun x -> f (g x))

type int_endo = IntEndo of int -> int

let compose_int_endo (IntEndo f) (IntEndo g) = IntEndo (fun x -> f (g x))

type ('a, 'b) iso =
  {
   from : 'a -> 'b;
   to : 'b -> 'a
  }

let iso_a_b_is_iso_b_a (type a b) : ((a, b) iso, (b, a) iso) iso =
  {
   from =
     fun ({
         from = f1;
         to = f2
        }) -> {
        from = f2;
        to = f1
       };
   to =
     fun ({
         from = f1;
         to = f2
        }) -> {
        from = f2;
        to = f1
       }
  }

let int_endo_is_endo : (int_endo, int endo) iso =
  {
   from = fun (IntEndo f) -> Endo f;
   to = fun (Endo f) -> IntEndo f
  }

let flipping : (int endo, int_endo) iso =
  let iso =
    (iso_a_b_is_iso_b_a
     : ((int_endo, int endo) iso, (int endo, int_endo) iso) iso) in
  iso.from int_endo_is_endo

let plus1 = Endo (fun x -> x + 1)

let plus2 = IntEndo (fun x -> x + 2)

let plus3 = compose_int_endo (int_endo_is_endo.to plus1) plus2

let id (type t) : t endo =
  Endo (fun x -> x) // Requires explicit type annotation

let map_endo (type a) (Endo f) =
  (Endo (List.map f) : a list endo) // Needs parens :(

let option_endo (Endo f) = Endo (Option.map f)

type 'a list2 = 'a list list

let endo_list2 (type a) : a endo -> a list2 endo =
  fun f -> map_endo (map_endo f)

let f1 : int list endo = Endo (List.map (fun x -> x + 1))

let x1 =
  fun t -> let (Endo f) = f1 in
    let (Endo g) = endo_list2 f1 in
    g [List.cons [5] (List.map f t)]

let z = Tezos.create_contract (fun x y -> ([], x + y))

type t = Test.Proxy_ticket.proxy_address

type 'v p = 'v Test.Proxy_ticket.proxy_address

type int_endo_option =
| No_int_endo
| Some_int_endo of int_endo
