module type List_SIG =
  sig
    type 'a t
    val map : 'a 'b.('a -> 'b) -> 'a t -> 'b t
  end