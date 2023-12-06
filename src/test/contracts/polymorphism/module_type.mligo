module type ListI = sig
    val mymap : 'a 'b. ('a -> 'b) -> 'a list -> 'b list
  end

module List : ListI = struct
  let mymap (type a b) (f : a -> b) (xs : a list) : b list = List.map f xs
end
