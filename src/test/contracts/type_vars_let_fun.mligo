(*
 const foo : ∀ a : * . list (a) -> list (a) = 
   Λ a -> 
      Λ b ->  
         fun ( xs : list (b)) : list (b) -> xs
*)

let foo (type a) : a list -> a list = 
   fun (type b) (xs : b list) : b list -> xs   

(*
const foo : ∀ a : * . list (a) -> list (a) = 
   Λ a -> 
      let bar : ∀ b : * . list (b) -> list (b) = 
         Λ b -> 
            fun ( xs : list (b)) : list (b) -> xs 
      in
      bar
*)

let foo (type a) : a list -> a list =
   let bar (type b) (xs : b list) : b list = xs in
   bar

(*
const foo : ∀ a : * . list (a) -> list (a) = 
   Λ a -> 
      fun (ys : list (a)) : list (a) ->  
         let bar : ∀ b : * . list (b) -> list (b) = 
            Λ b -> 
               fun ( xs : list (b)) : list (b) -> xs 
         in
         let id : ∀ c : * . c -> c = 
            Λ c -> 
               fun ( x : c) : c -> x 
         in
         let zs = ((List.map)@(id))@(ys) in
         (bar)@(zs)
*)

let foo (type a) (ys : a list) : a list =
   let bar (type b) (xs : b list) : b list = xs in
   let id (type c) (x : c) : c = x in
   let zs = List.map id ys in
   bar zs