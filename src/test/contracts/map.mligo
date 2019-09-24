type foobar = (int , int) map

let foobar : foobar = Map.empty

let foobarz : foobar = Map.literal [ (1 , 10) ; (2 , 20) ]

let foo : int = Map.find 1 foobarz
