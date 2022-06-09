type myt is Nil | Cons of (int * int)
type myr is record [ a : int ; b : nat ; c : string ]
type myd is One of myt | Two of myr

function t1 (const x: myt * myt) is {
  const fr = function (const x: myt) is 1 ;
  const fl = function (const x: myt) is 2 ;
  const ret =
    case x of [
    | (Nil , ys)  -> fr(ys)
    | (xs  , Nil) -> fl(xs)
    | (Cons (a,b) , Cons (c,d)) -> a + b + c + d
    ] ;
} with ret

(* this essentially test variable substitution (see var rule) *)
function t2 (const x: myt ; const y: myt) is
  case x of [
  | Nil -> (
    case y of [
    | Nil -> 1
    | Cons (a,b) ->
      {
        const a = "a" ;
      } with (int (String.length(a))) + b
    ]
  )
  | Cons (a,b) ->
    {
      const old_b = b ;
      const b =
        case y of [
        | Nil ->
          {
            const f = function (const b:int) is (b + a) ;
          } with f (b+1)
        | _ -> a + b
        ] ;
    } with a + old_b + b
  ]

function t3 (const x : myd) is
  case x of [
  | One (Nil) -> 1
  | One (x) -> (
    case x of [
    | Nil -> 2
    | Cons (a,b) -> a + b
    ]
  )
  | Two (record [ a = a ; b = b ; c = c ]) -> a + (int (b)) + (int (String.length(c)))
  ]

(* A possible bug in the self pass (?) *)
function t2_3 (const x: myt ; const y: myt ; const x2: myd) is {
  const t2 =
    case x of [
    | Nil -> (
      case y of [
      | Nil -> 1
      | Cons (a,b) ->
        {
          const a = "a" ;
        } with (int (String.length(a))) + b
      ]
    )
    | Cons (a,b) ->
      {
        const old_b = b ;
        const b =
          case y of [
          | Nil ->
            {
              const f = function (const b:int) is (b + a) ;
            } with f (b+1)
          | _ -> a + b
          ] ;
      } with a + old_b + b
    ]
  ;
  const t3 =
    case x2 of [
    | One (Nil) -> 1
    | One (x) -> (
      case x of [
      | Nil -> 2
      | Cons (a,b) -> a + b
      ]
    )
    | Two (record [ a = a ; b = b ; c = c ]) -> a + (int (b)) + (int (String.length(c)))
    ]
  ;
  } with t2 + t3

function t4 (const x: myd ; const y: myd) is
  case (x , y) of [
  | (a , One (x)) -> 1
  | (One (Nil) , y) -> 2
  | (One (Cons(a,b)) , y) -> a + b
  | (Two (record [a=a;b=b;c=c]) , Two (record [a=aa;b=_;c=cc])) ->
    a  + (int(b)) + (int (String.length(c))) + aa + (int (String.length (cc)))
  ]

function t5 (const x: int) is
  case (x, unit) of [
  | (a , _) -> a
  ]

function t6 (const x: int) is
  case (x, unit) of [
  | (_ , _) -> 2
  ]

function t7 (const x: option(int)) is
  case x of [
  | Some (x) -> x
  | None -> 1
  ]

function t8 (const x: option (int * int) ; const y: int) is
  case (x , y) of [
  | (None , x) -> x
  | (Some ((x,y)) , _) -> x + y
  ]


function t9 (const x: option (int) ; const y: option(int)) is
  case (x , y) of [
  | (None , ys)  -> 1
  | (xs  , None) -> 2
  | (Some (a) , Some (b)) -> a + b
  ]

type optioni is option(int)
type myti is Nili | Consi of optioni

function fl (const x:myti) is 1
function fo (const x:optioni) is 2

function t10 (const x: myti ; const y: myti) is
  case (x,y) of [
  | (Nili , ys)  -> fl (ys)
  | (xs  , Nili) -> fl (xs)
  | (Consi (None) , Consi (Some (b))) ->
    {
      const b = 1;
    } with b
  | (Consi (a) , Consi (b)) -> fo (a) + fo (b)
  ]

function t11 (const x: myti ; const y: myti) is
  case (x,y) of [
  | (Nili , ys)  -> fl (ys)
  | (xs  , Nili) -> fl (xs)
  | (Consi (Some (a)) , Consi (Some (b))) ->
    {
      const a = 1 ;
    } with a + b
  | (Consi (a) , Consi (b)) -> (
    case a of [
    | None -> fo (a) + fo (b)
    | Some (a) -> a
    ]
  )
  ]

function t12 (const x : list(int)) is
  case x of [
  | nil -> 0
  | hd#nil -> hd
  | hd#(hd2#nil) -> hd + hd2
  | hd#(hd2#(hd3#nil)) -> hd + hd2 + hd3
  | hd#tl -> -1
  ]

type recordi is record [ a : option(list(int)) ; b : list(int) ]

const none_a = record [ a = (None:option(list(int))) ; b = list [42] ]
const some_a = record [ a = (Some (list [1;2;3;4])) ; b = list [42] ]
const a_empty_b_not = record [ a = Some ((list []: list(int))) ; b = list [111] ]
const b_empty_a_not = record [ a = Some (list [222]) ; b = (nil: list(int)) ]

function t13 (const x:recordi ; const y:recordi) is
  case (x,y) of [
  | (record [ a=None;b=_] , record [ a = _ ; b = _ ]) -> -1
  | (record [ a=_;b=_]    , record [ a = Some (nil) ; b = (hd#tl) ]) -> hd
  | (record [ a=_;b=_]    , record [ a = Some ((hd#tl)) ; b = nil ]) -> hd
  | (record [ a=Some (a);b=_] , _) -> int ( List.length(a) )
  ]

type a is A | B of int | C of int * int * int | D | E ;
type r is record [ a : int; b : a ] ;

function nested_record_pm (const a : r) : int is
  case a of [
  | record [ a = _    ; b = A        ] -> 1
  | record [ b = B(_) ; a = _        ] -> 2
  | record [ a = _    ; b = C(_,_,_) ] -> 3
  | record [ b = D    ; a = _        ] -> 4
  | record [ a = _    ; b = E        ] -> 5
  ]