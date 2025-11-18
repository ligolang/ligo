```rust
T = (A) => (x : A) -> A;

T(Nat) == T(Nat)


Unit : Type;
unit : Unit;

Unit = (C : (u : Unit) -> Type, r : C(unit)) -> C(unit);
unit = (C, r) => r;
Unit



#Unit = (C : (u : #Unit) -> Type, r : C(#unit)) -> C(#unit);
#unit = (C, r) => r


x : A;
_B = _;
y : C;

y == _B

x : A;
_B = _;
y : C;

y == _B

L[_A] == N

incr = (y) => 1 + y;
f = (x) => _A[x];

f == incr
_A[x] =

_A == N

(x = N; _A[x]) == (x =)


Γ |- A ⇐ Type
-------------------------
Γ |- ∀[A, λ. Type] ⇒ Type

Γ |- A ⇐ Type  Γ |- B ⇐ ∀[A, λ. Type]
-------------------------------------
Γ |- ∀[A, B] ⇒ Type

Γ, A |- M ⇐ B(\0)
-------------------
Γ |- λ. M ⇐ ∀[A, B]

Γ |- M ⇒ ∀[A, B]  Γ |- N ⇐ A
----------------------------
Γ |- M(N) ⇒ B(N)

Γ is a context
Γ |- A ⇒ Type
---------------------
Γ, x : A is a context

Γ |- @(x) -> T ⇒ Type
---------------------------
Γ, x : @(x) -> T |- T ⇒ Type
----------------------------
Γ |- @(x) -> T ⇒ Type

Sigma = @(s) -> (
  K : (x : A) -> Type,
  k : (x : A, y : B(x)) -> K(x)
) -> K(#unfold(s)((x) => A, (x, y) => x))

Γ |- M |->> N  Γ |- M : A
-------------------------
Γ |- N : A

Γ |- M |->> N  Γ |- N : A
-------------------------
Γ |- M : A

Γ |- M ⇐ T
----------------------
Γ |- @(x) => M ⇐ T

Γ |- [M, N] : [A, B]
-------------------
Γ |- [M, N].0 : A
----------------
Γ |- M : A
-------------------
Γ |- [M, N] : [A, B]


Γ, x : @(x) -> T |- T ⇐ Type
----------------------------
Γ |- @(x) -> T ⇒ Type


Γ |- ∀[A, λ. Type] ⇐ Type  Γ, A |- Type ⇐ Type
----------------------------------------------
Γ |- λ. Type ⇐ ∀[A, λ. Type]
----------------------------
Γ |- ∀[A, λ. Type] ⇐ Type


∀[A : Type, (x) => _]

id = (A : ⇑Type) => (x : A) => x;

id = (A : ⇑Type) => <(x : ~A) => x>;

incr = (x) => 1 + x;
f = (y) => _A[y];
g = (z) => _B[z];

f == g
f(a) == g(a)
_A[y]{y := a} == _B[z]{z := a}
_A[y] == _B[z]{z := a}{a := y}
_A[y] == _B[z]{z := y}

_B = _;
f = (y) => _B[z]{z := y};
g = (z) => _B[z];

#A = _;
#B = _;
f = (y) => #A[y];
g = (z) => #B[z];

#A = #B[z]{z := y};
#B = _;
f = (y) => #A[y];
g = (z) => #B[z];

_A = _;
_A + _A

incr == f
incr(z) == f(z)
1 + z == _A[y]{y := z}

1 + y == _A[y]


_A[z] := 1 + z
_A[y]

(z) => 1 + z == _A

_A == (z) => 1 + z

incr = λ. \0 + \0;
f = λ. _A;

incr == f
\0 + \0 == _A
λ. \0 + \0 == _

_A(N)


#l = M(N); x = A; #l
#m = M; #n = N; #l = #m(#n); x = A; #m(#n)
#m = M; #n = N; #l = #m(#n); (x = A; #m)(x = A; #n)

f = (A) => (x : _A) =>
  _A == A;
  x;


Term (M, N, K) ::=
  | x | x = N; K
  | (x) => M | M(x)

x = _;
f = (y) => x;
z = N;
f(x)

_;
λ. \1;
N;
\0(\2)

(λ. _; \0) == λ. \0
_; \0 == \0


((y) => _X) == (y) => y

((y) => x = _; x) == (y) => y

((x) => x(x))((f) => f(I))

s = (x) => 1 + x; z = 0; s(s(z))
s = (x) => 1 + x; z = 0; x = s(z); 1 + x


x = (f) => f(I); x(x)
x = (f) => f(I); f = x; f(I)
x = (f) => f(I); f = x; f = I; f(I)

((x) => x(x, x))((f, g) => f(g, I))

x = (f, g) => f(g, I); x(x, x)
x = (f, g) => f(g, I); f = x; g = x; f(g, I)
x = (f, g) => f(g, I); f = x; g = x; ((f, g) => f(g, I))(x, I)
x = (f, g) => f(g, I); f = x; g = x; f = x; g = I; f(g, I)
x = (f, g) => f(g, I); f = x; g = x; f = x; g = I; f(g, I)

x = (f, g) => f(g, I); f = x; g = x; f = g; g = I; f(g, I)

x = (f) => f(I); f = x; a = f; f = I; f(I)

if _A then M else N == if _A then K else K

_A == i

_A(x, x) == K
_R[a := x][b := x] ==
(_A : Bool)(T1, M1, N1) == K
(_A : Bool)(T2, M2, N2) == K

(_A : Bool)(A, x, y) == x

(_A : Bool) == (x, y, z) => true


if _A then M else N == K

(((A, t, f) => _R[A, t, f]))(T, M, N) == K

A = T; t = M; f = N; _R[A, t, f] == K

_R[A := T][x := M][y := N] == K

_R[x := M][y := N] == K

_A(Bool, M, N) == K
_A == true

_A(M) == M
_A(M) == N

_A(a, b, c) == K

_A := (x, y, z) => _R

_R[x := a][y := b][z := c] == K
_R == K[c := z][b := y][a := x]


_A == f
_A == (x) => f(M)

_A(M) == f(M)

Type : Type
⇑Code : Type

(A : ⇑Code) -> A

(A : ⇑Code) -> ⇑(B : Code)

((x : (A : A_K)) -> (B : B_K) : K)

(x : A : A_K) -> (B : B_K)

(x : A) -> B

id : 'a. 'a -> 'a
id : (A : Code) -> <(x : A) -> A>;

Monad : Meta = [
  T : (A : Code) -> Code;
  pure : (A : Code) -> <(x : A) -> T(A)>;
  bind : (A : Code, B : Code) -> <(m : T(A), f : (x : A) -> T(B)) -> T(B)>;
];

pure : (A : ⇑Code) -> <(x : ~A) -> T(~A)>;

case(b) : (A : Code) -> <(t : A, f : A) -> A>
case(↑b) : (A : Meta) -> <(t : ⇑A, f : ⇑A) -> ⇑A>
ind(↑b) : (
  C : (x : ⇑Bool) -> Meta,
  t : C(↑true),
  f : C(↑false)
) -> C(↑b)

case(↑b, Nat, Unit) : Code
f : (b : Bool) -> case(↑b, ⇑Nat, ⇑Unit)

ind : (b : Bool, C $ 0, t : C(true), f : C(false)) -> C(b)

ind(~b) : (C : (x : Bool) -> Code, t : C(true), f : C(false)) -> C(b)

⇑ind(~b)

ind(~b) : (
  C : (x : ⇑Bool) -> Meta,
  t : C(↑true),
  f : C(↑false)
) -> C(↑b)
f = (b : ⇑Bool) => _;
ind(b)

f = (b : ⇑Bool) => ind(b)(
  (b) => case(b, Nat, Unit),
  t : Nat,
  f : Unit
)

Code : Meta

(A : Code) -> (x : ⇑A) -> ⇑T(A)
(A : Code) -> (x : A) -> T(A)

⇑(A : Code) : Meta
<M : A : >

Code : Meta
Nat : Code

id(<Nat>)

M(N)

M ⇒ (x : A) -> B

(x : Nat) -> (A : Code) -> (y : A) -> Nat

M(N)

# x : Nat, @ A : Code, # y : A |- x

(x : Nat) => m.case(m.true, <x>, <1>)

f = (x : Nat) => (A : Code) => x;

f = (x : ⇑Nat) => (A : Code) => < ~x >;

f = (x : Nat) => (A : Code) => (y : A) : Nat => x;

f = (x : ⇑Nat) => (A : Code) => <(y : A) : Nat => ~x>;

f = (x : ⇑Nat) => (A : Code) => <(y : A) : Nat => ~x>;

f = (A : Code) => <(x : Nat) => A>;

f = (A : Code) => A;
f = (A : Code) => <(x : A) => x>;

f = (x : Nat) => x;

(x : A) => K

coerce.pi(f : (x : ⇑A) -> ⇑B) == (x : A) => ~f(<x>)


(x : A : Code) => (B : Code) ⇐ <(x : A) -> B>

(x : A : Code) => (B : Code) ⇐ (x : ⇑A) -> ⇑B

(f ⇒ (x : A) -> B) ⇐ (x : ⇑A) -> ⇑B

(x : ⇑A) => <f(~x)>

(x : A) => (x : A; K ⇒ B : Code)


f = (x : Nat) => (A : Code) => (y : A) : Nat => x;
f = (A : Code) => <(x : Nat) => (y : A) : Nat => x>;

f = (A : Code) => <(x : Nat) => (y : A) : Nat => x>;

f = (x : Nat) => (A : Code) => <(y : A) : Nat => x>;

Id = (A : Code) => A;

f = (x : Id(Nat)) => Id(Nat);

f = ~(id(Nat))(1)
id(Nat)(1)


M(N) ⇒  |->
  ((M ⇒ _A)(N ⇐ #param(_A)) ⇒ #body(_A)(x))

((x : A) -> B) |->
  (x : (A ⇐ #coerce(_A_S))) -> (B ⇒ _B_S) ⇒ #coerce.pi(_A_S, _B_S)

f(N : ⇑Nat)
f(~N)


expected :

#coerce(_A_S) == Meta
_A_S == Meta

#coerce.pi(_A_S, _B_S)

(<M : ⇑B> : ⇑A)
----------------------
(M : (B : Code)) :> ⇑A

(M : Code) :> Nat
------------------
(M : Code) :> ⇑Nat

A == B
-------------------------
(A : Code) <: (⇑B : Meta)

⇑A == B
------------------------
(A : Code) <: (B : Meta)

A == B
-------------------------
(⇑A : Meta) <: (B : Code)

A == ⇑B
-------------------------
(A : Meta) <: (B : Code)


//
term : M
expected : (x : ⇑A) -> ⇑B
received : ⇑((x : A) -> B)

(x) => <M(~x)>


//
expected : ⇑((x : A) -> B)
received : (x : A) -> B

⇑((x : A) -> B) <: (x : A) -> B -| ~



(x : Nat) -> (A : Code) => (x : A) => x;

(x : A) -> B


(A : Code) -> (x : Nat) -> ⇑A;


(A : Code) :> Meta -| ⇑A | ~(•)

(x : Nat) => (A : ⇑Code) => x

(x : Nat) => (A : ⇑Code) => x

(x : ⇑Nat) => (A : ⇑Code) => <(~x)>

(x : ⇑Nat) -> (A : ⇑Code) -> ⇑Nat

(x : Nat) ⇐ ⇑Nat

(x : Nat) : Code ⇐ (⇑Nat : Meta)



M(N)

Γ |- M ⇒ (B : Code)
Γ |- A == ⇑B
--------------------------
Γ |- M ⇐ (A : Meta) -| <M>
(x) => (A) => x

(A : ⇑Code) => (x : Nat)



((A : ⇑Code) => (x : Nat)) :



(x : A : Meta) -> (B : Code)

(x : A : Meta) -> (B : Code)
(x : A : Meta) -> ⇑B

(x) => <f(x)>

((A : Code) -> (x : A) -> A : Meta);

(((x : A) -> B : Code) :> Meta)

((A : ⇑Code) -> ⇑((x : ~A) -> ~A) : Meta);

(A : Code) -> (x : ⇑A) -> ⇑A;

(A : Code) -> (x : ⇑A) -> ⇑A;


(x : Nat) -> (A : Code) -> (y : Nat) -> Nat

(x : A) -> (B : Meta)


((x : (A : _A_K)) -> (B : _B_K)) : 'k

x : #coerce(A :> _K) |- B ⇐ K

(x : ⇑A) -> (B : Meta)

(A : ⇑Code) -> (x : A) -> A

(A : ⇑Code) -> ⇑((x : ~A) -> ~A)>

f : (x : Nat) -> (A : Code) -> (y : Nat) -> Nat


g : (x : ⇑Nat) -> (A : Code) -> ⇑((y : Nat) -> Nat)

(x : ⇑Nat) => (A : Code) => <f(~x)(A)>

(x : A) -> B

(x : A) ->

(x : A : Code) => (K : B : Meta)

(⇑x : A : Code) => (K : B : Meta)

expected : Meta
received : Code

expected : Code
received : Meta



expected : Code
received : ⇑Code

(M : (x : ⇑A) -> B)(N : A)
(M : _A)(N : _B)

(x : A : Code) -> (B : Meta)

(x : ⇑A) -> B{x := ~x}

expected : (x : ⇑A) -> ⇑B
received : ⇑((x : A) -> B)


Code <: Meta -| ⇑(•) | ~(•)

(x : (A : Code)) -> (B : Meta)
(x : (A : Code)) -> (B : Meta)
(x : (A : Meta)) -> (B : Code)


(x : (A : Code)) -> (B : Meta)
(x : (A : Meta)) -> ⇑(B : Code)

(x : (A : Code)) => (K : B : Meta)

(x : (A : Code)) => (K : B : Meta)


(x : ⇑A) => (K : B : Meta){x := ~x}



(A : Code) :> Meta |-> ⇑A -|

C : (T : Type(1)) -> Type(1)

expected : (A : Type(0)) -> C(⇑A)
received : (A : Type(1)) -> C(A)

received : Type(0)
expected : Type(1)

expected : (x : Int) -> A
received : (x : Nat) -> B

received : Nat
expected : Int

expected : A
received : B

(x : (A : Code)) -> (B : Meta)

expected : Meta
received : Code

expected : A
received : ⇑A
⇑



(x : A) => (K : B : Meta)
(x : ⇑A) => (K : B : Meta){x := ~x}

f : (x : Nat) -> (A : Code) -> (y : Nat) -> Nat

f = (x : ⇑Nat) -> (A : Code) -> ⇑((y : Nat) -> Nat)

(M : (x : A) -> B : Meta)(N : A : Meta)

(M : (x : ⇑A) -> B : Meta)(N : A : Code)
(M : (x : ⇑A) -> B : Meta)(<N> : ⇑A : Meta)

(M : (x : A) -> B : Code)(N : ⇑A : Meta)
(M : (x : A) -> B : Code)(~N : A : Code)

(M : (x : A) -> Code : Meta)(N) : Meta
(M : (x : Meta) -> B : Meta)(N : Code : Meta)

(M : (x : A) -> B : Meta)()

[
  b : Bool;
  b ? Nat : String
];

[b : Bool, [b : Bool, T(b)]] == either(T(true), T(false))


f : (b : Bool) -> b ? Nat : String;

f : (g : ((b : Bool) -> b ? Nat : String) : Meta) -> _

x = ref(0)
f : (x : )

x : A;
y : B;

x = (a, n) => [x(a, y.0), a + n];
y = x(1, y.1);

p : (b : Bool) -> b ? A : B;
p = (b) =>
  b ? (a, n) => [x(a, y.0), a + n]
   : p(true)(1, y.1);

Ty : (A : Code) -> Code;
Ty = (A) =>
  | T_nat : Ty(Nat)
  | T_unit : Ty(Unit);


(A : Code) == (B : Code)

(C : (X : Code) -> Code) -> ⇑((r : T(A)) -> T(B))

Ty : (A : Code) -> Code;
Ty = (A) =>
  [
    b : Bool, _]

Ty : (A : Code) -> Code;
Ty = (A) => Either(A == Nat, A == Unit);

(A : Code) == (B : Code)

(A : Code, x : Ty(A), y : A) -> A

p = (A : Code, x : Ty(A), y : A) : A =>
  x.is(
  | T_nat => y + 1
  | T_unit =>
  )

T : Type;
T = Either(
  Nat,
  [T, T]
);

T : Type;
T = [
  b : Bool,
  b ? Nat : [hd : T, tl : T]
];

List : (A : Code) -> Code;
List = (A, B) => [b : Bool, b ? Unit : [hd : A, tl : List(A)]];

Either : (A : Code, B : Code) -> Code;
Either = (A, B) => [b : Bool, b ? A : B];

[
  b : Bool,
  b ? Nat : [hd : T, tl : T]
]

x = 1;

x = (M : A : Meta); (N : B : Meta)
x = (M : A : Code); (N : B : Code)

//
x = (M : A : Meta); (N : B : Code)
x = (M : A : Code); (N : B : Meta)


id : (A : ⇑Code) -> <(x : ~A) -> ~A>;
  = (A) => (x) => x;

id : (A : ⇑Code) -> <(x : ~A) -> ~A>;
  = (A) => <(x) => x>;

id = (A : ⇑Code) => <(x : A) => x>;

id = (A : ⇑Code) => <(x : A) => x>

if : (A : ⇑Code) -> <(b : Bool, t : ~A, f : ~A) -> ~A>;


if()(b, )
main = (A : )


T = Native(
  Either,
  (K : ⇑Code) -> <(l : (x : ~A) -> K, r : (x : ~A) -> K) -> K>
);

(x : T) => elim(case, x, (x) => _)


#unroll(Either(A, B))

Γ |- N : A : S  Γ, x : A |- M : B : S
-------------------------------------
Γ |- x = N; K : x = N; B : S

Γ |- N ⇒ A ⇒ S  Γ, x : A |- M ⇒ B ⇐ S
-------------------------------------
Γ |- x = N; K ⇒ x = N; B : S

Γ |- N ⇒ A ⇐ S  Γ, x : A |- M : T ⇐ S
-------------------------------------
Γ |- x = N; K ⇐ T : S

x : A : Meta = M; (K : B : Code)
~(x : A : Meta = M; (<K> : ⇑B))

x : A : Code = M; (K : B : Meta)
x : ⇑A : Meta = <M>; (K : B{x := ~x} : Meta)

x : A : Code = M; (K : B : Meta)

(x : A : Meta) -> <B>

(x : A : Code) -> ⇑B
(x : A : Code) -> ⇑B

(x : A : Code) -> (B : Meta)

(x : A : Code) -> (B : Meta)
(x : ⇑A : Meta) -> x : Code = ~x; (B : Meta)

(x : A : Code) -> B
(x : A : Code) -> B

(x : A : Code) => (K : B : Meta)
(x : A : Code) => (K : B : Meta)

x = (N : A : Code); (K : B : Meta)

((x : A : Meta) => <K : B : Code>)(N)

(x : A : Meta) = N
(x : A : Code) => (K : B : Meta)


(x : ⇑A) => K{x := }

((x : ⇑A : Code) => (K : B{x := ~x} : Meta))(<M>)

(x : ⇑A : Meta) -> x : Code = ~x; (B : Meta)

⇑A : Meta

x = (M : A : Code); (K : A : Meta)


(x : A : Code) = M; (K : B : Meta)
((x : A : Code) => (K : B : Meta))(M)
((x : ⇑A : Meta) => (K : B : Meta){x := ~x})(<M>)

(x : A : Meta) = M; (K : B : Code)
((x : A : Meta) => (K : B : Code))(M)
~(((x : A : Meta) => (<K> : ⇑B : Meta))(M))

((x : A : Code) => )

List(A : Code) : Code = #michelson.type(
  _,
  "list",
  A
);
Bool : Code = #michelson.type(
  "bool",
  (A : Code, t : A, f : A) -> A
);
true : Bool = #michelson.prim("true", (A, t, f) => t);
case : Bool = #michelson.prim("case", [b, t, f]);
case(A : ⇑Code) : <(b : Bool, t : ~A, f : ~A) -> ~A> = (b, t, f) =>
  #michelson.elim(b, "case", t, f);
case(A : ⇑Code) : <(b : Bool, t : ~A, f : ~A) -> ~A> = (b, t, f) =>
  #michelson.elim(b, "case")(t, f);

Bool = Michelson(
  "bool",
  (A : Code, t : A, f : A) -> A
);
true : Bool =

Bool : Code;
true : Bool;
false : Bool;

x : A;
x = () => x();

x = () =>   x();

y = x;

x : A;
x = M;

[x : A $ A_G, y : B $ B_G] $ I_G * E_G
// equivalent to
[x : A $ I_G * A_G, y : B $ I_G * B_G] $ E_G

(x : A $ A_G) -> (B $ B_G) $ I_G * E_G
// not equivalent to
(x : A $ I_G * A_G) -> (B $ I_G * B_G) $ E_G


(x : A $ 6) -> (B $ 8) $ 5
(x : A $ ) -> (B $ 8) $ 5

[x : A $ 2 * 2, y : B $ 2 * 3] $ 4

[x : A $ 4, y : B $ 6] $ 4
[x : A $ 2, y : B $ 3] $ 8

[x : A $ 0, y : B $ 6] $ 4
[x : A $ 0 * 6, y : B $ 1 * 6] $ 4
[x : A $ 0 * 6, y : B $ 1 * 6] $ 6 * 4

[x : A $ 0 * 2, y : B $ 3] $ 8

(x : A $ 0 * 3) -> (B $ 2 * 4) $ 5

(x : A $ 2 * 3) -> (B $ 2 * 4) $ 5

(x : A $ 6) -> (B $ 8) $ 5
// can go to
(x : A $ 3) -> (B $ 4) $ 10

// cannot go back
(x : A $ 6) -> (B $ 8) $ 5

expected : (x : A $ 6) -> (B $ 8) $ 5
received : (x : A $ 3) -> (B $ 4) $ 10

f : (x : A $ 3) -> (B $ 4) $ 10
(x : A $ 6) => (f * 2)(x) : B $ 8

expected : (x : A $ 3) -> (B $ 4) $ 10
received : (x : A $ 6) -> (B $ 8) $ 5

f : (x : A $ 6) -> (B $ 8) $ 5
(x : A $ 3) => f()

// can go to



(x : A $ A_G) -> (B $ B_G) $ I_G * E_G


(x : A $ ) -> (B $ 1) $ G
(x : A $ G) -> (B $ G) $ 1


P : [
  x : A;
  y : B;
];
P = [
  x = M;
  y = N;
];

x : A; x = M; K

x : A;
y : B;
x = M;
y = N;
K

B[K]

B[x]
B[C[x]] |-> B[C[B[x]]]

Meta : Meta
Code : Code
Line : Line

⇑(A : Code)
Γ, x : A $ {0, 1, ∞}
(x : A : Line) => (<x> : ⇑A : Meta)

(x : A : Line) =>
  x_meta = <x>;
  [~x_meta, ~x_meta];

T : Data;
T = [x : T, y : A];

T : _;
T = [
  x = M;
  y =
    // T.x == M
    P : [
      I : _;
      H : T == I
    ];
    P = [
      I = [
        x = M{T := P.I};
        // I.x == M{ := I}
        y = coe(_, P.H, T.y)
      ];

    ]
    I = @rec(I => (H : T.x == I.x) => [
      x = M{T := I};
      // I.x == M{ := I}
      y = coe(B, H, T.y)
    ]);
    T = I(refl());
    x = _;
    N;
];

x : A;
y : B[x];

x = M[x, y];
y =
  // x == M[x, y]
  N[x, y];
K

P : [
  x : A,
  y : B[x]
];
P = [
  x = M[P.x, P.y];
  y =
    // P.x == M[P.x, P.y]
    N[x, P.y];
];
x = P.x;
y = P.y;
K

T_y : Type;
x(y : T_y) : A;

T_y = @(y) -> B[x(y)];
x(y) = M[x(y), y.@];

P : [
  T_x : Type;
  T_y : (x : T_x) -> Type;
];
P = [
  T_x = @(x) -> (y : P.T_y(x)) -> A;
  T_y = (x) => @(y) -> B[(x.@(y) : A)];
];

P : [
  T_y : Type;
  x(y : T_y) : A;
];
P = [
  T_y = @(y) -> B[P.x(y)];
  x(y) = M[P.x(y), y.@ : B[P.x(y)]];
];

P : [
  Unit : Type;
  unit : Unit;
];

P : [
  T_y : Type;
  x(y : T_y) : A;
];
```
