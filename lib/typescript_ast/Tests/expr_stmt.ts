// type t = m.n.o.p<T>;
// type t = {+readonly [T in number as U] ?: V}
// type t = {+readonly [x: number] -?: T}
// type t = {public static override readonly async get foo? <T>(x: T) : T}
// type t = {abstract new <T>(x: T) : T}
// type t = {<T>(x: T) : X is Y}
// type t = {<T>(x: T) : asserts F}
// type t = {public static override readonly x?: number}
// type t = typeof f["goo"][string];
// type t = typeof f.#foo;
// type t = typeof f (6);
// type t = typeof x?.[number];
// type t = typeof import<T>;
// type t = typeof (M | number);
// type t = *
// type t = +10;
// type t = T[number];
// type t = A extends B ? {} : number;
// type t = number & string;
// type t = <T>(x: T) => T is number;
// type t = <T>(x: T) => asserts U is V;
// type t = abstract new <T>(@foo @bar("baz") public override readonly this:T=v, y?:T) => T;
// type t = infer u extends {};
// type a = [...[{y=4}, x=5] : c];
// type a = [...{...f, x, k:v, y=3} : c];
//{
// type t<T extends U = A> = [x: T, y?: number, string?, ...string, any[]];
//{}
//}
// type t = readonly number;
// f <string | (number)>("", x);
// [...x, 6];
// const x = (1+y, w)!, z;
// const x = new.target
const x = @foo class T<U> extends V<U,W>, Y implements A, B {}