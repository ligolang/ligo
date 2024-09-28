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
// const x = @foo class T<U> extends V<U,W>, Y implements A, B {}
// const x = class <T>{
//   @foo @bar f() {};
//   g() : T;
//   static {};
//  abstract h();
//  declare public f = 6;
// }
// const x = async x => {}
// const y = <T>(x: T) : T => x
// const f = function <T>(x: T) : T { return x }
// const x = { a: 5, ...f(), g() {}, z }
// const a = (a.b.c)?.z
// const c = (1)
// const d = (1, x, f())
// 1, x, f();
// const b = (a.b.c)?.[1+x, 3]
// const b = x[3]
// import m = a.b.m
// import m = n
// interface I<T> extends m.J.t<u>, K {}
// enum x {a, b=4, c, d=3}
// namespace N
// namespace N.M {}
// namespace "Foo" {}
// const x = <T>y
// const x = y<T>
// const x = 5 satisfies number
// const x = 5 as const
// const x = y as number

//   function* generator(i) {
//     yield *g;
//     yield i + 10;
//     yield;
//   }

// const c = new C;
// const c = new C<number>
// const c = new C<number>(4)
// const x = ++y
// const x = --y
// const x = y++
// const x = y--
// const t = true ? x : 0
// const x = (x ^ y) >= z
// const {[x]:[]} = {}

// async function foo(name) {
//   console.log(name, "start");
//   await console.log(name, "middle");
//   console.log(name, "end");
// }

// x += 4
// x = 5
// module N
// module N.M {}
// module "Foo" {}
// @foo class T<U> extends V<U,W>, Y implements A, B {}
// @foo abstract class T<U> extends V<U,W>, Y implements A, B {}
// async function f ()
// var x = (1+y, w)!, z
// label: throw C()
// ;
// return (1+y, w)!, z
// return
// continue label
// break label
// with (1+x) {y=x}
// try {}
// try {} catch {}
// try {} catch ([x,y]) {}
// try {} catch (x: T) {} finally {}
// do {} while (true)
// while (true) {}

// switch (x) {
//   case 0: ;
//   default: yield; break;
//   case x,y: break;
//   default: ;
// }

// for (;;);
// for (;; i++);
// for (; i<6; i++);
// for (i=0; i<6; i++);
// for (i=0; i<6; i++) {}
// if (x) {}
// if (x) {} else { f() }
// declare const x = 4
// declare global {}
// declare module.m : T
// declare module M {}
// interface C { }
// declare class C { }

// declare module M {
//   interface C1 { }
//   class C1 { }
//   interface C1 { }
//   interface C1 { }
//   export class C2 { }
// }

// declare module M {
//   export interface C2 { }
// }

// async function* f () {}
// async function* () {}
// const x = async function* f () {}

// switch (`abc${0}abc`) {
//     case `123`:
//     case `abc${0}abc`:
//         `def${1}def`, a; 1;
// }

type T1<T U V> = T1
