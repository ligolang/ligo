type foo<a> = [a , unit]
type bar<a,b> = [foo<a> , b]

let v : bar<int,string> = [[1 , unit] , "one"] ;
let [[x,unit],y] = v ;
let t1 : [int , string] = [x,y];

//type foo<a> = [a , unit]
//type bar<a,b> = [foo<a> , b]
//
//let t1 : [int , string] =
//  let v : bar<int,string> = [[1 , unit] , "one"] ;
//  let [[x,unit],y] = v ;
//  [x,y]

// type 'a foo = 'a list
// type bar = int foo

// let t2 : int list =
//   let f (x: int list) : bar =
//     List.map (fun (i:int) -> i +1) x
//   in
//   let z : bar = [ 1 ; 2 ; 3 ] in
//   f z

namespace Foo {
   export type t<a> =
   | ["Bar", a]
   export type s<a, b> =
   t<a>
}

type baz<a, b> = Foo.s<a, b>;

const t2 : Foo.t<int> = Bar(42);
const t3 : baz<int, string> = t2;
