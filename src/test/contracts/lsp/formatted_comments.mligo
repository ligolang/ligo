(* block comment attached to #define *)

#define I_AM_DIRECTIVE // trailing comment for #define
(* block comment attached to #include *)
// and a line comment
(* Another block comment attached to #include *)

#include "dummy.mligo" // trailing comment for #include
#import "dummy.mligo" "DUMMY" // trailing comment for #import

(* Block comment attached to decl *)
(* One more block comment attached to decl *)
let x = 3 + 4 //trailing comment for decl

//comment for #if

#if I_AM_DIRECTIVE //trailing comment for expr in #if

let c = 3
