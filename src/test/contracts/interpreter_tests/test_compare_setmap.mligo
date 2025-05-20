module Michelson = Test.Michelson

let test_address_set =
 let s : address set = Set.empty in
 let s = Set.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) s in
 let s = Set.add ("tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ" : address) s in
 let s : address set = Michelson.decompile (Michelson.eval s) in
 Michelson.eval s

let test_int_set =
 let s : int set = Set.empty in
 let s = Set.add 4 s in
 let s = Set.add 3 s in
 let s : int set = Michelson.decompile (Michelson.eval s) in
 Michelson.eval s

let test_map =
 let s : (address, int) map = Map.empty in
 let s = Map.add ("KT1WoTZUkky48v3QqZWzkeJCYfhWhNaVFYuC" : address) 100 s in
 let s = Map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
 let s : (address, int) map = Michelson.decompile (Michelson.eval s) in
 Michelson.eval s

let test_big_map =
 let s : (address, int) big_map = Big_map.empty in
 let s = Big_map.add ("KT1WoTZUkky48v3QqZWzkeJCYfhWhNaVFYuC" : address) 100 s in
 let s = Big_map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
 let s : (address, int) big_map = Michelson.decompile (Michelson.eval s) in
 Michelson.eval s
