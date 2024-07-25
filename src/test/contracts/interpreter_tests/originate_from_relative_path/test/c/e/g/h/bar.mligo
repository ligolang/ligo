module Test = Test.Next

let f = "../../../../../src/contract/unit.mligo"

let originate () =
    let x = Test.Originate.from_file f () 0tez in
    x.taddr
