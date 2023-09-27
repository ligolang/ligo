let f = "../../../../../src/contract/unit.mligo"

let originate () =
    let x : (unit, unit) origination_result = Test.originate_from_file f () 0tez in
    x.addr