type parameter is
    | Default of unit
    | OtherMethod of int

function main (const p : parameter; const s: unit) : list (operation) * unit is
    case p of [
        | Default(_) -> (nil, s)
        | OtherMethod(_) -> (nil, s)
    ];

const test_entrypoint = {
    const origination = Test.originate(main, Unit, 1tez);
    const contract : contract(int) = Test.to_entrypoint("otherMethod", origination.0);
    const _ = Test.transfer_to_contract_exn(contract, 42, 1tez);
} with ("Passed")
