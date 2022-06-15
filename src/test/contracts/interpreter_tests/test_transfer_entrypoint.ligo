type parameter is
    | Default of unit
    | Other of int

function main (const p : parameter; const s: unit) : list (operation) * unit is
    case p of [
        | Default(_) -> (nil, s)
        | Other(_) -> (nil, s)
    ];

const test = {
    const origination = Test.originate(main, Unit, 1tez);
    const i_contract : contract(int) = Test.to_entrypoint("other", origination.0);
    const _ = Test.transfer_to_contract_exn(i_contract, 42, 1tez);
    const u_contract : contract(unit) = Test.to_entrypoint("default", origination.0);
    const _ = Test.transfer_to_contract_exn(u_contract, Unit, 1tez);
} with (Unit)
