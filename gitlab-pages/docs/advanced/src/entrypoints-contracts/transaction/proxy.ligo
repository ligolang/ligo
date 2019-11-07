#include "counter.types.ligo"

// Replace the following address with your deployed counter contract address
const address: address = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3": address);

function proxy(const param: action; const store: unit): (list(operation) * unit)
    is block {
        const counter: contract(action) = get_contract(address);
        // re-use the param passed to the proxy in the subsequent transaction
        // e.g.:
        // const mockParam: action = Increment(5);
        const op: operation = transaction(param, 0mutez, counter);
        const opList: list(operation) = list op; end;
    } with (opList, store)