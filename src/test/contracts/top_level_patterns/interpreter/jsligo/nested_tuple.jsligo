const _ = Test.set_print_values ()

const f = () => { 
    Test.log("Once");
    return [[1 as nat, 1, "H"], [2 as nat, 2, "E"], [3 as nat, 3, "L"]]
}
const [[a1, a2, a3], [b1, b2, b3], [c1, c2, c3]] = f ()

const [[a4, a5, a6], [b4, b5, b6], [c4, c5, c6]] 
  = [[1 as nat, 1, "H"], [2 as nat, 2, "E"], [3 as nat, 3, "L"]]

const _test = () => {
    assert ([a1 + b1 + c1] == [a4 + b4 + c4]);
    assert ([a2 + b2 + c2] == [a5 + b5 + c5]);
    assert ([a3 + b3 + c3] == [a6 + b6 + c6])
}

const test = _test()