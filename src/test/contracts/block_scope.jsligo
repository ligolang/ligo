
// should work and return 3
function test_1 () : int {
    const _a = 2;
    {
        const _a = 3;
        return _a
    };
    return _a
}

// should work and return 3
function test_2 () : int {
    const _a = 2;
    {
        {
            const _a = 3;
            return _a
        }
    };
    return _a
}

// should work and return 3
function test_3 () : int {
    const _a = 2;
    {
        const _a = 3;
        return _a
    };
}

// should work and return 3
function test_4 () : int {
    const _a = 2;
    {
        {
            const _a = 3;
            return _a
        }
    };
}

// should work and return 2
function test_5 () : int {
    const _a = 2;
    {
        const _a = 3;
    };
    return _a
}

// should work and return 2
function test_6 () : int {
    const _a = 2;
    {
        {
            const _a = 3;
        }
    };
    return _a
}
