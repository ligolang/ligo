// accountBalances is a simple type, a map of address <-> tez
type accountBalances is map(address, tez);

const ledger: accountBalances = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> 10mutez
end