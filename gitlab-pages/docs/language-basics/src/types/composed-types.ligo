// alias two types
type account is address;
type numberOfTransactions is nat;
// accountData consists of a record with two fields (balance, numberOfTransactions)
type accountData is record
    balance: tez;
    numberOfTransactions: numberOfTransactions;
end
// our ledger / accountBalances is a map of account <-> accountData
type accountBalances is map(account, accountData);

// pseudo-JSON representation of our map 
// { "tz1...": {balance: 10mutez, numberOfTransactions: 5n} }
const ledger: accountBalances = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> record
      balance = 10mutez;
      numberOfTransactions = 5n;
    end
end