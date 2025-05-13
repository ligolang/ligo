type account_balances = (address, tez) map

let ledger : account_balances =
  Map.literal
    [(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), 10mutez)]