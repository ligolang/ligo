#import "FA1.2.entries.mligo" "FA12_IMPL_ENTRIES"

module type FA12_IFACE = sig

  type transfer =
    [@layout:comb]
    { [@annot:from] address_from : address;
      [@annot:to] address_to : address;
      value : nat }

  type approve =
    [@layout:comb]
    { spender : address;
      value : nat }

  type allowance_key =
    [@layout:comb]
    { owner : address;
      spender : address }

  type getAllowance =
    [@layout:comb]
    { request : allowance_key;
      callback : nat contract }

  type getBalance =
    [@layout:comb]
    { owner : address;
      callback : nat contract }

  type getTotalSupply =
    [@layout:comb]
    { request : unit ;
      callback : nat contract }

  type storage

  type result = operation list * storage

  [@entry] val transfer : transfer -> storage -> result

  [@entry] val approve : approve -> storage -> result

  [@entry] val getAllowance : getAllowance -> storage -> result

  [@entry] val getBalance : getBalance -> storage -> result

  [@entry] val getTotalSupply : getTotalSupply -> storage -> result

end

module FA12_ENTRIES : FA12_IFACE = FA12_IMPL_ENTRIES
