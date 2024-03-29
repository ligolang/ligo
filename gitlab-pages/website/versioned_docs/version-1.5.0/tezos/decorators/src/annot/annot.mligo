type transfer =
  [@layout comb]
  { [@annot from] address_from : address;
    [@annot to] address_to : address;
    value : nat }