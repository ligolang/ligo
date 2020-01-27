type myrec is record
  foo : nat;
  bar : string;
end;

const a : myrec = record
  foo = 0n;
  bar = "bar";
end;

const b : int = 2 ;