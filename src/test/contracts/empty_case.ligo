type foo is
  | Bar of int
  | Baz

function main (const f: foo) : int is 
  (case f of 
  | Bar (n) -> n
  | Baz -> -1
  end)
