type foo = {
  i : int;
}

let bar (x : foo) : int = 
  let y : string = x.i in
  42
