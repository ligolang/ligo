type myt = Nil(unit) | Cons ((int, int))

let t = (x: myt) => (y: myt) =>
  switch(x) {
  | Nil => (
    switch(y) {
    | Nil => 1
    | Cons((a,b)) =>
      let a = "a";
      (int (String.length(a))) + b
    }
  )
  | Cons((a,b)) =>
    let old_b = b;
    let b =
      switch(y) {
      | Nil =>
        let f = (b:int) => b + a;
        f (b+1)
      | Cons ((a,b)) => "invalid"
      };
    a + old_b + b
  }