let var = () => {
  let a = 2;
  let b = 0;
  let c = 0;
  c = b = a;
  return [c, b, a]
}

let tuple = () => {
  let a = [2, 3];
  let b = 0;
  let c = 0;
  a[1] = c;

  c = a[1] = a[0] = 2;
  assert(a[1] == 2);
  assert(a[0] == 2);
  assert(c == 2);
  
  let x = c = a[1] = 9; 
  assert(x == 9);
  assert(c == 9);
  assert(a[1] == 9);

  let d = c = a[1] = c = 5;
  assert(d == 5);
  assert(c == 5);
  assert(a[1] == 5);

  let d2 = c = d = a[1] = c = 6;
  assert(d2 == 6);
  assert(d == 6);
  assert(c == 6);
  assert(a[1] == 6);
  assert(a[0] == 2);
  assert(b == 0);

  let d3 = a[1] = c = 7;
  assert(d3 == 7);
  assert(c == 7);
  assert(a[1] == 7);
  
  return [c, b, a[1]]
}


