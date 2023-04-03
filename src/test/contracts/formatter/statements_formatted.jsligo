let gcd = (x: nat, y: nat): nat => {
  if (x < y) {
    const z: nat = x;
    x = y;
    y = z
  };
  let r: nat = 0 as nat;
  while (y != (0 as nat)) {
    r = x % y;
    x = y;
    y = r
  };
  return x
};
