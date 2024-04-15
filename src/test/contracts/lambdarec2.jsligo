
const wrong = (x : int) : int => {
  const l : list<int> = [x, x];
  let total = 0;
  for (const i of l) {
    total = total + wrong(i)
  }
  return total;
};
