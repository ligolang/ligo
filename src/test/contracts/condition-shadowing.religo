/* TODO : make a test using mutation, not shadowing */

let main = (i: int) => {
  let result = 0;
  if (i == 2) {
    let result = 42;
    result;
  } else {
    let result = 0;
    result;
  };
};
