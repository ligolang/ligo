
type parameter = { one : int , two : int };

let main = (action : parameter) : int =>
  switch(action) {
  | {one : _ , three : _} => 0
  }