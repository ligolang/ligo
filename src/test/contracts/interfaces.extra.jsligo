type storage = int;

interface FA0 {
  // @entry
  add : (s : int, k : storage) => [list<operation>, storage];
}

class Impl implements FA0 {
  @entry
  add = (s : int, k : int) : [list<operation>, int] => [[], s + k];

  @entry
  extra = (s : int, k : int) : [list<operation>, int] => [[], s - k];
}
