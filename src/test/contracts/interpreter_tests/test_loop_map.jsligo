const testmap : map <int, int> =
  Map.literal ([
    [0, 1],
    [1, 2],
    [2, 4]]);

const entries = (x : map<int, int>) : list<[int,int]> => {
    let lst : list<[int,int]> = [];
    for (const kv of x) {
      lst = [kv, ...lst];
    };
    return lst
};

const unzipped_entries = (x : map<int, int>) : [list<int>, list<int>] => {
    let keys : list<int> = [];
    let values : list<int> = [];
    for (const [k, v] of x) {
      keys = [k, ...keys];
      values = [v, ...values];
    };
    return [keys, values];
};

const test_entries = entries(testmap);
const test_unzipped_entries = unzipped_entries(testmap);
