const broken_unzipped_entries = (x : list<[int, int]>) : [list<int>, list<int>] => {
    let keys : list<int> = [];
    let values : list<int> = [];
    for (const [k, v] of x) {
      keys = [k, ...keys];
      values = [v, ...values];
    };
    return [keys, values];
};
