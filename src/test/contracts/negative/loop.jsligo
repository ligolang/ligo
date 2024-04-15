const broken_unzipped_entries = (x : map<int, int>) : [list<int>, list<int>] => {
    let keys : list<int> = [];
    let values : list<int> = [];
    for (const [k, v, z] of x) {
      keys = [k, ...keys];
      values = [v, ...values];
    };
    return [keys, values];
};
