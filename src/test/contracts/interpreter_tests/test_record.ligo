type internal_storage is [@layout comb]
  record [
    value : int;
  ]

type storage is record [
  internal_storage : internal_storage;
]

const initial_storage: storage = record [
    internal_storage = record [
      value = 0;
    ];
  ];

const test_reproducing = {
  Test.log(Test.eval(initial_storage));
} with ("OK")
