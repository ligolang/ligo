
const tests =
  [ ("666f6f" as bytes) == 0x666f6f
  , (bytes `foo`)     == 0x666f6f
  , (bytes `666f6f`)  == 0x363636663666
  ];

/* annotation might go wrong here */

const realistic : bytes =
  (bytes
  `
    {
    "name":"name",
    "description":"description",
    "version":"0.0.0",
    "license":{"name":"liname"},
    "authors":["authors"],
    "homepage":"",
    "source":{"tools":["tools"], "location":"location"},
    "interfaces":["TZIP"],
    "errors":[],
    "views":[]
    }
  ` as bytes);