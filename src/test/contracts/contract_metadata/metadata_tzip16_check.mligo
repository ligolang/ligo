#include "metadata_tzip16_check_included.mligo"

[@tzip16_compatible]
let good_storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("",
          [%bytes
          "https://ipfs.io/ipfs/QmSBc8QuynU7bArUGtjwCRhZUbJyZQArrczKnqM7hZPtfV"])
       ]
  }

[@tzip16_compatible]
let bad_storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("",
          [%bytes
          "https://ipfs.iasfao/iadsfdsfaspfs/QmSBc8QuynU7bArUGtjwCRhZUbJyZQArrczKnqM7hZPtfV"])
       ]
  }

[@tzip16_compatible]
let no_metadata = {data = 42}

[@tzip16_compatible]
let bad_metadata_type =
  {
   data = 42;
   metadata = Big_map.literal ([("", "a")])
  }
