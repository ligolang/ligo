type storage =
  {
   data : int;
   metadata : (string, bytes) big_map
  }

[@tzip16_compatible]
let storage_sample : storage =
  {
   data = 5;
   metadata =
     Big_map.literal
       [
         ("",
          [%bytes
          "https://ipfs.io/ipfs/QmSBc8QuynU7bArUGtjwCRhZUbJyZQArrczKnqM7hZPtfV"])
       ]
  }

[@entry]
let main (_ : string) (store : storage) : operation list * storage = ([], store)
