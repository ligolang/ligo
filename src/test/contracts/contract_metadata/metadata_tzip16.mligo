type param = int

(* --------------------------------------------------------------------------- *)
(* no metadata here *)

module Entry_no_metadata = struct
  type storage = {data : int}

  let main (_p, s : param * storage) : operation list * storage = [], s

end

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format *)

module Entry_invalid_metadata_1 = struct
  type storage =
    {
     data : int;
     metadata : nat
    }

  let main (_p, s : param * storage) : operation list * storage = [], s

end

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format (it's a big_map but params are reversed) *)

module Entry_invalid_metadata_2 = struct
  type storage =
    {
     data : int;
     metadata : (bytes, string) big_map
    }

  let main (_p, s : param * storage) : operation list * storage = [], s

end

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format (as the last one, but using annotation) *)

module Entry_invalid_metadata_3 = struct
  type storage =
    {
     data : int;
     [@annot metadata] notdata : (bytes, string) big_map
    }

  let main (_p, s : param * storage) : operation list * storage = [], s

end

(* --------------------------------------------------------------------------- *)
(* metadata with correct format *)

module Entry_valid_metadata = struct
  type storage =
    {
     data : int;
     metadata : (string, bytes) big_map
    }

  let main (_p, s : param * storage) : operation list * storage = [], s

  let off_view (p : int) (s : storage) : int = p + s.data

end

type storage = Entry_valid_metadata.storage

let json =
  [%bytes
  {| {
  "name":"FA2 NFT Marketplace",
  "description":"Example of FA2 implementation",
  "version":"0.0.1",
  "license":{"name":"MIT"},
  "authors":["Marigold<contact@marigold.dev>"],
  "homepage":"https://marigold.dev",
  "source":{
  "tools":["Ligo"],
  "location":"https://github.com/ligolang/contract-catalogue/tree/main/lib/fa2"},
  "interfaces":["TZIP-012"],
  "errors": [],
  "views": [
    {
      "name": "get-allowance-for-user",
      "description": "Get the current allowance for a user of the contract.",
      "pure": "true",
      "implementations": [
         { "michelsonStorageView" : {"parameter":{"prim":"int"},"returnType":{"prim":"int"},"code":[{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"ADD"}]} }
        ]
    }
    ]
  }
  |}]

let json_bad_view_type =
  [%bytes
  {| {
  "name":"FA2 NFT Marketplace",
  "description":"Example of FA2 implementation",
  "version":"0.0.1",
  "license":{"name":"MIT"},
  "authors":["Marigold<contact@marigold.dev>"],
  "homepage":"https://marigold.dev",
  "source":{
  "tools":["Ligo"],
  "location":"https://github.com/ligolang/contract-catalogue/tree/main/lib/fa2"},
  "interfaces":["TZIP-012"],
  "errors": [],
  "views": [
    {
      "name": "get-allowance-for-user",
      "description": "Get the current allowance for a user of the contract.",
      "pure": "true",
      "implementations": [
         { "michelsonStorageView" : {"parameter":{"prim":"string"},"returnType":{"prim":"int"},"code":[{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"ADD"}]} }
        ]
    }
    ]
  }
  |}]

let good_storage : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("", Bytes.concat [%bytes "tezos-storage:hello%2F"] [%bytes "world"]);
         ("hello/world", json)
       ]
  }

let bad_storage0 : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("",
          Bytes.concat
            [%bytes "tezos-storage:hello/"]
            [%bytes "invalid_not_http"]);
         ("hello/world", [%bytes "JSON?"]);
         ("invalid_not_http", [%bytes "https://www.example.com"]);
         ("invalid_trailing_slash",
          [%bytes "ipfs://QmWqi3uBhBQ5KU6sR1LpLqJTr4GxuPfEK7UDyv6Gcc3fHL/"]);
         ("invalid_wrong_hash", [%bytes "ipfs://invalid-hash"])
       ]
  }

let bad_storage1 : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("", [%bytes "haha"]);
         ("hello/world", [%bytes "http://www.example.com"])
       ]
  }

let bad_storage2 : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("", [%bytes "tezos-storage:haha"]);
         ("hello/world", [%bytes "http://www.example.com"])
       ]
  }

let bad_storage3 : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [("", [%bytes "tezos-storage:haha"]); ("haha", [%bytes "nojson!"])]
  }

let bad_storage4 : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("", Bytes.concat [%bytes "tezos-storage:hello%2F"] [%bytes "world"]);
         ("hello/world", json_bad_view_type)
       ]
  }

let bad_http_storage : storage =
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

let good_http_storage : storage =
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

let ipfs_storage : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [("", [%bytes "ipfs://QmSBc8QuynU7bArUGtjwCRhZUbJyZQArrczKnqM7hZPtfV"])]
  }

let good_http_sha256_storage : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("",
          [%bytes
          "sha256://0x1f7c7fde391bc46a28cd98a77733fbe5927190dbe5493fe81244ee29dc624577/https:%2F%2Fipfs.io%2Fipfs%2FQmSBc8QuynU7bArUGtjwCRhZUbJyZQArrczKnqM7hZPtfV"])
       ]
  }

let bad_http_sha256_storage : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("",
          [%bytes
          "sha256://0xff7c7fde391bc46a28cd98a77733fbe5927190dbe5493fe81244ee29dc624577/https:%2F%2Fipfs.io%2Fipfs%2FQmSBc8QuynU7bArUGtjwCRhZUbJyZQArrczKnqM7hZPtfV"])
       ]
  }

let hen_metadata =
  [%bytes
  {|{"authors":["@hicetnunc2000 <hicetnunc2000@protonmail.com>"],"description":"OBJKTs FA2 collectibles","homepage":"https://hicetnunc.xyz","interfaces":["TZIP-12"],"license":{"name":"MIT"},"name":"OBJKTs","repository":"https://github.com/hic2nc2000","version":"2.0.0"}|}]

let good_tezos_sha256_storage : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("",
          [%bytes
          "sha256://0x1f7c7fde391bc46a28cd98a77733fbe5927190dbe5493fe81244ee29dc624577/tezos-storage:m"]);
         ("m", hen_metadata)
       ]
  }

let bad_tezos_sha256_storage : storage =
  {
   data = 42;
   metadata =
     Big_map.literal
       [
         ("",
          [%bytes
          "sha256://0xff7c7fde391bc46a28cd98a77733fbe5927190dbe5493fe81244ee29dc624577/tezos-storage:m"]);
         ("m", hen_metadata)
       ]
  }
