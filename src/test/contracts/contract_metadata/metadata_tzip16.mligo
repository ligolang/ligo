
type param = int

(* --------------------------------------------------------------------------- *)
(* no metadata here *)
type storage =
  { data     : int
  }
let entry_no_metadata (_p, s : param * storage) : operation list * storage =
  [], s

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format *)
type storage =
  { data     : int
  ; metadata : nat
  }
let entry_invalid_metadata_1 (_p, s : param * storage) : operation list * storage =
  [], s

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format (it's a big_map but params are reversed) *)
type storage =
  { data     : int
  ; metadata : (bytes, string) big_map
  }
let entry_invalid_metadata_2 (_p, s : param * storage) : operation list * storage =
  [], s

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format (as the last one, but using annotation) *)
type storage =
  { data     : int
  ; [@annot metadata] notdata : (bytes, string) big_map
  }
let entry_invalid_metadata_3 (_p, s : param * storage) : operation list * storage =
  [], s

(* --------------------------------------------------------------------------- *)
(* metadata with correct format *)
type storage =
  { data     : int
  ; metadata : (string, bytes) big_map
  }
let entry_valid_metadata (_p, s : param * storage) : operation list * storage =
  [], s
