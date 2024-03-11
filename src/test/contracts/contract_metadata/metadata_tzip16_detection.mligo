type param = int

(* --------------------------------------------------------------------------- *)
(* no metadata here *)
module Entry_no_metadata = struct
  type storage = {data : int}

  [@entry]
  let main (_p : param) (s : storage) : operation list * storage = [], s
  end

(* --------------------------------------------------------------------------- *)
(* metadata with incorrect format (it's a big_map but params are reversed) *)
module Entry_invalid_metadata = struct
  type storage =
    {
     data : int;
     metadata : (bytes, string) big_map
    }

  [@entry]
  let main (_p : param) (s : storage) : operation list * storage = [], s
  end

(* --------------------------------------------------------------------------- *)
(* metadata with correct format *)
module Entry_valid_metadata = struct
  type storage =
    {
     data : int;
     metadata : (string, bytes) big_map
    }

  [@entry]
  let main (_p : param) (s : storage) : operation list * storage = [], s

  let off_view (p : int) (s : storage) : int = p + s.data
  end

module Entry_valid_metadata_with_dynamic_entrypoints = struct
  type storage =
    {
     storage
     : {
        data : int;
        metadata : (string, bytes) big_map
       };
     dynamic_entrypoints
    }

  [@entry]
  let main (_p : param) (s : storage) : operation list * storage = [], s

  [@dyn_entry]
  let foo () s : operation list * storage = [], s

  let off_view (p : int) (s : storage) : int = p + s.storage.data
  end

type storage = Entry_valid_metadata.storage

(* Note: in this file we do not care about metadata validity, only about whether
   tzip16 attribute is handled *)
let storage_simple_case : storage =
  {
   data = 0;
   metadata = Big_map.literal []
  }

let storage_without_explicit_type =
  {
   data = 0;
   metadata = (Big_map.literal [] : (string, bytes) big_map)
  }

[@tzip16_compatible]
let storage_with_attribute : storage =
  {
   data = 0;
   metadata = Big_map.literal []
  }

let storage_without_metadata_field : Entry_no_metadata.storage = {data = 0}

let storage_with_invalid_metadata : Entry_invalid_metadata.storage =
  {
   data = 0;
   metadata = Big_map.literal []
  }

let storage_of_no_existing_storage_type =
  {
   data = {meq = ""};
   metadata = (Big_map.literal [] : (string, bytes) big_map)
  }

module Wrapper = struct
  module Inner_wrapper = struct
    let storage_inside_module : storage =
      {
       data = 0;
       metadata = Big_map.literal []
      }
    end
  end

let storage_from_multi_binding, and_another_storage : storage * storage =
  let make (a : int) =
    {
     data = a;
     metadata = Big_map.literal []
    } in
  (make 1, make 2)
