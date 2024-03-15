module Module_x = struct
  type storage =
    {
     content : int;
     metadata : (string, bytes) big_map
    }

  [@entry]
  let main (_p : unit) (s : storage) : operation list * storage = [], s

  let storage : storage =
    {
     content = 0;
     metadata = Big_map.literal []
    }
  end
