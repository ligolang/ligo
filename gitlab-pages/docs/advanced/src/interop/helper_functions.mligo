type z_to_v =
  Z
| Y
| X
| W
| V

type w_or_v = (unit, "w", unit, "v") michelson_or
type x_or = (unit, "x", w_or_v, "other") michelson_or
type y_or = (unit, "y", x_or, "other") michelson_or
type z_or = (unit, "z", y_or, "other") michelson_or

type test = {
  z: string;
  y: int;
  x: string;
  w: bool;
  v: int
}

let make_concrete_sum (r: z_to_v) : z_or =
  match r with
    Z -> M_left (unit)
  | Y -> M_right (M_left (unit))
  | X -> M_right (M_right (M_left (unit)))
  | W -> M_right (M_right (M_right (M_left (unit))))
  | V -> M_right (M_right (M_right (M_right (unit))))

let make_concrete_record (r: test) =
  (r.z, r.y, r.x, r.w, r.v)

let make_abstract_sum (z_or: z_or) =
  match z_or with
  | M_left n -> Z
  | M_right y_or ->
    (match y_or with
    | M_left n -> Y
    | M_right x_or -> (
        match x_or with
        | M_left n -> X
        | M_right w_or -> (
            match w_or with
            | M_left n -> W
            | M_right n -> V)))

let make_abstract_record z y x w v =
  { z = z; y = y; x = x; w = w; v = v }
