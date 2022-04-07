open Stage_common.Enums

let literal_tag = literal_to_enum

let literal a b = Int.compare (literal_tag a) (literal_tag b)