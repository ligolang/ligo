// This is interpret.mligo
type myDataType = (int, string) map

let encodeEntry (a : int) (b : string): myDataType =
  Map.literal [(a, b)]