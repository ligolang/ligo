type record_1 =
  { numeric : int
  ; stringy : string
  }

let record_1 : record_1 = { numeric = 42; stringy = "hello" }

let hello : string = record_1.

type record_2 =
  { record_1 : record_1
  }

let record_2 : record_2 = { record_1 }

let hello : string = record_2.record_1.s
