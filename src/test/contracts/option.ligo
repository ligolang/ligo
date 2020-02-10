// Test the option type in PascaLIGO

type foobar is option(int)

const s : foobar = Some(42)
const n : foobar = None

function assign (var m : int) : foobar is
  block {
    var coco : foobar := None;
    coco := Some(m);
    coco := (None : foobar); //temporary annotation added until type inference
  } with coco
