// Test the option type in PascaLIGO

type foobar is option(int)

const s : foobar = Some(42)
const n : foobar = None

function assign (var m : int) : foobar is
  var coco : foobar := None;
  block
{
      coco := Some(m);
      coco := None;
}
with coco
