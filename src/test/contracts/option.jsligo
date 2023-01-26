type foobar = option <int>;

const s : foobar = Some (42);
const n : foobar = None ();
const i : int    = Option.unopt (s);


const assign = (m : int) : foobar => {
   let coco : foobar = None ();
   coco = Some (m);
   coco = (None () as foobar);
   return coco;
}

const f = (m : foobar) : int => {
  if (Option.is_none(m)) {
    return Option.value(1, m);
  } else {
    if (Option.is_some(m)) {
      return Option.value_exn("won't happen", m);
    } else {
      return -1;
    }
  }
}

const j = f(s);
const k = f(n);