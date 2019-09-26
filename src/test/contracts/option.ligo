// Test the option type in PascaLIGO

type foobar is option(int)

const s : foobar = Some(42)
const n : foobar = None
