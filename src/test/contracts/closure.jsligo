/* Test whether closures retain values in JsLIGO */

function test (k : int) : int {
  const j : int = k + 5
  const close : ((a: int) => int) = (i : int) => i + j

  // let j : int = 20 /* block scoped variables can't be reused in JsLIGO */
  return close (20)
}
