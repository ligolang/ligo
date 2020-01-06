function main(const p : unit; const s : int) : list(operation) * int is
  ((list end : list(operation)), s + 1)

function main(const p : unit; const s : int) : list(operation) * int is
  begin
    const ret : list(operation) * int = main(p, s)
  end
  with (ret.0, ret.1 + 1)