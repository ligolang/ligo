function foo (const input : int) : int is begin
   skip
end with (input + 42)

function main (const i : int) : int is
  begin
    skip
  end with i + foo (i)
