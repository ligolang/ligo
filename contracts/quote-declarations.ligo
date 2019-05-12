function foo (const input : int) : int is begin
   skip
end with (input + 23)

function bar (const input : int) : int is begin
   skip
end with (input + 51)


function main (const i : int) : int is
  begin
    skip
  end with foo (i) + bar (i)
