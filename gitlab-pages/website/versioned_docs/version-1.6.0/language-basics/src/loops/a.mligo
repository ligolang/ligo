let get_char s idx = String.sub idx 1n s

let is_palindrome s =
  let mut p = "" in
  let length = String.length s in
  let () =
    for i = length - 1 downto 0 do
      p := p ^ get_char s (abs i)
    done
  in p = s