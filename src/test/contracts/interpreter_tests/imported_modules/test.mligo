#import "main.mligo" "Main"

let test1 =
  let (_,_,_) = Test.originate_uncurried Main.main "a" 1tez in
  ()