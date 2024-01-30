let sum1 = List.fold_left (fun (a,i) -> a + i) 0 [1; 2; 3]
let sum2 = List.fold_right (fun (i,a) -> i + a) [1; 2; 3] 0