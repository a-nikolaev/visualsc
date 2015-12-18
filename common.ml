
let rec fold_lim f acc n0 n = if n0 > n then acc else fold_lim f (f acc n0) (n0+1) n
