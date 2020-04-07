let xs = [1;2;3;4;5]

(* replace 2 xs *)
let replace pos l = List.mapi (fun i x -> if i = pos then None else Some(x)) l