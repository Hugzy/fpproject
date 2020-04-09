let xs = [1]

(* replace 2 xs *)
let replace pos l = List.mapi (fun i x -> if i = pos then None else Some(x)) l



let rec remove pos list = match (list, pos) with
    | ([], _) -> []
    | (head::tail, 0) -> tail
    | (head::tail,_) -> [head]@(remove (pos-1) tail)
