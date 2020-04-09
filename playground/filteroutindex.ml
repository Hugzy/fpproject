let xs = [0; 1; 2; 3; 4; 5]

(* replace 2 xs *)
let replace pos l = List.mapi (fun i x -> if i = pos then None else Some(x)) l



let rec remove pos list = match (list, pos) with
  | ([], _) -> []
  | (head::tail, 0) -> tail
  | (head::tail,_) -> [head]@(remove (pos-1) tail)

let getPos ix list = ((List.length list - 1) - (ix mod List.length list))

let replaceElem pos list newelem = List.mapi (fun i x -> if i = pos then newelem else x) list