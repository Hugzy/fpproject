open QCheck
open Yojson.Basic.Util
open Curl
open Format
open Http
open JsonServerexternals

	
module APIConf =
struct

  type sut = (string list) ref
  type state = string list

  type cmd =
  	 | Get of int
  	 | Post
  	 | Del of int
  	 [@@deriving show { with_path = false }]
  
  let getURL="http://localhost:3000/todos/"
  let postURL="http://localhost:3000/todos/"
  let delURL="http://localhost:3000/todos/"
 
  let init_state = []
  let init_sut() = ref []
  let cleanup _  =  afterTestcleanup
  
  (* Functions *)
  (* Recursively drops n heads of a list and returns the rest of the list *)
  let rec drop n h = if n == 0 then h else (drop (n-1) (match h with
      | a::b -> b
      | [] -> []))
  
  (* basically this https://en.wikipedia.org/wiki/De_Bruijn_index *)
  let lookupItem ix state = List.hd (drop (ix mod List.length state) (List.rev state))
  
  let lookupSutItem ix sut = List.hd (drop (ix mod List.length sut) (List.rev sut))
  
  let checkInvariant state sut = List.length state = List.length !sut
  
  let rec remove_item pos list = match (list, pos) with
    | ([], _) -> []
    | (head::tail, 0) -> tail
    | (head::tail,_) -> [head]@(remove_item (pos-1) tail)
  
  (* Wanting to get the index of the id back, length of list will always start at 1 for a given element but the first element is at index 0*)
  let getPos ix list = ((List.length list - 1) - (ix mod List.length list))
  
  let replaceElem pos list newelem = List.mapi (fun i x -> if i = pos then newelem else x) list
  
  let inSpace value state = List.mem (value) state
  
  let isEmpty state = (List.length state = 0)
 
  let arb_cmd state = 
    let int_gen = Gen.oneof [Gen.small_int] in
    if state = [] then
      QCheck.make ~print:show_cmd
      (Gen.oneof [
      (Gen.return Post)
      ])
      
    else
      QCheck.make ~print:show_cmd
        (Gen.oneof [
  	      		  Gen.map (fun i -> Get i) int_gen;
  	      		  (Gen.return Post);
  	      		  Gen.map (fun i -> Del i) int_gen
                   ])
 
  let next_state cmd state = match cmd with
  	| Get  ix -> state
  	| Post  -> state@["{\"foo\":\"FOO\",\"bar\":\"BAR\"}"]
  	| Del  ix -> state
 
  let run_cmd cmd state sut = match cmd with
  	| Get ix -> if (checkInvariant state sut) then 
  	let id = lookupSutItem ix !sut in
  		let code,content = Http.get (getURL^"/"^id) in
  	(let extractedState = lookupItem ix state in
  		let id = lookupSutItem ix !sut in
  			let stateJson = Yojson.Basic.from_string extractedState in
  				let combined = combine_state_id stateJson id in
  					 (String.compare (Yojson.Basic.to_string combined) (Yojson.Basic.to_string content) == 0)
  	) && ( (code == 200))
  	 else false
  	| Post  -> if (checkInvariant state sut) then 
  	let code,content = Http.post postURL "{\"foo\":\"FOO\",\"bar\":\"BAR\"}" in
  	let id = extractIdFromContent content in
  		sut := !sut@[id];
  	 (code == 201)
  	 else false
  	| Del ix -> if (checkInvariant state sut) then 
  	let id = lookupSutItem ix !sut in
  		let code,content = Http.delete (delURL^"/"^id) "" in
  	 (code == 200)
  	 else false
 
  let precond cmd state = match cmd with
      | Get ix -> (List.length state > 0)  
      	 && (isEmpty state = false)
      | Post -> true 
      | Del ix -> (List.length state > 0)  
      	 && (isEmpty state = false)
 
 end
 
 
 module APItest = QCSTM.Make(APIConf)
 ;; 
 
 QCheck_runner.run_tests ~verbose:true
   [APItest.agree_test ~count:10 ~name:"JsonServer"]
