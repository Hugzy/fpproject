open QCheck
open Yojson.Basic.Util
open Curl
open Format
open Http
open JsonServerUserexternals

	
module APIConf =
struct

  type sut = (string list) ref
  type state = string list

  type cmd =
  	 | GetUser of int
  	 | PostUser
  	 | DelUser of int
  	 [@@deriving show { with_path = false }]
  
  let getUserURL="http://localhost:3000/users"
  let postUserURL="http://localhost:3000/users"
  let delUserURL="http://localhost:3000/users"
 
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
      (Gen.return PostUser)
      ])
      
    else
      QCheck.make ~print:show_cmd
        (Gen.oneof [
  	      		  Gen.map (fun i -> GetUser i) int_gen;
  	      		  (Gen.return PostUser);
  	      		  Gen.map (fun i -> DelUser i) int_gen
                   ])
 
  let next_state cmd state = match cmd with
  	| GetUser  ix -> state
  	| PostUser  -> state@["{\"firstname\":\"Neil\",\"lastname\":\"Down\",\"age\":\"too old\"}"]
  	| DelUser ix -> let pos = getPos ix state in
  	      (* Returns a list of all items except that which is 'item' found above *)
  	      let l = remove_item pos state in
  	      l
 
  let run_cmd cmd state sut = match cmd with
  	| GetUser ix -> if (checkInvariant state sut) then 
  	let id = lookupSutItem ix !sut in
  		let code,content = Http.get (getUserURL^"/"^id) in
  	(let extractedState = lookupItem ix state in
  		let id = lookupSutItem ix !sut in
  			let stateJson = Yojson.Basic.from_string extractedState in
  				let combined = combine_state_id stateJson id in
  					 (String.compare (Yojson.Basic.to_string combined) (Yojson.Basic.to_string content) == 0)
  	) && ( (code == 200))
  	 else false
  	| PostUser  -> if (checkInvariant state sut) then 
  	let code,content = Http.post postUserURL "{\"firstname\":\"Neil\",\"lastname\":\"Down\",\"age\":\"too old\"}" in
  	let id = extractIdFromContent content in
  		sut := !sut@[id];
  	 (code == 201)
  	 else false
  	| DelUser ix -> if (checkInvariant state sut) then 
  	let id = lookupSutItem ix !sut in
  		let code,content = Http.delete (delUserURL^"/"^id) in
  	let pos = getPos ix !sut in
  		sut := remove_item pos !sut;
  	 (code == 200)
  	 else false
 
  let precond cmd state = match cmd with
      | GetUser ix -> (List.length state > 0)  
      	 && (isEmpty state = false)
      | PostUser -> true 
      | DelUser ix -> (List.length state > 0)  
      	 && (isEmpty state = false)
 
 end
 
 
 module APItest = QCSTM.Make(APIConf)
 ;; 
 
 QCheck_runner.run_tests ~verbose:true
   [APItest.agree_test ~count:10 ~name:"JsonServerUser"]
