(* Implement this method to combine your state and id *)
let combine_state_id stateItem id =
	let sutJson = Yojson.Basic.from_string ("{\"id\": " ^ id ^ "}") in
    Yojson.Basic.Util.combine stateItem sutJson
	

(* Implement this method to extract your id from a json body *)
let extractIdFromContent (content:Yojson.Basic.t) : string =
	 string_of_int (content |> member "id" |> to_int)


(* Implement this method to cleanup after each test *)
let afterTestcleanup =
	ignore(Http.rawpost ("http://167.172.184.103" ^ "/api/shop/reset") "")