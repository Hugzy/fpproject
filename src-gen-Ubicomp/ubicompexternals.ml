open Yojson.Basic.Util

(* Implement this method to combine your state and id *)
let combine_state_id stateItem id =
	let sutJson = Yojson.Basic.from_string ("{\"userid\": \"" ^ id ^ "\"}") in
    Yojson.Basic.Util.combine sutJson stateItem 

(* Implement this method to extract your id from a json body *)
		let extractIdFromContent (content:Yojson.Basic.t) : string =
			content |> member "id" |> to_string

(* Implement this method to cleanup after each test *)
		let afterTestcleanup =
			ignore(Http.rawpost ("http://localhost:3000" ^ "/api/reset") "")
