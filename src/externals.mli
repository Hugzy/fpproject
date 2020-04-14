(* Implement this method to combine your state and id *)
let combine_state_id (stateItem:Yojson.Basic.t) (id:string) : Yojson.Basic.t =
	

(* Implement this method to extract your id from a json body *)
let extractIdFromContent (content:Yojson.Basic.t) : string

(* Implement this method to cleanup after each test *)
let afterTestcleanup : unit