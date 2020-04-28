open Yojson
(* Implement this method to combine your state and id *)
val combine_state_id : Yojson.Basic.t -> string -> Yojson.Basic.t
	

(* Implement this method to extract your id from a json body *)
val extractIdFromContent : Yojson.Basic.t -> string

(* Implement this method to cleanup after each test *)
val afterTestcleanup : unit
