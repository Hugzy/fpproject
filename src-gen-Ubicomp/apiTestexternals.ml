(* Implement this method to combine your state and id *)
		let combine_state_id stateItem id =
			let combined = ... in

			combined

(* Implement this method to extract your id from a json body *)
		let extractIdFromContent (content:Yojson.Basic.t) : string =
			let extracted = ... in

			extracted

(* Implement this method to cleanup after each test *)
		let afterTestcleanup =
			...
