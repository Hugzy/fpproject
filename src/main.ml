open QCheck
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util


module MyState = Map.Make(String)

module APIConf =
struct
  type sut = unit

  type state = []
  let rec drop n h = if n == 0 then h else (drop (n-1) (match h with
      | a::b -> b
      | [] -> []))
  let lookupItem ix state = List.hd (drop (ix mod List.length state) (List.rev state))
  let extract_id json =
    [json]
    |> filter_member "id"
    |> filter_string

  let lookupId ix state = let mItem = lookupItem ix state in
    let json  = Yojson.Basic.from_string mItem in
    List.hd (extract_id json)

  type cmd =
    | Get of int
    | Create [@@deriving show { with_path = false }]

  let arb_cmd state = 
    let int_gen = Gen.oneof [Gen.small_int] in 
    QCheck.make ~print:show_cmd
      (Gen.oneof [Gen.return Create;
                  Gen.map (fun i -> Get i) int_gen])

  let init_state = []

  let next_state cmd state = match cmd with
    | Get ix -> state
    | Create -> state 

  let addItemToState sutItem state = state@[sutItem]


  let init_sut () = 0
  let cleanup _  = 0

  let url = "http://localhost:3000"
  let get = "/api/shop/get/"
  let create = "/api/shop/create/item"

  let bodyget id = Client.get (Uri.of_string (url ^ get ^ id )) >>= fun (resp, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
  let bodycreate = Client.post (Uri.of_string (url ^ create)) >>= fun (resp, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
  (* Returns response from Post request *)
  let create_request = let body = bodycreate in body

  (* Returns response from get request with id *)
  let get_request id = let body = bodyget id in body

  let run_cmd cmd state sut = match cmd with
    | Get ix -> String.compare (get_request (lookupItem ix state)) (lookupItem ix state) 
    | Create -> let sutItem = create_request
                    addItemToState sutItem state
                    true

  let precond _ _ = true
end
module APItest = QCSTM.Make(APIConf)
;;
let () = 
  Lwt_main.run (QCheck_runner.run_tests ~verbose:true
                  [APItest.agree_test ~count:100 ~name:"Api Model agreement"])