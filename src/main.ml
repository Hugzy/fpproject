open QCheck
open Yojson.Basic.Util
open Curl
open Format
open Http
open Externals

module APIConf =
struct
  (* Types *)
  type sut = (string list) ref
  type state = string list
  type cmd =
    | Get of int
    | Create 
    | Put of int
    | Delete of int [@@deriving show { with_path = false }]

  (* Constants *)
  let url = "http://167.172.184.103"
  let get = "/api/shop/get/"
  let create = "/api/shop/create/item"
  let put = "/api/shop/update/"
  let put_url = url ^ put
  let delete_url = url ^ "/api/shop/delete/"
  let init_state = []
  let init_sut() = ref []
  let cleanup _  =  ignore(Http.rawpost (url ^ "/api/shop/reset") "")

  (* Functions *)
  (* Recursively drops n heads of a list and returns the rest of the list *)
  let rec drop n h = if n == 0 then h else (drop (n-1) (match h with
      | a::b -> b
      | [] -> []))

  (* basically this https://en.wikipedia.org/wiki/De_Bruijn_index *)
  let lookupItem ix state = List.hd (drop (ix mod List.length state) (List.rev state))

  let lookupSutItem ix sut = List.hd (drop (ix mod List.length sut) (List.rev sut))

  let checkInvariant state sut = List.length state = List.length !sut

  let extract_id json =
    [json]
    |> filter_member "id"
    |> flatten
    |> filter_string

  let lookupId ix state = let mItem = lookupItem ix state in
    let json  = Yojson.Basic.from_string mItem in
    List.hd (extract_id json)

  let arb_cmd state = 
    let int_gen = Gen.oneof [Gen.small_int] in
    if state = [] then
      QCheck.make ~print:show_cmd
        (Gen.return Create)
    else
      QCheck.make ~print:show_cmd
        (Gen.oneof [ Gen.return Create;
                     Gen.map (fun i -> Delete i) int_gen;
                     Gen.map (fun i -> Get i) int_gen;
                     Gen.map (fun i -> Put i) int_gen])

  let rec remove_item pos list = match (list, pos) with
    | ([], _) -> []
    | (head::tail, 0) -> tail
    | (head::tail,_) -> [head]@(remove_item (pos-1) tail)

  (* Wanting to get the index of the id back, length of list will always start at 1 for a given element but the first element is at index 0*)
  let getPos ix list = ((List.length list - 1) - (ix mod List.length list))

  let replaceElem pos list newelem = List.mapi (fun i x -> if i = pos then newelem else x) list

  let next_state cmd state = match cmd with
    | Get ix -> state
    | Create -> state@["{\"name\": \"bar\"}"]
    | Delete ix -> let pos = getPos ix state in
      (* Returns a list of all items except that which is 'item' found above *)
      let l = remove_item pos state in
      l
    | Put ix -> let newelem = "{\"name\": \"foo\"}" in
      let pos = getPos ix state in
      replaceElem pos state newelem

  let run_cmd cmd state sut = match cmd with
    | Get ix -> if (checkInvariant state sut) then 
        let id = lookupSutItem ix !sut in
        let code,content = Http.get (url ^ get ^ id) in
        let extractedState = lookupItem ix state in
        let stateJson = Yojson.Basic.from_string extractedState in
        let sutJson = Yojson.Basic.from_string ("{\"id\": " ^ id ^ "}") in
        let combinedJson = Yojson.Basic.Util.combine stateJson sutJson in
        String.compare (Yojson.Basic.to_string content) (Yojson.Basic.to_string combinedJson) == 0
      else
        false
    | Create -> let code,content = Http.post (url^create) "{\"name\": \"bar\"}" in
      (* Get contents id and add it to sut *)
      let id = content |> member "id" |> to_int in 
      sut := !sut@[string_of_int id];
      true
    | Delete ix -> if (checkInvariant state sut) then (
        let id = lookupSutItem ix !sut in
        let code,content = Http.delete (delete_url ^ id) in
        if code == 200 then
          let pos = getPos ix !sut in
          sut := remove_item pos !sut;
          true;
        else 
          false
      )
      else
        false;
    | Put ix -> if (checkInvariant state sut) then (
        let id = lookupSutItem ix !sut in
        let newelem = "{\"name\": \"foo\"}" in
        let code,content = Http.put (put_url ^ id) newelem in 
        if code == 200 then (
          true;
        ) else
          false;
      ) else
        false

  let precond cmd state = match cmd with
    | Get ix -> List.length state > 0 
    | Delete ix-> List.length state > 0
    | Create -> true
    | Put ix -> List.length state > 0 
end

module APItest = QCSTM.Make(APIConf)
;; 

QCheck_runner.run_tests ~verbose:true
  [APItest.agree_test ~count:10 ~name:"Api Model agreement"]