open QCheck
open Yojson.Basic.Util
open Curl

module MyState = Map.Make(String)

module InternalHttp =
struct
  let get ?(header = "") url =
    let r = Buffer.create 16384 in
    let c = Curl.init () in
    set_url c url;
    set_httpheader c [header];
    set_writefunction c (fun s -> Buffer.add_string r s; String.length s);
    perform c;
    let code = get_responsecode c in
    cleanup c;
    (code, Buffer.contents r)

  let post ?(header = "") (* ?(content_type = "text/html") *) url data =
    let r = Buffer.create 16384 in
    let c = Curl.init () in
    set_url c url;
    set_post c true;
    set_httpheader c [header];
    set_writefunction c (fun s -> Buffer.add_string r s; String.length s);
    set_postfields c data;
    set_postfieldsize c (String.length data);
    perform c;
    let code = get_responsecode c in
    cleanup c;
    (code, Buffer.contents r)
end

module Http =
struct
  (* Http Headers *)
  let get_header = "Content-Type:application/json"
  let post_header = "Content-Type:application/json"
  
  let get ?(header = get_header) url =
    let c,r = InternalHttp.get ~header:header url in
    (c, Yojson.Basic.from_string r)
  let rawpost ?(header = post_header) url data =
    let c,r = InternalHttp.post ~header:header url data in
    (c,r)
  let post ?(header = post_header) url data =
    let c,r = InternalHttp.post ~header:header url data in
    (c, Yojson.Basic.from_string r)
end

module APIConf =
struct
  (* Types *)
  type sut = (string list) ref
  type state = string list
  type cmd =
    | Get of int
    | Create [@@deriving show { with_path = false }]

  (* Constants *)
  let url = "http://167.172.184.103"
  let get = "/api/shop/get/"
  let create = "/api/shop/create/item"
  let init_state = []
  let init_sut() = ref 0
  let cleanup _  = ignore(Http.rawpost (url ^ "/api/shop/reset") "")

  (* Functions *)
  (* Recursively drops n heads of a list and returns the rest of the list *)
  let rec drop n h = if n == 0 then h else (drop (n-1) (match h with
      | a::b -> b
      | [] -> []))

  (* basically this https://en.wikipedia.org/wiki/De_Bruijn_index *)
  let lookupItem ix state = List.hd (drop (ix mod List.length state) (List.rev state))
  
  let extract_id json =
    [json]
    |> filter_member "id"
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
        (Gen.oneof [(* Gen.return Create; *)
                    Gen.map (fun i -> Get i) int_gen])

  let next_state cmd state = match cmd with
    | Get ix -> state
    | Create -> state 
  
  (* 
  let addItemToState sutItem state = 
    state := state@[sutItem]
  *)

  let run_cmd cmd state sut = match cmd with
    | Get ix -> let id = lookupItem ix state in
                let code,content = Http.get (url ^ get ^ id) in
                String.compare (Yojson.Basic.to_string content) (lookupItem ix state) == 0
    | Create -> let code,content = Http.post (url^create) "{\"foo\": \"bar\"}" in
                (* Get contents id and add it to sut *)
                let id = List.hd(extract_id content);
                true

  let precond cmd state = match cmd with
    | Get ix -> List.length state > 0
    | Create -> true

end

module APItest = QCSTM.Make(APIConf)
;; 

QCheck_runner.run_tests ~verbose:true
  [APItest.agree_test ~count:1000 ~name:"Api Model agreement"]