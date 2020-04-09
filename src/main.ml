open QCheck
open Yojson.Basic.Util
open Curl
open Format

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

  let put ?(header = "") url data =
    let pos = ref 0
    and len = String.length data in
    let rf cnt =
      let can_send = len - !pos in
      let to_send = if can_send > cnt then cnt else can_send in
      let r = String.sub data !pos to_send in
      pos := !pos + to_send; r 
    and r = Buffer.create 16384 in
    let c = Curl.init () in
    set_url c url;
    set_put c true;
    set_upload c true;
    set_readfunction c rf;
    set_httpheader c [header];
    set_writefunction c (fun s -> Buffer.add_string r s; String.length s);
    (*set_postfields c data;
      set_postfieldsize c (String.length data);*)
    perform c;
    let code = get_responsecode c in
    cleanup c;
    (code, Buffer.contents r)

  let patch ?(header = "") url data =
    let r = Buffer.create 16384 in
    let c = Curl.init () in
    set_customrequest c "PATCH";
    set_url c url;
    set_httpheader c [header];
    set_writefunction c (fun s -> Buffer.add_string r s; String.length s);
    perform c;
    let code = get_responsecode c in
    cleanup c;
    (code, Buffer.contents r)

  let delete ?(header = "") url =
    let r = Buffer.create 16384 in
    let c = Curl.init () in
    set_customrequest c "DELETE";
    set_url c url;
    set_httpheader c [header];
    set_writefunction c (fun s -> Buffer.add_string r s; String.length s);
    perform c;
    let code = get_responsecode c in
    cleanup c;
    (code, Buffer.contents r)
end

module Http =
struct
  (* Http Headers *)
  let get_header = "Content-Type:application/json; "
  let post_header = "Content-Type:application/json"
  let put_header = "Content-Type:application/json"
  let patch_header = "Content-Type:application/json"
  let delete_header = "Content-Type:application/json"

  let get ?(header = get_header) url =
    let c,r = InternalHttp.get ~header:header url in
    (c, Yojson.Basic.from_string r)
  let rawpost ?(header = post_header) url data =
    let c,r = InternalHttp.post ~header:header url data in
    (c,r)
  let post ?(header = post_header) url data =
    let c,r = InternalHttp.post ~header:header url data in
    (c, Yojson.Basic.from_string r)
  let put ?(header = put_header) url data =
    let c,r = InternalHttp.put ~header:header url data in
    (c, Yojson.Basic.from_string r)
  let patch ?(header = patch_header) url data =
    let c,r = InternalHttp.patch ~header:header url data in
    (c, Yojson.Basic.from_string r)
  let delete ?(header = delete_header) url =
    let c,r = InternalHttp.delete ~header:header url in
    (c, r)

end

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
        let pos = getPos ix !sut in
        let code,content = Http.delete (delete_url ^ id) in
        if code == 200 then (
          sut := remove_item pos !sut;
          true;
        )
        else 
          false
      )
      else
        false
    | Put ix -> if (checkInvariant state sut) then (
        let id = lookupSutItem ix !sut in
        let newelem = "{\"name\": \"foo\"}" in
        let code,content = Http.put (put_url ^ id) newelem in 
        if code == 200 then (
          true;
        ) else
          false
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