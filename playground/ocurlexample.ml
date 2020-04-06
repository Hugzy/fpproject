open Curl
open Yojson
open Yojson.Basic.Util

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

let post ?(header = "") ?(content_type = "text/html") url data =
  let r = Buffer.create 16384 in
  let c = Curl.init () in
  set_url c url;
  set_post c true;
  set_httpheader c [ "Content-Type: " ^ content_type ];
  set_httpheader c [header];
  set_writefunction c (fun s -> Buffer.add_string r s; String.length s);
  set_postfields c data;
  set_postfieldsize c (String.length data);
  perform c;
  let code = get_responsecode c in
  cleanup c;
  (code, Buffer.contents r)
  
 let extract_id json =
    [json]
    |> filter_member "id"
    |> filter_string

let jsonget url = 
  let header = "Content-Type:application/json" in
  let c,r = get ~header:header url in
  (c, Yojson.Basic.from_string r)
let mpost url =
  let data = "{
    \"title\": \"foo\",
    \"body\": \"bar\",
    \"userId\": 1
  }" in
  let header = "Content-Type:application/json; charset=UTF-8" in
  let c,r = post ~header:header url data in
  (c, Yojson.Basic.from_string r)

let mget url =
  let header = "Content-Type:application/json" in
  let c,r = get ~header:header url in
  (c, Yojson.Basic.from_string r)

