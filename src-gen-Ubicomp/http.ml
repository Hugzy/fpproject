open Curl

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

