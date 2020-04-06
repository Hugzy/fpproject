open Curl

module Http =
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
end