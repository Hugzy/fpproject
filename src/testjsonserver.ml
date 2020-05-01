open Yojson.Basic.Util
open Yojson

let json = Yojson.Basic.from_string "{\"id\": 2, \"todos\": \"foo\"}"

let extractIdFromJson (content:Yojson.Basic.t) : string =
    string_of_int (json |> member "id" |> to_int)
