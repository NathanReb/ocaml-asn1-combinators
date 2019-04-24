type t =
  | Null
  | Bool of bool
  | Integer of Z.t
  | Bit_string of Cstruct.t
  | Octet_string of Cstruct.t
  | Oid of Asn.OID.t
  | Generalized_time of Ptime.t
  | Utc_time of Ptime.t
  | Utf8_string of string
  | Numeric_string of string
  | Printable_string of string
  | Teletex_string of string
  | Videotex_string of string
  | Ia5_string of string
  | Graphic_string of string
  | Visible_string of string
  | General_string of string
  | Universal_string of string
  | Bmp_string of string
  | Enumerated of int
  | Sequence of t list
  | Set of t list

let as_choice_grammar =
  let open Asn.S in
  fix
    (fun self ->
       choice4
         (choice6 null bool integer bit_string_cs octet_string oid)
         (choice6 generalized_time utc_time utf8_string numeric_string printable_string teletex_string)
         (choice6 videotex_string ia5_string graphic_string visible_string general_string universal_string)
         (choice4 bmp_string (enumerated (fun i -> i) (fun i -> i)) (sequence_of self) (set_of self)))

let rec from_choice = function
  | `C1 (`C1 ()) -> Null
  | `C1 (`C2 b) -> Bool b
  | `C1 (`C3 z) -> Integer z
  | `C1 (`C4 cs) -> Bit_string cs
  | `C1 (`C5 cs) -> Octet_string cs
  | `C1 (`C6 oid) -> Oid oid

  | `C2 (`C1 time) -> Generalized_time time
  | `C2 (`C2 time) -> Utc_time time
  | `C2 (`C3 str) -> Utf8_string str
  | `C2 (`C4 str) -> Numeric_string str
  | `C2 (`C5 str) -> Printable_string str
  | `C2 (`C6 str) -> Teletex_string str

  | `C3 (`C1 str) -> Videotex_string str
  | `C3 (`C2 str) -> Ia5_string str
  | `C3 (`C3 str) -> Graphic_string str
  | `C3 (`C4 str) -> Visible_string str
  | `C3 (`C5 str) -> General_string str
  | `C3 (`C6 str) -> Universal_string str

  | `C4 (`C1 str) -> Bmp_string str
  | `C4 (`C2 i) -> Enumerated i
  | `C4 (`C3 l) -> Sequence (List.map from_choice l)
  | `C4 (`C4 l) -> Set (List.map from_choice l)

let rec to_choice = function
  | Null -> `C1 (`C1 ())
  | Bool b -> `C1 (`C2 b)
  | Integer z -> `C1 (`C3 z)
  | Bit_string cs -> `C1 (`C4 cs)
  | Octet_string cs -> `C1 (`C5 cs)
  | Oid oid -> `C1 (`C6 oid)

  | Generalized_time time -> `C2 (`C1 time)
  | Utc_time time -> `C2 (`C2 time)
  | Utf8_string str -> `C2 (`C3 str)
  | Numeric_string str -> `C2 (`C4 str)
  | Printable_string str -> `C2 (`C5 str)
  | Teletex_string str -> `C2 (`C6 str)

  | Videotex_string str -> `C3 (`C1 str)
  | Ia5_string str -> `C3 (`C2 str)
  | Graphic_string str -> `C3 (`C3 str)
  | Visible_string str -> `C3 (`C4 str)
  | General_string str -> `C3 (`C5 str)
  | Universal_string str -> `C3 (`C6 str)

  | Bmp_string str -> `C4 (`C1 str)
  | Enumerated i -> `C4 (`C2 i)
  | Sequence l -> `C4 (`C3 (List.map to_choice l))
  | Set l -> `C4 (`C4 (List.map to_choice l))

let grammar = Asn.S.map from_choice to_choice as_choice_grammar

let decode_all ~codec ~name cs =
  match Asn.decode codec cs with
  | Ok (t, left) when Cstruct.len left = 0 -> Ok t
  | Ok _ -> Error (Printf.sprintf "%s: non empty leftover" name)
  | Error (`Parse s) -> Error s

let ber_codec = Asn.(codec ber grammar)

let decode_ber = decode_all ~codec:ber_codec ~name:"Any"
