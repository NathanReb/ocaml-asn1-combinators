module Any = struct
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

  let choice_grammar =
    let open Asn.S in
    fix
      (fun self ->
         choice4
           (choice6 null bool integer bit_string octet_string oid)
           (choice6 generalized_time utc_time utf8_string numeric_string printable_string teletex_string)
           (choice6 videotex_string ia5_string graphic_string visible_string general_string universal_string)
           (choice4 bmp_string (enumerated (fun i -> i) (fun i -> i)) (sequence_of self) (set_of self)))

  let codec = Asn.(codec ber choice_grammar)

  let decode = Asn.decode codec
end

let decode_file file =
  let fd = Unix.(openfile file [O_RDONLY] 0o644) in 
  let gen_array =
    UnixLabels.map_file
      ~pos:0L
      ~kind:Bigarray.char
      ~layout:Bigarray.C_layout
      ~shared:false
      ~dims:[| -1 |]
      fd
  in
  let dims = Bigarray.Genarray.dims gen_array in
  let size = dims.(0) in
  let buffer = Bigarray.reshape_1 gen_array size in
  let cs = Cstruct.of_bigarray buffer in
  Any.decode cs

let () =
  let input = Sys.argv.(1) in
  match decode_file input with
  | Ok _
  | Error _ -> exit 0
