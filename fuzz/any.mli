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
[@@deriving eq,show]

val grammar : t Asn.t

val ber_codec : t Asn.codec

val decode_ber : Cstruct.t -> (t, string) result
