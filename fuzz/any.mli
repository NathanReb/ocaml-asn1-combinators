type ('bit_string, 'integer) t =
  | Null
  | Bool of bool
  | Integer of 'integer
  | Bit_string of 'bit_string
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
  | Sequence of ('bit_string, 'integer) t list
  | Set of ('bit_string, 'integer) t list
[@@deriving eq,show]

val grammar : (Cstruct.t, Z.t) t Asn.t
val grammar' : (bool array, int) t Asn.t
val grammar'' : (int list, Z.t) t Asn.t

val ber_codec : (Cstruct.t, Z.t) t Asn.codec
val ber_codec' : (bool array, int) t Asn.codec
val ber_codec'' : (int list, Z.t) t Asn.codec

val decode_ber : Cstruct.t -> ((Cstruct.t, Z.t) t, string) result
val decode_ber' : Cstruct.t -> ((bool array, int) t, string) result
val decode_ber'' : Cstruct.t -> ((int list, Z.t) t, string) result
