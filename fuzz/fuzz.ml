module D = struct
  module Z = struct
    type t = Z.t
    let equal = Z.equal
    let pp fmt t = Format.pp_print_string fmt (Z.to_string t)
  end
  module Cstruct = struct
    type t = Cstruct.t
    let equal = Cstruct.equal
    let pp fmt t = Format.pp_print_string fmt ([%show: string] (Cstruct.to_string t))
  end
end

let cstruct_from_file file =
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
  Cstruct.of_bigarray buffer

let () =
  let input = Sys.argv.(1) in
  let cs = cstruct_from_file input in
  let _ = Printf.printf "%s\n" ([%show: ((D.Cstruct.t, D.Z.t) Any.t, string) result] @@ Any.decode_ber cs) in
  let _ = Printf.printf "%s\n" ([%show: ((bool array, int) Any.t, string) result] @@ Any.decode_ber' cs) in
  let _ = Printf.printf "%s\n" ([%show: ((int list, D.Z.t) Any.t, string) result] @@ Any.decode_ber'' cs) in
  ()
