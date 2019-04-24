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
  Any.decode_ber cs

let () =
  let input = Sys.argv.(1) in
  match decode_file input with
  | Ok _
  | Error _ -> exit 0
