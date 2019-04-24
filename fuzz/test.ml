let test_case ~input ?name () =
  let test_name = match name with Some n -> n | None -> input in
  let test_fun () =
    let input = Cstruct.of_hex input in
    ignore @@ Any.decode_ber input
  in
  (test_name, `Quick, test_fun)


let test_cases =
  [ test_case ~input:"030105" () (* Works with [bit_string_cs] but fails with [bit_string] *)
  ]

let () = Alcotest.run "AFL fuzz test cases" [("", test_cases)]
