let test_case ~input ?name () =
  let test_name = match name with Some n -> n | None -> input in
  let test_fun () =
    let input = Cstruct.of_hex input in
    ignore @@ Any.decode_ber input;
    ignore @@ Any.decode_ber' input;
    ignore @@ Any.decode_ber'' input
  in
  (test_name, `Quick, test_fun)


let test_cases =
  [ test_case ~input:"030105" ()
  ; test_case ~input:"030101" ()
  ; test_case ~input:"030101010101010101010000040001010114" ()
  ; test_case ~input:"030101ff" ()
  ; test_case ~input:"03010300400000faff7b03030300" ()
  ; test_case ~input:"0301032100" ()
  ; test_case ~input:"030110" ()
  ; test_case ~input:"030116" ()
  ; test_case ~input:"03011f" ()
  ; test_case ~input:"03017503017f" ()
  ; test_case ~input:"030180" ()
  ; test_case ~input:"0301b8" ()
  ; test_case ~input:"03036161615f2161616161616161616161616108616161610100" ()
  ; test_case ~input:"03036a0303037f" ()
  ; test_case ~input:"030ed897b2bebebebebebe7fbebebebebebebebebebebebebebebebebebe" ()
  ; test_case ~input:"bebebebebebe869700" ()
  ]

let () = Alcotest.run "AFL fuzz test cases" [("", test_cases)]
