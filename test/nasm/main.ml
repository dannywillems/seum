let test_names =
  [ "simple";
    "simple_with_comments";
    "fibonacci";
    "multiple_lines";
    "simple_without_label" ]

let check_parser test_name () =
  let input_filename = "examples/" ^ test_name ^ ".input" in
  let output_filename = "examples/" ^ test_name ^ ".output" in
  let exp_output =
    let c = open_in output_filename in
    let rec aux acc = try aux (input_line c :: acc) with End_of_file -> acc in
    let res = aux [] in
    close_in c ;
    List.rev res
  in
  let output =
    let lines = Nasm.read_file input_filename in
    let lines = List.map Nasm.Untyped_syntax.string_of_line lines in
    lines
  in
  print_endline (String.concat "\n" output) ;
  assert (List.for_all2 String.equal exp_output output)

let () =
  let open Alcotest in
  run
    "NASM"
    [ ( "Check parser",
        List.map
          (fun test_name -> test_case test_name `Quick (check_parser test_name))
          test_names ) ]
