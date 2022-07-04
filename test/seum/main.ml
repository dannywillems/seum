let check_dsl filename prog () =
  let output_filename = "res/" ^ filename ^ ".output" in
  let exp_output =
    let c = open_in output_filename in
    let rec aux acc = try aux (input_line c :: acc) with End_of_file -> acc in
    let res = aux [] in
    close_in c ;
    List.rev res
  in
  let output = List.map Seum.string_of_line prog in
  assert (List.for_all2 String.equal exp_output output)

[@@@ocamlformat "disable=true"]

let maxofthree_prog : Seum.prog =
  let open Seum in
  let open Seum.Operand in
  "maxofthree" |: mov rax rdi
   >>= mov rax rsi
   >=  cmovl rax rsi
   >=  cmp rax rdx
   >=  cmovl rax rdx
   >=  ret

[@@@ocamlformat "disable=false"]

let tests = [("maxofthree", maxofthree_prog)]

let () =
  let open Alcotest in
  run
    "NASM"
    [ ( "Check internal Seum representation",
        List.map
          (fun (filename, prog) ->
            test_case filename `Quick (check_dsl filename prog))
          tests ) ]
