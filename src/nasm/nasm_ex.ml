let () =
  let filename = Sys.argv.(1) in
  let lines = Nasm.read_file filename in
  let lines = List.map Nasm.Untyped_syntax.string_of_line lines in
  print_endline (String.concat "\n" lines)
