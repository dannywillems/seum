let () =
  let filename = Sys.argv.(1) in
  let prog = Nasm.read_file filename in
  Seum.print_prog prog
