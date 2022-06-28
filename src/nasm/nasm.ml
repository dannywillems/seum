module Untyped_syntax = Nasm_untyped_syntax

let read_file filename =
  let c = open_in filename in
  let lexbuf = Lexing.from_channel c in
  let rec read_lines acc =
    try
      let acc = Parser.nasm_line Lexer.prog lexbuf :: acc in
      read_lines acc
    with End_of_file -> acc
  in
  let res = read_lines [] in
  close_in c ;
  List.rev res
