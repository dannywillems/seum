[@@@ocamlformat "break-infix=fit-or-vertical"]

let maxofthree_prog : Seum.prog =
  let open Seum in
  let open Seum.Operand in
  ("maxofthree" |: mov rax rdi)
  ^> mov rax rsi
  ^> cmovl rax rsi
  ^> cmp rax rdx
  ^> cmovl rax rdx
  ^- ret

[@@@ocamlformat "break-infix=wrap"]

let () =
  print_endline
    (String.concat "\n" (List.map Seum.string_of_e_line maxofthree_prog))
