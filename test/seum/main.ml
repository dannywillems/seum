[@@@ocamlformat "disable=true"]

let maxofthree_prog : Seum.prog =
  let open Seum in
  let open Seum.Operand in
  ("maxofthree",
   mov rax rdi) >>=
   mov rax rsi >=
   cmovl rax rsi >=
   cmp rax rdx >=
   cmovl rax rdx >=
   ret

[@@@ocamlformat "disable=false"]

let () =
  print_endline
    (String.concat "\n" (List.map Seum.string_of_line maxofthree_prog))
