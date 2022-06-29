[@@@ocamlformat "disable=true"]

let maxofthree_prog : Seum.prog =
  let open Seum in
  let open Seum.Operand in
  ("maxofthree",
   mov (R Rax) (R Rdi)) >>=
   mov (R Rax) (R Rsi) >=
   cmovl (R Rax) (R Rsi) >=
   cmp Rax Rdx >=
   cmovl (R Rax) (R Rdx) >=
   ret

[@@@ocamlformat "disable=false"]

let () =
  print_endline
    (String.concat "\n" (List.map Seum.string_of_line maxofthree_prog))
