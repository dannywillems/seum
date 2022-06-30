let assert_length ops n = assert (List.length ops = n)

type symbol = string

type label = string

type register =
  | Eax
  | Ebx
  | Ecx
  | Edx
  | Esp
  | Ebp
  | Esi
  | Edi
  | Rax
  | Rbx
  | Rcx
  | Rdx
  | Rdi
  | Rsi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | Ax
  | Bx
  | Cx
  | Dx
  | Sp
  | Bp
  | Si
  | Di
  | Ah
  | Al
  | Bl
  | Bh
  | Ch
  | Cl
  | Dh
  | Dl
  | Spl
  | Bpl
[@@deriving show { with_path = false }, variants]

let string_of_register x = show_register x |> String.lowercase_ascii

let register_of_string x =
  let x = String.lowercase_ascii x in
  match x with
  | "eax" -> Eax
  | "ebx" -> Ebx
  | "ecx" -> Ecx
  | "edx" -> Edx
  | "esp" -> Esp
  | "ebp" -> Ebp
  | "esi" -> Esi
  | "edi" -> Edi
  | "rax" -> Rax
  | "rbx" -> Rbx
  | "rcx" -> Rcx
  | "rdx" -> Rdx
  | "rdi" -> Rdi
  | "rsi" -> Rsi
  | "r8" -> R8
  | "r9" -> R9
  | "r10" -> R10
  | "r11" -> R11
  | "r12" -> R12
  | "r13" -> R13
  | "r14" -> R14
  | "r15" -> R15
  | "ax" -> Ax
  | "bx" -> Bx
  | "cx" -> Cx
  | "dx" -> Dx
  | "sp" -> Sp
  | "bp" -> Bp
  | "si" -> Si
  | "di" -> Di
  | "ah" -> Ah
  | "al" -> Al
  | "bl" -> Bl
  | "bh" -> Bh
  | "ch" -> Ch
  | "cl" -> Cl
  | "dh" -> Dh
  | "dl" -> Dl
  | "spl" -> Spl
  | "bpl" -> Bpl
  | _ -> raise (Invalid_argument (Printf.sprintf "Unknown register %s" x))

module Address = struct
  type t =
    (* $ *)
    | Current
    (* Add *)
    | Add of t * t
    (* Sub *)
    | Sub of t * t
    (* Register *)
    | R of register
    (* Raw address as a string, hex string, etc *)
    | S of string
    (* Any label *)
    | L of label

  module Infix = struct
    let ( + ) x y = Add (x, y)

    let ( - ) x y = Sub (x, y)

    let ( $ ) = Current
  end
end

let rec string_of_address x =
  let open Address in
  match x with
  | Current -> "$"
  | Add (x, y) ->
      Printf.sprintf "(%s + %s)" (string_of_address x) (string_of_address y)
  | Sub (x, y) ->
      Printf.sprintf "(%s - %s)" (string_of_address x) (string_of_address y)
  | R r -> string_of_register r
  | S a -> a
  | L l -> l

module Operand = struct
  type t =
    | F of float
    | I of int
    | R of register
    | S of string
    | H of string
    | A of Address.t
  [@@deriving variants]

  let string_of_t = function
    | F f -> string_of_float f
    | I i -> string_of_int i
    | R r -> string_of_register r
    | S s -> s
    | H s -> s
    | A a -> string_of_address a

  let string_of_ts ops = String.concat ", " (List.map string_of_t ops)
end

type section = Bss | Data | Text

let string_of_section = function
  | Bss -> ".bss"
  | Data -> ".data"
  | Text -> ".text"

type instr =
  | Mov of Operand.t * Operand.t
  | Cmovl of Operand.t * Operand.t
  | Add of Operand.t * Operand.t
  | Addc of Operand.t * Operand.t
  | Mulc of Operand.t * Operand.t
  | Dec of Operand.t
  | Inc of Operand.t
  | Xor of Operand.t * Operand.t
  | And of Operand.t * Operand.t
  | Mul of Operand.t * Operand.t
  | Cmp of register * register
  | Int of Operand.t
  (* | Raw of string * Operand.t list *)
  | Pop of register
  | Push of register
  | Ret
  | Jnz of label
  | Call of label
  (* Pseudo instruction *)
  | Db of Operand.t list
  | Dw of Operand.t list
  | Dd of Operand.t list
  | Dq of Operand.t list
  | Ddq of Operand.t list
  | Dt of Operand.t list
  | Do of Operand.t list
  | Resb of Operand.t list
  | Resw of Operand.t list
  | Resd of Operand.t list
  | Resq of Operand.t list
  | Rest of Operand.t list
  | Reso of Operand.t list
  | Resy of Operand.t list
  | Incbin of Operand.t list
  | Equ of Operand.t list
[@@deriving variants]

let instr_of_string instr ops =
  let instr = String.lowercase_ascii instr in
  match instr with
  | "dw" -> Dw ops
  | "dd" -> Dd ops
  | "dq" -> Dq ops
  | "ddq" -> Ddq ops
  | "dt" -> Dt ops
  | "do" -> Do ops
  | "resb" -> Resb ops
  | "resw" -> Resw ops
  | "resd" -> Resd ops
  | "resq" -> Resq ops
  | "rest" -> Rest ops
  | "reso" -> Reso ops
  | "resy" -> Resy ops
  | "incbin" -> Incbin ops
  | "equ" -> Equ ops
  | "db" -> Db ops
  | "jnz" ->
      assert_length ops 1 ;
      let r1 = List.nth ops 0 in
      Jnz (Option.get @@ Operand.s_val r1)
  | "call" ->
      assert_length ops 1 ;
      let r1 = List.nth ops 0 in
      Call (Option.get @@ Operand.s_val r1)
  | "push" ->
      assert_length ops 1 ;
      let r1 = List.nth ops 0 in
      Push (Option.get @@ Operand.r_val r1)
  | "pop" ->
      assert_length ops 1 ;
      let r1 = List.nth ops 0 in
      Pop (Option.get @@ Operand.r_val r1)
  | "int" ->
      assert_length ops 1 ;
      let r1 = List.nth ops 0 in
      Int r1
  | "cmp" ->
      assert_length ops 2 ;
      let r1 = List.nth ops 0 in
      let r2 = List.nth ops 1 in
      Cmp (Option.get @@ Operand.r_val r1, Option.get @@ Operand.r_val r2)
  | "and" ->
      assert_length ops 2 ;
      let r1 = List.nth ops 0 in
      let r2 = List.nth ops 1 in
      And (r1, r2)
  | "xor" ->
      assert_length ops 2 ;
      let r1 = List.nth ops 0 in
      let r2 = List.nth ops 1 in
      Xor (r1, r2)
  | "Inc" ->
      assert_length ops 1 ;
      let r1 = List.nth ops 0 in
      Inc r1
  | "dec" ->
      assert_length ops 1 ;
      let r1 = List.nth ops 0 in
      Dec r1
  | "mulc" ->
      assert_length ops 2 ;
      let r1 = List.nth ops 0 in
      let r2 = List.nth ops 1 in
      Mulc (r1, r2)
  | "addc" ->
      assert_length ops 2 ;
      let r1 = List.nth ops 0 in
      let r2 = List.nth ops 1 in
      Addc (r1, r2)
  | "mul" ->
      assert_length ops 2 ;
      let r1 = List.nth ops 0 in
      let r2 = List.nth ops 1 in
      Mul (r1, r2)
  | "add" ->
      assert_length ops 2 ;
      let r1 = List.nth ops 0 in
      let r2 = List.nth ops 1 in
      Add (r1, r2)
  | "mov" ->
      assert_length ops 2 ;
      let r1 = List.nth ops 0 in
      let r2 = List.nth ops 1 in
      Mov (r1, r2)
  | "cmovl" ->
      assert_length ops 2 ;
      let r1 = List.nth ops 0 in
      let r2 = List.nth ops 1 in
      Cmovl (r1, r2)
  | "ret" ->
      assert_length ops 0 ;
      Ret
  | _ ->
      raise
        (Invalid_argument (Printf.sprintf "Unsupported instruction %s" instr))

let string_of_instr = function
  | Mov (r1, r2) ->
      Printf.sprintf
        "mov %s, %s"
        (Operand.string_of_t r1)
        (Operand.string_of_t r2)
  | Cmovl (r1, r2) ->
      Printf.sprintf
        "cmovl %s, %s"
        (Operand.string_of_t r1)
        (Operand.string_of_t r2)
  | Jnz lbl -> Printf.sprintf "jnz %s" lbl
  | Call lbl -> Printf.sprintf "call %s" lbl
  | Add (r1, r2) ->
      Printf.sprintf
        "add %s, %s"
        (Operand.string_of_t r1)
        (Operand.string_of_t r2)
  | Mulc (r1, r2) ->
      Printf.sprintf
        "mulc %s, %s"
        (Operand.string_of_t r1)
        (Operand.string_of_t r2)
  | Addc (r1, r2) ->
      Printf.sprintf
        "addc %s, %s"
        (Operand.string_of_t r1)
        (Operand.string_of_t r2)
  | Inc r1 -> Printf.sprintf "inc %s" (Operand.string_of_t r1)
  | Dec r1 -> Printf.sprintf "dec %s" (Operand.string_of_t r1)
  | And (r1, r2) ->
      Printf.sprintf
        "and %s, %s"
        (Operand.string_of_t r1)
        (Operand.string_of_t r2)
  | Xor (r1, r2) ->
      Printf.sprintf
        "xor %s, %s"
        (Operand.string_of_t r1)
        (Operand.string_of_t r2)
  | Mul (r1, r2) ->
      Printf.sprintf
        "mul %s, %s"
        (Operand.string_of_t r1)
        (Operand.string_of_t r2)
  | Ret -> "ret"
  | Cmp (r1, r2) ->
      Printf.sprintf
        "cmp %s, %s"
        (string_of_register r1)
        (string_of_register r2)
  | Int op -> Printf.sprintf "int %s" (Operand.string_of_t op)
  | Pop r -> Printf.sprintf "pop %s" (string_of_register r)
  | Push r -> Printf.sprintf "push %s" (string_of_register r)
  (* Pseudo instruction *)
  | Db ops -> Printf.sprintf "db %s" (Operand.string_of_ts ops)
  | Dw ops -> Printf.sprintf "dw %s" (Operand.string_of_ts ops)
  | Dd ops -> Printf.sprintf "dd %s" (Operand.string_of_ts ops)
  | Dq ops -> Printf.sprintf "dq %s" (Operand.string_of_ts ops)
  | Ddq ops -> Printf.sprintf "ddq %s" (Operand.string_of_ts ops)
  | Dt ops -> Printf.sprintf "dt %s" (Operand.string_of_ts ops)
  | Do ops -> Printf.sprintf "do %s" (Operand.string_of_ts ops)
  | Resb ops -> Printf.sprintf "resb %s" (Operand.string_of_ts ops)
  | Resw ops -> Printf.sprintf "resw %s" (Operand.string_of_ts ops)
  | Resd ops -> Printf.sprintf "resd %s" (Operand.string_of_ts ops)
  | Resq ops -> Printf.sprintf "resq %s" (Operand.string_of_ts ops)
  | Rest ops -> Printf.sprintf "rest %s" (Operand.string_of_ts ops)
  | Reso ops -> Printf.sprintf "reso %s" (Operand.string_of_ts ops)
  | Resy ops -> Printf.sprintf "resy %s" (Operand.string_of_ts ops)
  | Incbin ops -> Printf.sprintf "incbin %s" (Operand.string_of_ts ops)
  | Equ ops -> Printf.sprintf "equ %s" (Operand.string_of_ts ops)

type extern_

type global_

type section_

type linstr_

type instr_

type 'a line =
  | Extern : symbol -> extern_ line
  | Global : label -> global_ line
  | Section : section -> section_ line
  | LInstr : label * instr_ line -> linstr_ line
  | Instr : instr -> instr_ line

type e_line = L : 'a line -> e_line

type prog = e_line list

let mov a b = Instr (mov a b)

let cmovl a b = Instr (cmovl a b)

let cmp a b = Instr (cmp a b)

let ret = Instr ret

let rax = Operand.R rax

let rdi = Operand.R rdi

let rsi = Operand.R rsi

let rdx = Operand.R rdx

let string_of_line : type a. a line -> string = function
  | Extern s -> Printf.sprintf "  extern %s" s
  | Global s -> Printf.sprintf "  global %s" s
  | Section s -> Printf.sprintf "  section %s" (string_of_section s)
  | LInstr (lbl, Instr instr) ->
      Printf.sprintf "%s: %s" lbl (string_of_instr instr)
  | Instr instr -> Printf.sprintf "  %s" (string_of_instr instr)

let string_of_e_line : e_line -> string = fun (L l) -> string_of_line l
(*
let ( >>= ) (lbl, i1) i2 = [LInstr (lbl, i1); Instr i2]

let ( >= ) l1 i2 = List.concat [l1; [Instr i2]] *)

let ( |: ) s l = LInstr (s, l)

let ( ^> ) l p = L l :: p

let ( ^- ) l1 l2 = [L l1; L l2]