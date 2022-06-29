type symbol = string

type label = string

type instruction = string

type register =
  | Eax
  | Ebx
  | Ecx
  | Edx
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

let string_of_register = function
  | Eax -> "eax"
  | Ebx -> "ebx"
  | Ecx -> "ecx"
  | Edx -> "edx"
  | Rax -> "rax"
  | Rbx -> "rbx"
  | Rcx -> "rcx"
  | Rdx -> "rdx"
  | Rdi -> "rdi"
  | Rsi -> "rsi"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

let register_of_string x =
  let x = String.lowercase_ascii x in
  match x with
  | "eax" -> Eax
  | "ebx" -> Ebx
  | "ecx" -> Ecx
  | "edx" -> Edx
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
  | _ -> raise (Invalid_argument (Printf.sprintf "Unknown register %s" x))

type address =
  | Current
  | Add of address * address
  | Sub of address * address
  | R of register
  | Raw of string
  | Label of label

let rec string_of_address = function
  | Current -> "$"
  | Add (x, y) ->
      Printf.sprintf "(%s + %s)" (string_of_address x) (string_of_address y)
  | Sub (x, y) ->
      Printf.sprintf "(%s - %s)" (string_of_address x) (string_of_address y)
  | R r -> string_of_register r
  | Raw a -> a
  | Label l -> l

type operand =
  | Float of float
  | Int of int
  | Register of register
  | String of string
  | Hex of string
  | Address of address

let string_of_operand = function
  | Float f -> string_of_float f
  | Int i -> string_of_int i
  | Register r -> string_of_register r
  | String s -> s
  | Hex s -> s
  | Address a -> string_of_address a

type section = Bss | Data | Text

let string_of_section = function
  | Bss -> ".bss"
  | Data -> ".data"
  | Text -> ".text"

type line =
  | Extern of symbol
  | Global of label
  | Section of section
  | Labelled_instruction of label * instruction * operand list
  | Instruction of instruction * operand list

let string_of_line = function
  | Extern s -> Printf.sprintf "  extern %s" s
  | Global s -> Printf.sprintf "  global %s" s
  | Section s -> Printf.sprintf "  section %s" (string_of_section s)
  | Labelled_instruction (lbl, instr, ops) ->
      Printf.sprintf
        "%s: %s %s"
        lbl
        instr
        (String.concat ", " (List.map string_of_operand ops))
  | Instruction (instr, ops) ->
      Printf.sprintf
        "  %s %s"
        instr
        (String.concat ", " (List.map string_of_operand ops))
