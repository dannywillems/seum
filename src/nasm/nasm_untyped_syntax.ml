type symbol = string

type label = string

type instruction = string

type register = string

type operand =
  | Float of float
  | Int of int
  | Register of register
  | String of string
  | Hex of string

let string_of_operand = function
  | Float f -> string_of_float f
  | Int i -> string_of_int i
  | Register r -> r
  | String s -> s
  | Hex s -> s

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
