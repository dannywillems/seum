type symbol = string

type label = string

type instruction = string

type register = string

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
  | R r -> r
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
  | Register r -> r
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
