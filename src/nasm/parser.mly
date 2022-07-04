(*
  Ignore:
  - macros
*)

%{
  let print_endline =
    if Option.is_some (Sys.getenv_opt "NASM_DEBUG") then print_endline else fun _s -> ()

  let printf =
    if Option.is_some (Sys.getenv_opt "NASM_DEBUG") then Printf.printf else fun _ _ -> ()
%}

%token COMMA
%token COLON
%token LEFT_PAREN
%token RIGHT_PAREN
%token PLUS
%token TIMES
%token MINUS
%token DOLLAR
%token OPENING_SQUARE_BRACKET
%token CLOSING_SQUARE_BRACKET
%token EOF
%token <string> ID
%token <string> SECTION_NAME
%token <string> LABEL
(* Might require to be more specific depending on the allowed size *)
%token <int> INTEGER
%token <string> STRING
%token SECTION
%token GLOBAL
%token EXTERN
%token <string> INSTRUCTION
%token <string> PSEUDO_INSTRUCTION
%token NEW_LINE

(* TODO: split register in 64BITS, 32BITS, etc *)
%token <string> REGISTER
%token <float> FLOAT
%token <string> HEX_STRING

%start < Seum.e_line > nasm_line

%%

nasm_line:
| EOF { raise End_of_file }
| EXTERN ;
  lbl = LABEL ;
  NEW_LINE {
      let ret = Seum.(L (extern (label lbl))) in
      print_endline (Seum.string_of_e_line ret);
      ret
    }
| GLOBAL ;
  lbl = LABEL ;
  NEW_LINE {
      let ret = Seum.(L (global (label lbl))) in
      print_endline (Seum.string_of_e_line ret);
      ret
    }
| SECTION ;
  section_name = SECTION_NAME ;
  NEW_LINE {
      let ret =
        if String.equal section_name ".bss"
        then Seum.(L section_bss)
        else if String.equal section_name ".data"
        then Seum.(L section_data)
        else if String.equal section_name ".text"
        then Seum.(L section_text)
        else raise (Invalid_argument (Printf.sprintf "%s is not a valid section name" section_name))
      in
      print_endline (Seum.string_of_e_line ret);
      ret
    }
| lbl_opt = option(LABEL) ;
  instr = instruction ;
  ops = separated_list(COMMA, operand) ;
  NEW_LINE {
      let instr = Seum.instr_of_string instr ops in
      let ret =
        if (Option.is_some lbl_opt)
        then Seum.(L (LInstr (label (Option.get lbl_opt), Instr instr)))
        else Seum.(L (Instr instr))
      in
      print_endline (Seum.string_of_e_line ret);
      ret
    }
| lbl = LABEL ;
  option(COLON) ;
  instr = pseudo_instruction ;
  ops = separated_list(COMMA, pseudo_operand) ;
  NEW_LINE {
      let instr = Seum.pseudo_instr_of_string instr ops in
      let ret =
        Seum.(L (PseudoInstr (label lbl, instr)))
      in
      print_endline (Seum.string_of_e_line ret);
      ret
    }
| lbl = LABEL ;
  COLON ;
  option(NEW_LINE) ;
  instr = instruction ;
  ops = separated_list(COMMA, operand) ;
  NEW_LINE {
      let instr = Seum.instr_of_string instr ops in
      let ret = Seum.(L (LInstr (label lbl, Instr instr))) in
      print_endline (Seum.string_of_e_line ret);
      ret
    }

instruction:
| instr = INSTRUCTION { instr }

address:
| add1 = address ;
  MINUS ;
  add2 = address {
             let ( - ) = Seum.Address.Infix.( - ) in
             add1 - add2
           }
| add1 = address ;
  TIMES ;
  add2 = address {
             let ( * ) = Seum.Address.Infix.( * ) in
             add1 * add2
           }
| add1 = address ;
  PLUS ;
  add2 = address {
             let ( + ) = Seum.Address.Infix.( + ) in
             add1 + add2
           }
| reg = REGISTER {
             Seum.Address.register (Seum.register_of_string reg)
           }
| lbl = LABEL {
          Seum.Address.label (Seum.label lbl)
        }
| DOLLAR {
  Seum.Address.Infix.($)
}

pseudo_instruction:
| instr = PSEUDO_INSTRUCTION { instr }

pseudo_operand:
| int = INTEGER {
            printf "Parser op/int: %d\n" int ; Seum.PseudoOperand.int int
          }
| f = FLOAT {
          printf "Parser op/float: %f\n" f ; Seum.PseudoOperand.float f
        }
| hex = HEX_STRING {
            printf "Parser op/hex: %s\n" hex ; Seum.PseudoOperand.hexadecimal (`Hex hex)
          }
| str = STRING {
            printf "Parser op/str: %s\n" str ; Seum.PseudoOperand.string str
          }
| addr = address {
      Seum.PseudoOperand.of_address addr
    }

operand:
| reg_name = REGISTER {
                  Seum.Operand.register (Seum.register_of_string reg_name) }
| int = INTEGER {
            printf "Parser op/int: %d\n" int ; Seum.Operand.int int
          }
| f = FLOAT {
            printf "Parser op/float: %f\n" f ; Seum.Operand.float f
          }
| hex = HEX_STRING {
            printf "Parser op/hex: %s\n" hex ; Seum.Operand.hexadecimal (`Hex hex)
          }
| str = STRING {
            printf "Parser op/str: %s\n" str ; Seum.Operand.string str
          }
| lbl = LABEL {
           printf "Parser op/label: %s\n" lbl ; Seum.Operand.label (Seum.label lbl)
         }
| OPENING_SQUARE_BRACKET ;
  addr = address ;
  CLOSING_SQUARE_BRACKET {
      Seum.Operand.of_address addr
    }
