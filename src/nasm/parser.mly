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
  label = LABEL ;
  NEW_LINE {
      let ret = Seum.Extern label in
      print_endline (Seum.string_of_line ret);
      ret
    }
| GLOBAL ;
  label = LABEL ;
  NEW_LINE {
      let ret = Seum.Global label in
      print_endline (Seum.string_of_line ret);
      ret
    }
| SECTION ;
  section_name = SECTION_NAME ;
  NEW_LINE {
      let ret =
        if String.equal section_name ".bss"
        then Seum.Section Seum.Bss
        else if String.equal section_name ".data"
        then Seum.Section Seum.Data
        else if String.equal section_name ".text"
        then Seum.Section Seum.Text
        else raise (Invalid_argument (Printf.sprintf "%s is not a valid section name" section_name))
      in
      print_endline (Seum.string_of_line ret);
      ret
    }
| label_opt = option(LABEL) ;
  instr = instruction ;
  ops = separated_list(COMMA, operand) ;
  NEW_LINE {
      let instr = Seum.instr_of_string instr ops in
      let ret =
        if (Option.is_some label_opt)
        then Seum.LInstr ((Option.get label_opt), Seum.Instr instr)
        else Seum.Instr instr
      in
      print_endline (Seum.string_of_line ret);
      ret
    }
| label = LABEL ;
  option(COLON) ;
  instr = pseudo_instruction ;
  ops = separated_list(COMMA, pseudo_operand) ;
  NEW_LINE {
      let instr = Seum.pseudo_instr_of_string instr ops in
      let ret =
        Seum.PseudoInstr (label, instr)
      in
      print_endline (Seum.string_of_line ret);
      ret
    }
| label = LABEL ;
  COLON ;
  option(NEW_LINE) ;
  instr = instruction ;
  ops = separated_list(COMMA, operand) ;
  NEW_LINE {
      let instr = Seum.instr_of_string instr ops in
      let ret = Seum.LInstr (label, Seum.Instr instr) in
      print_endline (Seum.string_of_line ret);
      ret
    }

instruction:
| instr = INSTRUCTION { instr }

address:
| add1 = address ;
  MINUS ;
  add2 = address {
             Seum.Address.Sub (add1, add2)
           }
| add1 = address ;
  TIMES ;
  add2 = address {
             Seum.Address.Times (add1, add2)
           }
| add1 = address ;
  PLUS ;
  add2 = address {
             Seum.Address.Add (add1, add2)
           }
| reg = REGISTER {
             Seum.Address.R (Seum.register_of_string reg)
           }
| lbl = LABEL {
          Seum.Address.L lbl
        }
| DOLLAR {
  Seum.Address.Current
}

pseudo_instruction:
| instr = PSEUDO_INSTRUCTION { instr }

pseudo_operand:
| int = INTEGER {
            printf "Parser op/int: %d\n" int ; Seum.PseudoOperand.C (Seum.Int int)
          }
| f = FLOAT {
          printf "Parser op/float: %f\n" f ; Seum.PseudoOperand.C (Seum.Float f)
        }
| hex = HEX_STRING {
            printf "Parser op/hex: %s\n" hex ; Seum.PseudoOperand.C (Seum.Hexadecimal (`Hex hex))
          }
| str = STRING {
            printf "Parser op/str: %s\n" str ; Seum.PseudoOperand.C (Seum.String str)
          }
| addr = address {
      Seum.PseudoOperand.E addr
    }

operand:
| reg_name = REGISTER {
                  Seum.Operand.R (Seum.register_of_string reg_name) }
| int = INTEGER {
            printf "Parser op/int: %d\n" int ; Seum.Operand.C (Seum.Int int)
          }
| f = FLOAT {
            printf "Parser op/float: %f\n" f ; Seum.Operand.C (Seum.Float f)
          }
| hex = HEX_STRING {
            printf "Parser op/hex: %s\n" hex ; Seum.Operand.C (Seum.Hexadecimal (`Hex hex))
          }
| str = STRING {
            printf "Parser op/str: %s\n" str ; Seum.Operand.C (Seum.String str)
          }
| lbl = LABEL {
           printf "Parser op/label: %s\n" lbl ; Seum.Operand.L lbl
         }
| OPENING_SQUARE_BRACKET ;
  addr = address ;
  CLOSING_SQUARE_BRACKET {
      Seum.Operand.E addr
    }
