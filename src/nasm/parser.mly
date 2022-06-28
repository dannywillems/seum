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
(* %token DOT *)
%token EOF
%token <string> ID
%token <string> LABEL
(* Not using it for the moment because the lexer cannot differentiate with the
   labels. A check must be done at runtime.
*)
(* %token <string> INSTRUCTION *)
(* Might require to be more specific depending on the allowed size *)
%token <int> INTEGER
%token <string> STRING
%token SECTION
%token GLOBAL
%token EXTERN
%token <string> INSTRUCTION
(* %token PERCENT *)
%token NEW_LINE
(* For operands *)
(* TODO: split register in 64BITS, 32BITS, etc *)
%token <string> REGISTER
%token <float> FLOAT

%start < Nasm_untyped_syntax.line > nasm_line

%%

nasm_line:
| EOF { raise End_of_file }
| EXTERN ;
  label = ID ;
  NEW_LINE {
      let ret = Nasm_untyped_syntax.Extern label in
      print_endline (Nasm_untyped_syntax.string_of_line ret);
      ret
    }
| GLOBAL ;
  label = ID ;
  NEW_LINE {
      let ret = Nasm_untyped_syntax.Global label in
      print_endline (Nasm_untyped_syntax.string_of_line ret);
      ret
    }
| SECTION ;
  section_name = ID ;
  NEW_LINE {
      let ret =
        if String.equal section_name ".bss" then Nasm_untyped_syntax.Section Nasm_untyped_syntax.Bss
        else if String.equal section_name ".data" then Nasm_untyped_syntax.Section Nasm_untyped_syntax.Data
        else if String.equal section_name ".text" then Nasm_untyped_syntax.Section Nasm_untyped_syntax.Text
        else raise (Invalid_argument (Printf.sprintf "%s is not a valid section name" section_name))
      in
      print_endline (Nasm_untyped_syntax.string_of_line ret);
      ret
    }
| label_opt = option(ID) ;
  instr = instruction ;
  ops = separated_list(COMMA, operand) ;
  NEW_LINE {
      let ret =
        if (Option.is_some label_opt)
        then Nasm_untyped_syntax.Labelled_instruction ((Option.get label_opt), instr, ops)
        else Nasm_untyped_syntax.Instruction (instr, ops)
      in
      print_endline (Nasm_untyped_syntax.string_of_line ret);
      ret

    }
| label = ID ;
  COLON ;
  option(NEW_LINE) ;
  instr = instruction ;
  ops = separated_list(COMMA, operand) ;
  NEW_LINE {
      let ret = Nasm_untyped_syntax.Labelled_instruction (label, instr, ops) in
      print_endline (Nasm_untyped_syntax.string_of_line ret);
      ret
    }

instruction:
| instr = INSTRUCTION { instr }

(* TODO: add hex integers *)
operand:
| reg_name = REGISTER { Nasm_untyped_syntax.Register reg_name }
| int = INTEGER { printf "Parser op/int: %d\n" int ; Nasm_untyped_syntax.Int int }
| f = FLOAT { Nasm_untyped_syntax.Float f }
(* | lbl = LABEL { lbl } *)
| op = ID { printf "Parser op/ID: %s\n" op; Nasm_untyped_syntax.String op }
| op = STRING { let op = Printf.sprintf "\"%s\"" op in Nasm_untyped_syntax.String op }