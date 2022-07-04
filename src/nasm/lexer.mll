(*
  Not supporting:
  - escaped "" in string
  - pseudo instructions (https://www.nasm.us/xdoc/2.11.08/html/nasmdoc3.html#section-3.2)
  - dword ptr
  - dereferencement [..]
  = ".. with pn"
  - the file must end with a new line
  - Instructions prefixes
    > An instruction is not required to use a prefix: prefixes such as CS, A32,
      LOCK or REPE can appear on a line by themselves, and NASM will just
      generate the prefix bytes.
  - segment register as prefixed
  - label having the same name than the registers. $eax is used
  - support register offset in operands
  - case sensitive
*)

{
exception IllegalCharacter of char

exception LabelTooLong of int

let print_endline =
  if Option.is_some (Sys.getenv_opt "NASM_DEBUG") then print_endline else fun _s -> ()

let printf =
  if Option.is_some (Sys.getenv_opt "NASM_DEBUG") then Printf.printf else fun _ _ -> ()

let string_of_char l = String.make 1 l

let max_label_length = 4095

let update_lexbuf_pos_line lexbuf =
  let n = String.length (Lexing.lexeme lexbuf) in
  print_endline "---------------";
  let pos = lexbuf.Lexing.lex_curr_p in
  let tmp = lexbuf.Lexing.lex_curr_p <-
      {
        pos with Lexing.pos_bol = lexbuf.Lexing.lex_curr_pos;
                 Lexing.pos_lnum = pos.Lexing.pos_lnum + n
      } in printf "line_num: %d\n" (pos.Lexing.pos_lnum); tmp

let currently_commenting = ref false

let currently_in_string = ref false

let current_string = ref []

let append_to_current_string s = current_string := s :: (!current_string)

let empty_line = ref true

let x86_64bits_register_names =
  let tmp = [
    "rax"; "rbx"; "rcx"; "rdx"; "rsp"; "rbp"; "rsi"; "rdi"; "r8"; "r9";
    "r10"; "r11"; "r12"; "r13"; "r14"; "r15"; "r8d"; "r9d"; "r10d"; "r11d";
    "r12d"; "r13d"; "r14d"; "r15d"; "r8w"; "r9w"; "r10w"; "r11w"; "r12w";
    "r13w"; "r14w"; "r15w"; "r8b"; "r9b"; "r10b"; "r11b"; "r12b"; "r13b";
    "r14b"; "r15b"; "spl"; "bpl"; "sil"; "dil"
  ] in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let x86_32bits_register_names =
  let tmp = ["eax"; "ebx"; "ecx"; "edx"; "esp"; "ebp"; "esi"; "edi"] in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let x86_16bits_register_names =
  let tmp = ["ax"; "bx"; "cx"; "dx"; "sp"; "bp"; "si"; "di"] in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let x86_8bits_register_names =
  let tmp = ["ah"; "al"; "bl"; "bh"; "ch"; "cl"; "dh"; "dl"; "spl"; "bpl"] in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let is_register x =
  let x = String.lowercase_ascii x in
  Hashtbl.mem x86_64bits_register_names x ||
  Hashtbl.mem x86_32bits_register_names x ||
  Hashtbl.mem x86_16bits_register_names x ||
  Hashtbl.mem x86_8bits_register_names x

let instructions_8086_8088 =
  let tmp = [
    "aaa"; "aad"; "aam"; "aas"; "adc"; "add"; "and"; "call"; "cbw"; "clc"; "cld";
    "cli"; "cmc"; "cmovl"; "cmp"; "cmpzz"; "cwd"; "dec"; "inc"; "int";
    "jnz"; "jne"; "je"; "jmp"; "mov"; "mul"; "pop"; "push"; "ret"; "xor"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

(* Added in CPU 80186/80188 *)
let instructions_80186_80188 =
  let tmp = [
    "bound"; "enter"; "insb"; "insw"; "leave"; "outsb"; "outsw"; "popa"; "pusha"; "pushw"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let instructions_80286 =
  let tmp = [
    "arpl"; "clts"; "lar"; "lgdt"; "lidt"; "lldt"; "lmsw"; "loadall";
    "lsl"; "ltr"; "sgdt"; "sidt"; "sldt"; "smsw"; "str"; "verr"; "verw"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let instructions_80386 =
  let tmp = [
    "bsf"; "bsr"; "bt"; "btc"; "btr"; "bts"; "cdq"; "cmpsd"; "cwde";
    "insd"; "iretd"; "iretdf"; "iretf"; "jecxz"; "lfs"; "lgs"; "lss";
    "lodsd"; "loopd"; "looped"; "loopned"; "loopnzd"; "loopzd"; "movsd";
    "movsx"; "movzx"; "outsd"; "popad"; "popfd"; "pushad"; "pushd";
    "pushfd"; "scasd"; "seta"; "setae"; "setb"; "setbe"; "setc"; "sete";
    "setg"; "setge"; "setl"; "setle"; "setna"; "setnae"; "setnb"; "setnbe";
    "setnc"; "setne"; "setng"; "setnge"; "setnl"; "setnle"; "setno";
    "setnp"; "setns"; "setnz"; "seto"; "setp"; "setpe"; "setpo"; "sets";
    "setz"; "shld"; "shrd"; "stosd"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let instructions_80486 =
  let tmp = [
    "bswap"; "cmpxchg"; "cpuid"; "invd"; "invlpg"; "rsm"; "wbinvd"; "xadd"
  ] in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let instructions_pentium =
  let tmp = ["cmpxchg8b"; "rdmsr"; "rdpmc"; "rdtsc"; "wrmsr"] in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let instructions_pentium_pro =
  let tmp = [
    "cmova"; "cmovae"; "cmovb"; "cmovb"; "cmove"; "cmovg"; "cmovge";
    "cmovl"; "cmovle"; "cmovna"; "cmovnae"; "cmovnb"; "cmovnbe"; "cmovnc";
    "cmovne"; "cmovng"; "cmovnge"; "cmovnl"; "cmovnle"; "cmovno"; "cmovnp";
    "cmovns"; "cmovnz"; "cmovo"; "cmovp"; "cmovpe"; "cmovpo"; "cmovs";
    "cmovz"; "sysenter"; "sysexit"; "ud2"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let instructions_pentium_4 =
  let tmp = [
    "maskmovq"; "movntps"; "movntq"; "prefetch0"; "prefetch1"; "prefetch2";
    "prefetchnta"; "sfence"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let instructions_pentium_4_sse3 =
  let tmp = [
    "clflush"; "lfence"; "maskmovdqu"; "mfence"; "movntdq"; "movnti"; "movntpd"; "pause"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let instructions_mmx =
  let tmp = [
    "emms"; "movd"; "movq"; "pabsb"; "pabsw"; "pabsd"; "packssdw";
    "packsswb"; "packuswb"; "paddb"; "paddd"; "paddsb"; "paddsw"; "paddusb";
    "paddusw"; "paddw"; "pand"; "pandn"; "pcmpeqb"; "pcmpeqd"; "pcmpeqw";
    "pcmpgtb"; "pcmpgtd"; "pcmpgtw"; "pmaddwd"; "pmulhw"; "pmullw"; "por";
    "pslld"; "psllq"; "psllw"; "psrad"; "psraw"; "psrld"; "psrlq"; "psrlw";
    "psubb"; "psubd"; "psubq"; "psubsb"; "psubsw"; "psubusb"; "psubusw";
    "psubw"; "punpckhbw"; "punpckhdq"; "punpckhwd"; "punpcklbw"; "punpckldq";
    "punpcklwd"; "pxor"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let instructions_ss2_simd_integers =
  let tmp = [
    "movdq2q"; "movdqa"; "movdqu"; "movq2dq"; "paddq"; "pmuludq"; "pshufhw";
    "pshuflw"; "pshufd"; "pslldq"; "psrldq"; "punpckhqdq"; "punpcklqdq"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let is_instruction x =
  let x = String.lowercase_ascii x in
  Hashtbl.mem instructions_8086_8088 x ||
  Hashtbl.mem instructions_80186_80188 x ||
  Hashtbl.mem instructions_80286 x ||
  Hashtbl.mem instructions_80386 x ||
  Hashtbl.mem instructions_80486 x ||
  Hashtbl.mem instructions_pentium x ||
  Hashtbl.mem instructions_pentium_4 x ||
  Hashtbl.mem instructions_pentium_pro x ||
  Hashtbl.mem instructions_pentium_4_sse3 x ||
  Hashtbl.mem instructions_ss2_simd_integers x ||
  Hashtbl.mem instructions_mmx x

let pseudo_instructions =
  let tmp = [
    "db"; "dw"; "dd"; "dq"; "ddq"; "dt"; "do";
    "resb"; "resw"; "resd"; "resq"; "rest"; "reso"; "resy";
    "incbin"; "equ"; "times"
  ]
  in
  Hashtbl.of_seq (List.to_seq (List.map (fun x -> x, x) tmp))

let is_pseudo_instruction x =
  let x = String.lowercase_ascii x in
  Hashtbl.mem pseudo_instructions x

}

let white = [' ' '\t' '\r']
let newline = ['\n']
let newlines = newline+

let dollar = '$'
let left_paren = '('
let right_paren = ')'
let plus = '+'
let minus = '-'
let alpha = ['a'-'z' 'A' - 'Z']
let alpha_capitalize = ['A' - 'Z']
let alpha_num = ['A' - 'Z' 'a' - 'z' '0' - '9']
let alpha_lowercase = ['a' - 'z']
let any_character = ['A' - 'Z' 'a' - 'z' '0' - '9' ' ' '\t' '\r' '%']
let integers = ['0' - '9']+
let floats =  ['-' '+']? [ '0' - '9' ]* ['.']? [ '0' - '9' ]*
let hex_integers = "0x" ['0' - '9' 'a' - 'f' 'A' - 'F']

(*
  https://www.nasm.us/xdoc/2.11.08/html/nasmdoc3.html
  > Valid characters in labels are letters, numbers, _, $, #, @, ~, ., and ?. The
  > only characters which may be used as the first character of an identifier are
  > letters, . (with special meaning: see section 3.9), _ and ?.
*)
let label_name = ['.' '_' '?']* ['A' - 'Z' 'a' - 'z' '0' - '9' '.' '_' '?' '$' '#' '@' '~']+

let section = "section" | "segment"
let extern = "extern"
let global = "global" | "globl"

let section_bss = ".bss"
let section_data = ".data"
let section_text = ".text"

(* Ignoring for the moment *)
let wrt = "WRT ..plt"


rule prog = parse
  | white { prog lexbuf }
  | ";" [^ '\n']* {
      prog lexbuf
    }
  | eof { Parser.EOF }
  | newlines {
      update_lexbuf_pos_line lexbuf;
      if !empty_line
      then prog lexbuf
      else (empty_line := true; Parser.NEW_LINE)
    }
  | wrt { prog lexbuf }
  | '"' | '\'' as opening_quote {
      empty_line := false;
      currently_in_string := true;
      string opening_quote lexbuf
    }
  | '[' {
    empty_line := false;
    Parser.OPENING_SQUARE_BRACKET
    }
  | ']' {
    empty_line := false;
    Parser.CLOSING_SQUARE_BRACKET
  }
  (* | "." { Parser.DOT } *)
  | ':' {
      empty_line := false;
      Parser.COLON
    }
  | minus {
      empty_line := false;
      Parser.MINUS
    }
  | plus {
    empty_line := false;
    Parser.PLUS
  }
  | left_paren {
    empty_line := false;
    Parser.LEFT_PAREN
  }
  | right_paren {
    empty_line := false;
    Parser.RIGHT_PAREN
  }
  | dollar {
    empty_line := false;
    Parser.DOLLAR
  }
  | "*" {
    print_endline "TIMES";
    empty_line := false;
    Parser.TIMES
  }
  | "," {
      print_endline "COMMA";
      empty_line := false;
      Parser.COMMA
    }
  (* Integers *)
  | integers as ident {
      empty_line := false;
      Parser.INTEGER (int_of_string ident)
    }
  (* Floats *)
  | floats as ident {
      empty_line := false;
      Parser.FLOAT (float_of_string ident)
    }
  | hex_integers as ident {
      empty_line := false;
      Parser.HEX_STRING ident
    }
  | extern {
    empty_line := false;
    print_endline "extern "; Parser.EXTERN
  }
  | global {
    empty_line := false;
    print_endline "Found global "; Parser.GLOBAL
  }
  | section {
    empty_line := false;
    print_endline "Found section "; Parser.SECTION
  }
  | ".bss" | ".data" | ".text" as ident {
    empty_line := false;
    Parser.SECTION_NAME ident
  }
  | label_name as ident {
      empty_line := false;
      if is_instruction ident then (Parser.INSTRUCTION ident)
      else if is_register ident then (Parser.REGISTER ident)
      else if is_pseudo_instruction ident then (Parser.PSEUDO_INSTRUCTION ident)
      (* TODO: add check on the valid syntax for labels *)
      else (
        let label_length = String.length ident in
        if label_length > max_label_length then raise (LabelTooLong label_length)
        else (Parser.LABEL ident)
      )
    }
  | _ as l {
      print_endline (string_of_char l);
      raise (IllegalCharacter l)
    }


and string opening_quote = parse
  | newline { failwith "Unterminated string" }
  | eof { failwith "Unterminated string" }
  | '"' | '\'' as l {
      (* If same than opening quote, we *)
      if Char.equal l opening_quote then (
        currently_in_string := false;
        let s = String.concat "" (List.rev (!current_string)) in
        current_string := [];
        print_endline s;
        Parser.STRING s
      ) else (
        append_to_current_string (string_of_char l);
        string opening_quote lexbuf
      )
    }
  | _  as l {
      append_to_current_string (string_of_char l);
      string opening_quote lexbuf
    }
