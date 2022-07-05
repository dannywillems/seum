let assert_length ops n = assert (List.length ops = n)

type symbol = string

type label = string

let label lbl = lbl

module Register = struct
  (* NB: if you add a register, add an alias at the top level and also in the
     module Address.t (suffixed with _ in the latter case) *)
  type t =
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
    | Rsp
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
  [@@deriving show { with_path = false }]

  let to_string x = show x |> String.lowercase_ascii

  let of_string x =
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
    | "rsp" -> Rsp
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
end

module Constant = struct
  type t =
    | Char of char
    | String of string
    | Float of float
    | Int of int
    | Hexadecimal of Hex.t
  [@@deriving variants]

  let to_string = function
    | Char c -> String.make 1 c
    | String s -> Printf.sprintf "\"%s\"" s
    | Float f -> string_of_float f
    | Hexadecimal h -> Printf.sprintf "%s" (Hex.show h)
    | Int i -> string_of_int i
end

module Address = struct
  type t =
    (* $ *)
    | Current
    (* Add *)
    | Add of t * t
    (* Sub *)
    | Sub of t * t
    (* eax * 2 *)
    | Times of t * t
    (* 2 *)
    | I of int
    (* Register *)
    | R of Register.t
    (* Any label *)
    | L of label

  let register r = R r

  let label lbl = L lbl

  let int i = I i

  module Infix = struct
    let ( + ) x y = Add (x, y)

    let ( * ) x y = Times (x, y)

    let ( - ) x y = Sub (x, y)

    let ( $ ) = Current
  end

  (* Alias to get a more friendly UI when writing programs in OCaml *)
  let eax_ = R Eax

  let ebx_ = R Ebx

  let ecx_ = R Ecx

  let edx_ = R Edx

  let esp_ = R Esp

  let ebp_ = R Ebp

  let esi_ = R Esi

  let edi_ = R Edi

  let rax_ = R Rax

  let rbx_ = R Rbx

  let rcx_ = R Rcx

  let rdx_ = R Rdx

  let rdi_ = R Rdi

  let rsi_ = R Rsi

  let rsp_ = R Rsp

  let r8_ = R R8

  let r9_ = R R9

  let r10_ = R R10

  let r11_ = R R11

  let r12_ = R R12

  let r13_ = R R13

  let r14_ = R R14

  let r15_ = R R15

  let ax_ = R Ax

  let bx_ = R Bx

  let cx_ = R Cx

  let dx_ = R Dx

  let sp_ = R Sp

  let bp_ = R Bp

  let si_ = R Si

  let di_ = R Di

  let ah_ = R Ah

  let al_ = R Al

  let bl_ = R Bl

  let bh_ = R Bh

  let ch_ = R Ch

  let cl_ = R Cl

  let dh_ = R Dh

  let dl_ = R Dl

  let spl_ = R Spl

  let bpl_ = R Bpl

  let rec to_string x =
    match x with
    | Current -> "$"
    | Add (x, y) -> Printf.sprintf "(%s + %s)" (to_string x) (to_string y)
    | Times (x, y) -> Printf.sprintf "(%s * %s)" (to_string x) (to_string y)
    | Sub (x, y) -> Printf.sprintf "(%s - %s)" (to_string x) (to_string y)
    | R r -> Register.to_string r
    | I i -> string_of_int i
    | L l -> l
end

module Section = struct
  type t = Bss | Data | Text

  let to_string = function Bss -> ".bss" | Data -> ".data" | Text -> ".text"
end

module PseudoInstruction = struct
  (* Pseudo instructions/operands. Important to split for the parser
     TODO: It is not complete. For instance, times is not supported.
  *)
  module Operand = struct
    type t = C of Constant.t | E of Address.t

    let of_address e = E e

    let char c = C (Constant.char c)

    let string c = C (Constant.string c)

    let float x = C (Constant.float x)

    let int x = C (Constant.int x)

    let hexadecimal x = C (Constant.hexadecimal x)

    let string_of_t = function
      | E e -> Address.to_string e
      | C c -> Constant.to_string c

    let string_of_ts ops = String.concat ", " (List.map string_of_t ops)
  end

  (* If you add a pseudo instruction, add an alias function to build the
     corresponding type pseudo_instr_ line at the bottom of this file *)
  type t =
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

  let of_string instr ops =
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
    | _ ->
        raise
          (Invalid_argument
             (Printf.sprintf "Unsupported pseudo instruction %s" instr))

  let to_string instr =
    match instr with
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
end

module Instruction = struct
  module Operand = struct
    type t =
      (* mov eax, [eax] *)
      | E of Address.t
      (* format db 'char' *)
      | C of Constant.t
      (* mov eax, eax *)
      | R of Register.t
      (* jmp l *)
      | L of label
    [@@deriving variants]

    let label lbl = L lbl

    let register r = R r

    let of_address e = E e

    let char c = C (Constant.char c)

    let string c = C (Constant.string c)

    let float x = C (Constant.float x)

    let int x = C (Constant.int x)

    let hexadecimal x = C (Constant.hexadecimal x)

    let ( ! ) x = E x

    (* Alias to get a more friendly UI when writing programs in OCaml *)
    let eax = R Eax

    let ebx = R Ebx

    let ecx = R Ecx

    let edx = R Edx

    let esp = R Esp

    let ebp = R Ebp

    let esi = R Esi

    let edi = R Edi

    let rax = R Rax

    let rbx = R Rbx

    let rcx = R Rcx

    let rdx = R Rdx

    let rdi = R Rdi

    let rsi = R Rsi

    let rsp = R Rsp

    let r8 = R R8

    let r9 = R R9

    let r10 = R R10

    let r11 = R R11

    let r12 = R R12

    let r13 = R R13

    let r14 = R R14

    let r15 = R R15

    let ax = R Ax

    let bx = R Bx

    let cx = R Cx

    let dx = R Dx

    let sp = R Sp

    let bp = R Bp

    let si = R Si

    let di = R Di

    let ah = R Ah

    let al = R Al

    let bl = R Bl

    let bh = R Bh

    let ch = R Ch

    let cl = R Cl

    let dh = R Dh

    let dl = R Dl

    let spl = R Spl

    let bpl = R Bpl

    let string_of_t = function
      | E e -> Printf.sprintf "[%s]" (Address.to_string e)
      | C c -> Constant.to_string c
      | R r -> Register.to_string r
      | L l -> l

    let string_of_ts ops = String.concat ", " (List.map string_of_t ops)
  end

  (* If you add an instruction, add an alias function to build the
     corresponding type instr_ line at the bottom of this file *)
  type t =
    | Mov of Operand.t * Operand.t
    | Cmovl of Operand.t * Operand.t
    | Add of Operand.t * Operand.t
    | Sub of Operand.t * Operand.t
    | Addc of Operand.t * Operand.t
    | Mulc of Operand.t * Operand.t
    | Dec of Operand.t
    | Inc of Operand.t
    | Xor of Operand.t * Operand.t
    | And of Operand.t * Operand.t
    | Mul of Operand.t * Operand.t
    | Cmp of Operand.t * Operand.t
    | Int of Operand.t
    (* | Raw of string * Operand.t list *)
    | Pop of Operand.t
    | Push of Operand.t
    | Ret
    | Jnz of label
    | Call of label
  [@@deriving variants]

  let of_string instr ops =
    let instr = String.lowercase_ascii instr in
    match instr with
    | "jnz" ->
        assert_length ops 1 ;
        let r1 = List.nth ops 0 in
        Jnz (Option.get @@ Operand.l_val r1)
    | "call" ->
        assert_length ops 1 ;
        let r1 = List.nth ops 0 in
        Call (Option.get @@ Operand.l_val r1)
    | "push" ->
        assert_length ops 1 ;
        let r1 = List.nth ops 0 in
        Push r1
    | "pop" ->
        assert_length ops 1 ;
        let r1 = List.nth ops 0 in
        Pop r1
    | "int" ->
        assert_length ops 1 ;
        let r1 = List.nth ops 0 in
        Int r1
    | "cmp" ->
        assert_length ops 2 ;
        let r1 = List.nth ops 0 in
        let r2 = List.nth ops 1 in
        Cmp (r1, r2)
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
    | "inc" ->
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
    | "sub" ->
        assert_length ops 2 ;
        let r1 = List.nth ops 0 in
        let r2 = List.nth ops 1 in
        Sub (r1, r2)
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

  let to_string = function
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
    | Sub (r1, r2) ->
        Printf.sprintf
          "sub %s, %s"
          (Operand.string_of_t r1)
          (Operand.string_of_t r2)
    | Ret -> "ret"
    | Cmp (r1, r2) ->
        Printf.sprintf
          "cmp %s, %s"
          (Operand.string_of_t r1)
          (Operand.string_of_t r2)
    | Int op -> Printf.sprintf "int %s" (Operand.string_of_t op)
    | Pop r -> Printf.sprintf "pop %s" (Operand.string_of_t r)
    | Push r -> Printf.sprintf "push %s" (Operand.string_of_t r)
end

module Directive = struct
  type t = string

  let to_string s = s

  let of_string s = s
end

(* Abstract line *)
type extern_

type global_

type section_

type instr_

type directive_

type pseudo_instr_

type linstr_

type 'a line =
  | Extern : symbol -> extern_ line
  | Global : label -> global_ line
  | Section : Section.t -> section_ line
  | Directive : Directive.t -> directive_ line
  | LInstr : label * instr_ line -> linstr_ line
  | PseudoInstr : label * PseudoInstruction.t -> pseudo_instr_ line
  | Instr : Instruction.t -> instr_ line

type e_line = L : 'a line -> e_line

type prog = e_line list

let string_of_line : type a. a line -> string = function
  | Extern s -> Printf.sprintf "  extern %s" s
  | Global s -> Printf.sprintf "  global %s" s
  | Section s -> Printf.sprintf "  section %s" (Section.to_string s)
  | Directive s -> Printf.sprintf "%s" (Directive.to_string s)
  | LInstr (lbl, Instr instr) ->
      Printf.sprintf "%s: %s" lbl (Instruction.to_string instr)
  | PseudoInstr (lbl, instr) ->
      Printf.sprintf "%s: %s" lbl (PseudoInstruction.to_string instr)
  | Instr instr -> Printf.sprintf "  %s" (Instruction.to_string instr)

let string_of_e_line (L l) = string_of_line l

let string_of_prog p = String.concat "\n" (List.map string_of_e_line p)

let print_prog prog = print_endline (string_of_prog prog)

(* Syntactic sugar for instruction *)
let mov op1 op2 = Instr (Instruction.mov op1 op2)

let cmovl op1 op2 = Instr (Instruction.cmovl op1 op2)

let add op1 op2 = Instr (Instruction.add op1 op2)

let sub op1 op2 = Instr (Instruction.sub op1 op2)

let addc op1 op2 = Instr (Instruction.addc op1 op2)

let mulc op1 op2 = Instr (Instruction.mulc op1 op2)

let dec op1 = Instr (Instruction.dec op1)

let inc op1 = Instr (Instruction.inc op1)

let xor op1 op2 = Instr (Instruction.xor op1 op2)

let and_ x y = Instr (Instruction.And (x, y))

let mul op1 op2 = Instr (Instruction.mul op1 op2)

let cmp op1 op2 = Instr (Instruction.cmp op1 op2)

let int i = Instr (Instruction.int (Instruction.Operand.int i))

let pop r = Instr (Instruction.pop r)

let push r = Instr (Instruction.push r)

let ret = Instr Instruction.ret

let jnz lbl = Instr (Instruction.jnz lbl)

let call lbl = Instr (Instruction.call lbl)

(* Pseudo instruction *)
let db lbl ops = PseudoInstr (lbl, PseudoInstruction.db ops)

let dw lbl ops = PseudoInstr (lbl, PseudoInstruction.dw ops)

let dd lbl ops = PseudoInstr (lbl, PseudoInstruction.dd ops)

let dq lbl ops = PseudoInstr (lbl, PseudoInstruction.dq ops)

let ddq lbl ops = PseudoInstr (lbl, PseudoInstruction.ddq ops)

let dt lbl ops = PseudoInstr (lbl, PseudoInstruction.dt ops)

let do_ lbl ops = PseudoInstr (lbl, PseudoInstruction.Do ops)

let resb lbl ops = PseudoInstr (lbl, PseudoInstruction.resb ops)

let resw lbl ops = PseudoInstr (lbl, PseudoInstruction.resw ops)

let resd lbl ops = PseudoInstr (lbl, PseudoInstruction.resd ops)

let resq lbl ops = PseudoInstr (lbl, PseudoInstruction.resq ops)

let rest lbl ops = PseudoInstr (lbl, PseudoInstruction.rest ops)

let reso lbl ops = PseudoInstr (lbl, PseudoInstruction.reso ops)

let resy lbl ops = PseudoInstr (lbl, PseudoInstruction.resy ops)

let incbin lbl ops = PseudoInstr (lbl, PseudoInstruction.incbin ops)

let equ lbl ops = PseudoInstr (lbl, PseudoInstruction.equ ops)

(* Syntactic sugar for programs *)

let ( |: ) s l = LInstr (s, l)

let ( ^> ) l p = L l :: p

let ( ^- ) l1 l2 = [L l1; L l2]

let global str = Global str

let extern str = Extern str

let to_line l = L l

let section_text = Section Text

let section_data = Section Data

let section_bss = Section Bss
