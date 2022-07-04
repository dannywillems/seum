type symbol = string

type label

val label : string -> label

type register

val string_of_register : register -> string

val register_of_string : string -> register

type constant

val string_of_constant : constant -> string

module Address : sig
  type t

  val label : label -> t

  val register : register -> t

  val int : int -> t

  module Infix : sig
    val ( + ) : t -> t -> t

    val ( * ) : t -> t -> t

    val ( - ) : t -> t -> t

    val ( $ ) : t
  end

  val eax_ : t

  val ebx_ : t

  val ecx_ : t

  val edx_ : t

  val esp_ : t

  val ebp_ : t

  val esi_ : t

  val edi_ : t

  val rax_ : t

  val rbx_ : t

  val rcx_ : t

  val rdx_ : t

  val rdi_ : t

  val rsi_ : t

  val rsp_ : t

  val r8_ : t

  val r9_ : t

  val r10_ : t

  val r11_ : t

  val r12_ : t

  val r13_ : t

  val r14_ : t

  val r15_ : t

  val ax_ : t

  val bx_ : t

  val cx_ : t

  val dx_ : t

  val sp_ : t

  val bp_ : t

  val si_ : t

  val di_ : t

  val ah_ : t

  val al_ : t

  val bl_ : t

  val bh_ : t

  val ch_ : t

  val cl_ : t

  val dh_ : t

  val dl_ : t

  val spl_ : t

  val bpl_ : t
end

val string_of_address : Address.t -> string

module Operand : sig
  type t

  val char : char -> t

  val string : string -> t

  val float : float -> t

  val int : int -> t

  val hexadecimal : Hex.t -> t

  val of_address : Address.t -> t

  val label : label -> t

  (** Dereferencement *)
  val ( ! ) : Address.t -> t

  (** Use the corresponding register aliases below instead *)
  val register : register -> t

  (** Alias for the registers *)
  val eax : t

  val ebx : t

  val ecx : t

  val edx : t

  val esp : t

  val ebp : t

  val esi : t

  val edi : t

  val rax : t

  val rbx : t

  val rcx : t

  val rdx : t

  val rdi : t

  val rsi : t

  val rsp : t

  val r8 : t

  val r9 : t

  val r10 : t

  val r11 : t

  val r12 : t

  val r13 : t

  val r14 : t

  val r15 : t

  val ax : t

  val bx : t

  val cx : t

  val dx : t

  val sp : t

  val bp : t

  val si : t

  val di : t

  val ah : t

  val al : t

  val bl : t

  val bh : t

  val ch : t

  val cl : t

  val dh : t

  val dl : t

  val spl : t

  val bpl : t

  val string_of_t : t -> string

  val string_of_ts : t list -> string
end

type section

val string_of_section : section -> string

module PseudoOperand : sig
  type t

  val char : char -> t

  val string : string -> t

  val float : float -> t

  val int : int -> t

  val hexadecimal : Hex.t -> t

  val of_address : Address.t -> t

  val string_of_t : t -> string

  val string_of_ts : t list -> string
end

type pseudo_instr

val pseudo_instr_of_string : string -> PseudoOperand.t list -> pseudo_instr

val string_of_pseudo_instr : pseudo_instr -> string

type instr

val instr_of_string : string -> Operand.t list -> instr

val string_of_instr : instr -> string

(** Abstract lines *)

type extern_

type global_

type section_

type instr_

type pseudo_instr_

type linstr_

type 'a line =
  | Extern : symbol -> extern_ line
  | Global : label -> global_ line
  | Section : section -> section_ line
  | LInstr : label * instr_ line -> linstr_ line
  | PseudoInstr : label * pseudo_instr -> pseudo_instr_ line
  | Instr : instr -> instr_ line

type e_line = L : 'a line -> e_line

type prog

val string_of_e_line : e_line -> string

val string_of_prog : prog -> string

(** Instructions *)
val mov : Operand.t -> Operand.t -> instr_ line

val cmovl : Operand.t -> Operand.t -> instr_ line

val add : Operand.t -> Operand.t -> instr_ line

val sub : Operand.t -> Operand.t -> instr_ line

val addc : Operand.t -> Operand.t -> instr_ line

val mulc : Operand.t -> Operand.t -> instr_ line

val dec : Operand.t -> instr_ line

val inc : Operand.t -> instr_ line

val xor : Operand.t -> Operand.t -> instr_ line

val and_ : Operand.t -> Operand.t -> instr_ line

val mul : Operand.t -> Operand.t -> instr_ line

val cmp : Operand.t -> Operand.t -> instr_ line

val int : int -> instr_ line

val pop : Operand.t -> instr_ line

val push : Operand.t -> instr_ line

val ret : instr_ line

val jnz : label -> instr_ line

val call : label -> instr_ line

(** Pseudo instructions *)

val db : string -> PseudoOperand.t list -> pseudo_instr_ line

val dw : string -> PseudoOperand.t list -> pseudo_instr_ line

val dd : string -> PseudoOperand.t list -> pseudo_instr_ line

val dq : string -> PseudoOperand.t list -> pseudo_instr_ line

val ddq : string -> PseudoOperand.t list -> pseudo_instr_ line

val dt : string -> PseudoOperand.t list -> pseudo_instr_ line

val do_ : string -> PseudoOperand.t list -> pseudo_instr_ line

val resb : string -> PseudoOperand.t list -> pseudo_instr_ line

val resw : string -> PseudoOperand.t list -> pseudo_instr_ line

val resd : string -> PseudoOperand.t list -> pseudo_instr_ line

val resq : string -> PseudoOperand.t list -> pseudo_instr_ line

val rest : string -> PseudoOperand.t list -> pseudo_instr_ line

val reso : string -> PseudoOperand.t list -> pseudo_instr_ line

val resy : string -> PseudoOperand.t list -> pseudo_instr_ line

val incbin : string -> PseudoOperand.t list -> pseudo_instr_ line

val equ : string -> PseudoOperand.t list -> pseudo_instr_ line

(** Programs *)

val to_line : 'a line -> e_line

val global : label -> global_ line

val extern : label -> extern_ line

val section_text : section_ line

val section_bss : section_ line

val section_data : section_ line

(** Syntactic sugar for building programs *)

val ( |: ) : label -> instr_ line -> linstr_ line

val ( ^> ) : 'a line -> prog -> prog

val ( ^- ) : 'a line -> 'a line -> prog
