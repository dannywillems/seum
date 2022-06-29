(* name, type, value as string *)
type raw_data_section_content = string * string * string list

type raw_operand = string

type raw_instruction = string

type label = string

type section =
  | Data of raw_data_section_content
  | BSS
  | Text of label option * raw_instruction * raw_operand list

type 'a stack

type uint16 = Unsigned.UInt16.t

type uint32 = Unsigned.UInt32.t

type uint64 = Unsigned.UInt64.t

type 'a register =
  | EAX : uint32 -> uint32 register
  | EBX : uint32 -> uint32 register
  | ECX : uint32 -> uint32 register
  | EDX : uint32 -> uint32 register
  | RAX : uint64 -> uint64 register
  | RBX : uint64 -> uint64 register
  | RCX : uint64 -> uint64 register
  | RDX : uint64 -> uint64 register
  | R8 : uint64 -> uint64 register
  | R9 : uint64 -> uint64 register
  | R10 : uint64 -> uint64 register
  | R11 : uint64 -> uint64 register
  | R12 : uint64 -> uint64 register
  | R13 : uint64 -> uint64 register
  | R14 : uint64 -> uint64 register
  | R15 : uint64 -> uint64 register

(* type ('before_top, 'before, 'result_top, 'result) instr = *)
(* | POP : 'before register -> ('b, 's, 'r, 'f) instr ->  *)
(* | PUSH : 'before stack * 'after register * 'after stack -> ('after, 'before) instr *)

(* type 'a instr = *)
(*   | MOV : 'a register * 'a register *)
(*   | ADD : 'a register * 'a register *)
