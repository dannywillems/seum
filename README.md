Seum
=====

OCaml DSL and tool to write and compare programs in assembly, in a typed and
"safe" fashion way. Why? Because I have "le seum" to write them.


## Motivation

Writing performant applications might require to write low level assembly code
following a specific assembler syntax like GAS or NASM and when the code is
written, OCaml developers must write boilerplate code in C and in OCaml to bind
the hand-written assembly routines to expose it into a library.
Also, comparing two routines implementing the same algorithm in
assembly is hard because they might be implemented in different assembler, might
use different ISA or request access a different number of time the memory.

Seum aims to embed a syntax close to the NASM syntax in OCaml to write typed
assembly program. Parsers for different assemblers are provided to be able to
compare programs using the internal Seum representation. A cost model is
implemented counting the number of CPU cycle each instruction requires in
addition to counting the number of memory accesses and registers used, providing
more precise comparisons of algorithm implementations.
Additionnally, using the embedded syntax, OCaml bindings to the assembly code can be
automatically generated. Calling the assembly code directly from OCaml like any
other function should also be possible and straightforward. The OCaml developer
has never to write assembly in a different file and can threat assembly code
like pure OCaml functions.


## Example:

```ocaml
let print_prog : Seum.prog =
  let open Seum in
  let main = label "main" in
  let puts = label "puts" in
  let open Seum.Address in
  let open Seum.Instruction.Operand in
     global main
  ^> extern puts
  ^> section_text
  ^> (main |: push rdi)
  ^> push rsi
  ^> sub rsp (int 8)
  ^> mov rdi !(rsi_)
  ^> call puts
  ^> add rsp (int 8)
  ^> pop rsi
  ^> pop rdi
  ^> add rsi (int 8)
  ^> dec rdi
  ^> jnz main
  ^- ret
```

## Install

Parsers and internal tools are shipped into different packages. Each assembler
specific packages depend on `seum`.

For the internal tool:
```
opam install seum
```

For parsing NASM programs:
```
opam install nasm
```

## Features

Brainstorming for first release:

- [x] Simple programs using mov, add, registers, stack ops.
- [x] NASM parser for the simple programs.
- [x] Write assembly programs and dump them into a file
- [ ] Registers should not be simply a variant with only tags. We must be able
      to threat a type `Seum.prog` as a function, taking inputs. With this idea,
      we can test directly the code and threat the assembly as a normal OCaml
      function.
- [ ] Cost model: encode the cost of an instruction (CPU cycle, number of registers
  used, etc). Also, count the number of memory access.
- [ ] Compare programs based on a cost model
- [ ] Encode the stack state in the type (instructions are encoded using a GADT with
  the stack state in the type)
- [ ] Encode the type of values registers can have (EAX can use uint32, RAX uint64, etc)
- [ ] Instructions like `MOV` are used with registers of the same size.
- [ ] VM to run an assembly program. It will consist of creating an inhabitant of
  the type, feeding the registers with the initial values

Future:
- Check the program can be run on architecture X
- Detect bugs in assembly programs. This is not a precise task as "bug" must be
  specified
