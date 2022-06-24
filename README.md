Seum
=====

OCaml DSL and tool to write and compare programs in assembly, in a typed and "safe" fashion way. Why? Because I have "le seum" to write them.

## Features

Brainstorming for first release:

- Simple programs using mov, add, registers, stack ops. No pointer, dereferencing,
  data section.
- Encode the stack state in the type (instructions are encoded using a GADT with
  the stack state in the type)
- Cost model: encode the cost of an instruction (CPU cycle, number of registers used, etc)
- Compare programs based on a cost model
- Encode the type of values registers can have (EAX can use uint32, RAX uint64, etc)
- Instructions like `MOV` are used with registers of the same size
- Read an existing assembly program and verify properties
- Write assembly programs and dump them into a file
- VM to run an assembly program. It will consist of creating an inhabitant of
  the type, feeding the registers with the initial values

Future:
- Can check the program can be run on architecture X
- Detect bugs in assembly programs
