NASM parser/lexer
=====================

Following [this specification](https://www.csie.ntu.edu.tw/~comp03/nasm/nasmdoc3.html).

Features:
- [x] basic line format: `label: operands ; comments`
- [ ] dword ptr (ex: `mov eax, dword ptr x`)
- [ ] derefencement in instructions (ex: `mov eax, [ptr]`)
- [ ] Macros
- [ ] multi-line
