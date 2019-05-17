# Caesar
Compiler for the Caesar programming language

## What is the Caesar programming language?
Caesar is:
- a systems-level language
- that allows straightforward manual memory management and provides a C FFI
- but also provides memory safety guarantees
- all while offering a modern and comfortable syntax

Currently the compiler generates assembler targeting 64-bit macOS.
The resulting assembler can be assembled using [NASM](https://www.nasm.us/).

	@FFI("C") extern printf(fmt: Byte^, ...)
	
	fn main()
	    printf("Hello World!\n")