# Caesar
Compiler for the Caesar programming language

## What is the Caesar programming language?
Caesar is:
- a systems-level language
- that allows straightforward manual memory management and provides a C FFI
- but also provides memory safety guarantees
- all while offering a modern and comfortable syntax

Currently the compiler generates assembler targeting 64-bit 
macOS and relies upon [NASM](https://www.nasm.us/) as a dependency.

"Hello world" example:

	@FFI("C") extern printf(fmt: Byte&, ...) -> Int32
	
	fn main()
	    printf("Hello World!\n")

Building a program:

	./caesar.py input_file.csr --bin program_name
