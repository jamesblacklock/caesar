# Caesar
Compiler for the Caesar programming language

## What is the Caesar programming language?
Caesar is:
- a systems-level language

	- requires no runtime
	- allows straightforward manual memory management
	- links to native C libraries
	- can export native libraries through a C FFI

- an easy-to-use language

	- features a JIT-compiled REPL mode, suitable for use as a shell language
	- offers a modern and comfortable syntax similar to modern scripting languages
	- systems-level features are not needed for common application-level use cases
	- high-level features and structures are built into the language

- a safe language

	- features a memory-safe subset
	- features powerful analysis constructs for providing safety guarantees, but...
	- also provides an "unsafe" escape hatch for times when you just need to do something the compiler doesn't like

Under heavy development. Not yet ready for production use.

Currently the compiler generates assembler targeting 64-bit 
macOS and relies upon [NASM](https://www.nasm.us/) as a dependency.

"Hello world" example:

	@ffi("C") extern fn printf(fmt: &byte, ...) -> int
	
	fn main()
	    printf("Hello World!\n")

Building a program:

	./caesar.py input_file.csr --bin program_name
