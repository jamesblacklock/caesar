
./caesar.py test2.csr test2.asm && \
nasm -f macho64 test2.asm -o test2.o && \
ld -e _start -macosx_version_min 10.8 -arch x86_64 test2.o -lc -lSystem -no_pie -o test2

