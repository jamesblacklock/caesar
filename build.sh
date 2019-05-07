
python3 test.py > test.asm && \
nasm -f macho64 test.asm -o test.o && \
ld -e _start -macosx_version_min 10.8 -arch x86_64 test.o -lc -lSystem -no_pie -o test

