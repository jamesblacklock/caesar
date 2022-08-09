./caesar.py --run "tests/test.csr" && \
./caesar.py --sources "tests/test2.csr" --run 17 23 && \
./caesar.py --run "tests/test3.csr" && \
./caesar.py --run "tests/test4.csr" && \
./caesar.py --run "tests/test5.csr" && \
./caesar.py --link "tests/test_c_struct.o" --run "tests/test_union.csr" && \
./caesar.py --run "tests/test_owned.csr" && \
./caesar.py --run "tests/test_impl.csr" && \
./caesar.py --run "tests/test_trait.csr" && \
./caesar.py --run "tests/test_string.csr" && \
./caesar.py --run "tests/test6.csr" && \
./caesar.py --run "tests/hashmap_test.csr"