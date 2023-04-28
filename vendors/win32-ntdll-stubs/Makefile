BUILD_DIR = $(cur__target_dir)
SOURCE_DIR = $(cur__root)
PREFIX = $(cur__install)
PREFIX_LIB = $(cur__lib)
CC = x86_64-w64-mingw32-gcc

all:
	$(CC) -c lib.c -o $(BUILD_DIR)/lib.o
	ar -q $(BUILD_DIR)/ligoNtdll.a $(BUILD_DIR)/lib.o

install:
	install $(BUILD_DIR)/ligoNtdll.a $(PREFIX_LIB)
