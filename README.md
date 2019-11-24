# dogfood

Small pet project for implementing basic blocks of operating system without any dependency involved.
This includes compiler, C library, linker, kernel and so on. It's a work in progress.

Compiler is based on chibicc ([MIT](LICENSE.CHIBICC)). The rest of the code is [CC0](LICENSE).

## dfcc

C compiler which is able to compile all the code of dogfood project.
Implements a subset of C11 standard, simple C preprocessor, x86-64 codegen (in progress).

It's started as a fork of [chibicc](https://github.com/rui314/chibicc),
code generation was partly taken from [lacc](https://github.com/larmel/lacc).

## libdfc

C library which is enough for all the code of dogfood project.
Right now only declarations are provided.
