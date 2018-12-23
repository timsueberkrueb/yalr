# YALR

YALR (Yet Another LR parser generator) is a Rust library for generating `LALR(1)` parsers.

**IMPORTANT:** YALR is early work in progress and currently in an early prototypical state. Here be dragons.

## Features

* **Straightforward syntax** - no custom grammar files, only Rust code and macros
* **Tooling included** - Parser introspection tooling included (LALR parse table, LALR state diagram)
* **Lexer-independent** - Works with any lexer that implements the `Lexer` trait

## Build

Rust >= `1.31.0` is required to build YALR.

## Usage

Examples can be found in the [examples](examples) directory.

## License

YALR is licensed under either of the following licenses, at your option:

* [Apache License Version 2.0](LICENSE-APACHE)
* [MIT License](LICENSE-MIT)
