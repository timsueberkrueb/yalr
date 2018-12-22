# YALR

YALR (Yet Another LR parser generator) is a Rust library for generating `LALR(1)` parsers.

**IMPORTANT:** YALR is early work in progress and currently in an early prototypical state. Here be dragons.

## Goals

These are some general goals for the project with varying degrees of completion:

* **Panic-free**: The generated parser implementation should not panic
* **Fast**: The generated parser implementation should be reasonably fast
* **Straightforward syntax**: no custom grammar files, only Rust code and macros
* **Tooling**: CLI tooling should be provided for introspection (LALR parse table, LALR state diagram)
* **Lexer-independent**: You should be able use a generated YALR parser with any lexer that implements the `YALRLexer`
  trait. Support for popular lexers should be provided out-of-the-box using cargo features (e.g. `logos_support`)
* **Arbitrary inputs**: Since YALR is not bound to a specific lexer, you should be able to parse arbitrary inputs
* **Documentation**: This project should eventually be well-documented and include examples as well as a
  beginner-friendly guide
* **Tests**: This project should eventually have a good test coverage

## Build

Rust >= `1.31.0` is required to build YALR.

## Usage

Examples can be found in the [examples](examples) directory.

## License

YALR is licensed under either of the following licenses, at your option:

* [Apache License Version 2.0](LICENSE-APACHE)
* [MIT License](LICENSE-MIT)
