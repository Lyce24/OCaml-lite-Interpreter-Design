# OCaml-lite Interpreter

## Project Description

This repository contains the OCaml-lite interpreter, a project developed as part of a programming languages course. OCaml-lite is a subset of OCaml, designed to provide a comprehensive yet manageable environment for understanding the principles of language interpretation. This interpreter allows users to parse, type check, and execute OCaml-lite programs.

## Features

- **AST Generation:** Converts OCaml-lite code into an Abstract Syntax Tree (AST) for further processing.
- **Parsing:** Robust parsing capabilities to interpret OCaml-lite syntax. Using the Menhir parser generator, the parser is able to handle complex grammars.
- **Type Inference:** Implements a Hindley-Milner type system for type checking. The type inference algorithm including unification and generalization is implemented in OCaml.
- **Interpretation:** Executes OCaml-lite programs, handling both standard and user-defined types.
- **Error Handling:** Provides clear and informative error messages for syntax and type errors.
- **Test Suite:** Includes a comprehensive suite of tests ensuring interpreter correctness.

## Installation

To use the OCaml-lite interpreter, clone this repository to your local machine:

```bash
git clone https://github.com/Lyce24/OCaml-lite-Interpreter-Design
```

After cloning, navigate to the project directory and compile the project using Dune:

```bash
cd ocaml-lite-interpreter
dune build
```

## Usage

To run an OCaml-lite program, use the following command:

```bash
dune exec ocaml_lite -- [filename]
```

Replace `[filename]` with the path to your OCaml-lite file.

## License

This project is licensed under the MIT License - see the `LICENSE.md` file for details.

## Acknowledgments

- Special thanks to the course instructors Greg Anderson
- Contributions and feedback from fellow students
