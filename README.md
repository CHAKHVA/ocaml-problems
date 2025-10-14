# 99 Problems in OCaml

A collection of [99 OCaml problems](https://ocaml.org/problems) - classic exercises to learn and practice functional programming in OCaml. These problems range from simple list operations to more complex algorithmic challenges.

## About

The "99 Problems" is a well-known collection of programming exercises originally created for Prolog and later adapted for various functional programming languages. This repository contains OCaml implementations of these problems, providing a practical way to learn OCaml's features and functional programming concepts.

## Project Structure

```
ocaml_problems/
├── bin/
│   └── main.ml          # Problem implementations and test cases
├── lib/                 # Library code (if needed)
├── test/                # Unit tests
├── dune-project         # Dune project configuration
└── README.md
```

## Requirements

- OCaml (>= 4.14)
- Dune (>= 3.16)
- opam (OCaml package manager)

## Installation

1. Clone this repository:

   ```bash
   git clone https://github.com/CHAKHVA/ocaml-problems.git
   cd ocaml_problems
   ```

2. Install dependencies:

   ```bash
   opam install . --deps-only
   ```

## Building

Build the project using Dune:

```bash
dune build
```

## Running

Execute the main program to see the implementations in action:

```bash
dune exec ocaml_problems
```

Run tests:

```bash
dune test
```

## Learning Resources

- [Official OCaml Website](https://ocaml.org/)
- [99 Problems on OCaml.org](https://ocaml.org/problems)
- [Real World OCaml](https://dev.realworldocaml.org/)
- [OCaml Manual](https://ocaml.org/manual/)
