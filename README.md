# OCaml - Programming Paradigms (2024/2025)

This repository contains solutions to exercises in **OCaml**, completed as part of the *Programming Paradigms* course at **Università di Pisa** during the academic year 2024/2025. The exercises focus on functional programming concepts, recursion, pattern matching, higher-order functions, and other fundamental aspects of OCaml.

## Repository Structure

- **`exercises/`** - Contains OCaml scripts solving various exercises assigned during the course.
- **`interpreter/`** - A dedicated directory for running the OCaml custom interpreter.
- **`README.md`** - This file, providing an overview of the repository.

## Getting Started

### Installing OCaml

To run the exercises, you need **OCaml** installed on your system. The recommended way to install it is through [OPAM](https://opam.ocaml.org/), the OCaml package manager:

```sh
# Install OPAM (if not installed)
brew install opam  # macOS
sudo apt install opam  # Linux (Debian-based)
sudo dnf install opam  # Fedora
```

### Initialize OPAM
```sh
opam init
eval $(opam env)
```

### Install OCaml
```sh
opam install ocaml
```

## Running the Code
To execute an OCaml script, navigate to the **`exercises/`** directory and run:
```sh
ocaml exercise.ml
```

## About the Course
Programming Paradigms is a course at **Università di Pisa** focused on different programming paradigms, including **functional programming** using OCaml. The exercises in this repository explore key concepts such as:
- Recursion and Tail Recursion
- Pattern Matching
- Lists and Functional Data Structures
- Higher-Order Functions
- Type Inference and Polymorphism
This repository serves as a collection of solutions and notes to help reinforce these concepts.

## License
This repository is intended for educational purposes. You are free to use, modify, and share the solutions.


**Author:** [Lorenzo Libardi](mailto:lorenzo.libard@icloud.com)
