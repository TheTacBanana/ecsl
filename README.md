# ECSL

> ECSL (Entity Component System Language)

A general purpose compiled programming language integrating elements from the
[ECS architectural pattern](https://en.wikipedia.org/wiki/Entity_component_system).

The language is compiled to bytecode and then executed in a virtual machine, exposing a generic ECS
runtime to the compiled program.

## Features

* Static Typing
* Structs
* Tagged Union Enums
* Monads
* Simple Build Tool
* Dependency Support
* Entitys and Components
* Queries
* Systems and Schedules

## Motivation

I built this language to explore integrating ECS patterns in to general purpose programming languages.

Developed as part of my Bachelors Dissertation Project.

## Requirements

* Rust Nightly
* Zig 0.12

Compile both `bundle` and `ecsl_vm`
```
cargo build -r
zig build --release=fast
```

Create a new project with
```
bundle new hello_world
cd hello_world/
bundle run
```
