# ECSL

> ECSL (Entity Component System Language)

A general purpose compiled programming language integrating elements from the
[ECS architectural pattern](https://en.wikipedia.org/wiki/Entity_component_system).

The language is compiled to bytecode and then executed in a virtual machine, exposing a generic ECS
runtime to the compiled program.

## Features

* Static Typing with basic type inference
* Structs, Enums
* Simple Build Tool
* Dependency Support
* Entitys and Components
* Queries
* Systems and Schedules

## Motivation

This language was built to explore integrating ECS patterns in to general purpose programming languages.

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

Note - the build.rs for bundle will copy the std library to `$HOME/.ecsl/ecsl_std`

## Overview

Brief language overview aiming to cover all relevant syntax

---
Add `comp` to the definition to make it a component
```
struct comp Health {
    val: int 
}

enum comp EnemyKind {
    Knight,
    Archer {
        range: float,
    }
}
```
---
Turn a function into a system by adding the `sys` keyword, allowing it access to ECS features
```
fn double(i: int) int {
    return i * 2;
}

sys create_archer() Entity {
    let e = Entity->new();
    e.insert(Health {
        val: 42,
    });
    e.insert(EnemyKind::Archer {
        range: 21.0,
    });
    return e;
}
```
---
Impl block for static functions and member functions.
Static functions are called using `Foo->bar()` syntax
```
impl Health {
    fn default() Health {
        return Health { val: 8 };
    }    

    fn take_damage(&self, damage: int) {
        self.val = self.val - damage;
    }
}
```
---
File path based imports
```
use std::vector::Vector3;
use lib_foo::foo::{Foo, Bar};
```
---
Entry point can be multiple types of function.

Plain is just a default entry point with no ECS features
Unscheduled allows ECS features but doesnt return a schedule.
Scheduled allows ECS features, once executes the schedule once and loop will indefinitely run until exit or panic

* Plain - `fn main() { .. }`
* Unscheduled - `sys main() { .. }`
* Scheduled Once - `sys main_once() Schedule { .. }`
* Scheduled Loop - `sys main_loop() Schedule { .. }`
---
```
let b = true; 
if (b) {
    print("True");
} else {
    print("False");
}

for (i in 0..=10) {
    print_int(i);
}

while (true) {
    print("Not really endless");
    break;
}

let opt = Option::<Int>::Some { val: 8 };
match (opt) {
    Some { val } -> {
      print_int(val);
    },
    _ -> {},
}
```
---
Create and destroy entitys
```
let e = Entity->new();
e.insert::<T>(..);

// Get Option<&T>
e.get::<T>().unwrap();

e.destroy();
```
---
Query for entitys with filters
```
let q = Query.with<Health, Poisoned>
             .without<Resistance>();
for (e in q) {
    ..
}
```
---
A Schedule is a user defined ordering of systems to execute
```
return Schedule [
    { foo, bar },
    last,
];
```

* Ordered Schedule - `[ .. ]`
* Unordered Schedule - `{ .. }`

## Drawbacks

Heap Allocation - No way of performing heap allocation, no dynamic data structures in the standard library

Parallelism - Systems can't be executed in parallel

FFI Support - No FFI support

VM Performance - VM is very unoptimized and developed in a way allowing flexibility over performance

Optimisations - Compiled bytecode is often severely unoptimized

Polymorphism - Lacking traits or other forms of compile time ad-hoc polymorphism

Visibility - No public/private visibility modifiers

Platform Compatibility - Untested on platforms other than x64 Linux NixOS
