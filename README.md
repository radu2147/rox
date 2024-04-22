# Rox

Rust implementation of Lox programming language interpreter from the book Crafting Interpreters. This repo also comes with a VSCode extension, that allows syntax highlighting specific to the language. Available features include:

- Control flow statements: `if`, `while` statements
- Functions: custom behaviour can be defined using `fun` keyword
- Objects: custom objects can be defined with classes
- Helpful error handling

To run, create a file with `.rox` extension and in terminal run the command:

    cargo run path/to/file

## Examples

```
class A {
    fun constructor(x){}

    fun test_a(q){
        return this.x + q;
    }
}

var a = A(4);
a = A(2);

print a.test_a(1);
```