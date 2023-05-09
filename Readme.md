Rust implmentation of an interpreter for the Monkey programming language.  Following the book [Writing an Interpreter in Go](https://interpreterbook.com/).

Below is an example of the Monkey Language:

```Rust
let myVar = 1
myVar;

let age = 1;
let name = "Monkey";
let result = 10 * (20 / 2);

let myArray = [1, 2, 3];

let twice = fn(f, x) {
  return f(f(x));
};

let addTwo = fn(x) {
  return x + 2;
};

twice(addTwo, 2); // => 6
```
