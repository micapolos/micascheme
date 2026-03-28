# Leo Scheme Programming Language

Leo Scheme is a dialect of Scheme which uses indentation instead of parentheses.

## Quick Start

### Overview
* **Indentation:** Leo translates indentation into standard Scheme s-expressions.
* **Nesting:** Each indented block represents a new level of nesting.

### Hello, Leo!
```leo
display-line "Hello, Leo!"
```

### Checks
```leo
check equal?
  add: 2, 2
  4
```

### Quoting
```leo
check equal?
  'add: 2, 2
  'add: 2, 2

check equal?
  `result` add: 2, 2
  'result 4

check equal?
  `results`... list: 1, 2, 3, 4
  'results 1 2 3 4

check equal?
  `circle
    radius` sqrt 4
    center point
      x` add: 10, 20
      y` add: 30, 40
  'circle
    radius 2
    center point
      x 30
      y 70
```

### Definitions
```leo
define hello "Hello"
define leo "Leo"

define
  exclamated string
  string-append: string, "!"

define
  comma-separated: first-string, second-string
  string-append: first-string, ", ", second-string

check equal?
  exclamated comma-separated: hello, leo
  "Hello, Leo!"
```

### Local bindings
```leo
check equal?
  let
    hello "Hello"
    leo "Leo"
    in string-append: hello, ", ", leo, "!"
  "Hello, Leo!"

check equal?
  let*
    hello "Hello"
    leo "Leo"
    hello-leo string-append: hello, ", ", leo
    in string-append: hello-leo, "!"
  "Hello, Leo!"
```

### Control Flow
```leo
check equal?
  if
    greater?: 10, 5
    'ok
    'error
  'ok
```

### Lists
```leo
define numbers list: 1, 2, 3
define fruits list: 'apple, 'banana, 'orange
define various list: true, char a, and numbers

check equal?
  list: true, char a, and numbers
  list: true, char a, 1, 2, 3

check equal?
  append: numbers, fruits
  list: 1, 2, 3, 'apple, 'banana, 'orange

check equal?
  map: number->string, numbers
  list: "1", "2", "3"

check equal?
  filter: odd?, numbers
  list: 1, 3

check equal?
  fold-left: add, 0, numbers
  6
```

### Characters
```leo
char a
char z
char A
char Z
char 0
char 9
char space
char newline
char dot
char semicolon
char code 128512
char 😀
```

### Vectors
```leo
define my-vector vector: "foo", char a, 3.14

check equal?
  vector-length my-vector
  3

check equal?
  vector-ref: my-vector, 0
  "foo"

vector-set!: my-vector, 0, "bar"
check equal?
  vector-ref: my-vector, 0
  "bar"
```

### Bytevectors
```leo
define my-bytevector bytevector: 10, 20, 30, 40

check equal?
  bytevector-length my-bytevector
  4

check equal?
  bytevector-ref: my-bytevector, 1
  20

bytevector-set!: my-bytevector, 1, 50
check equal?
  bytevector-ref: my-bytevector, 1
  50
```

### Macros
```leo
define-syntax
  when
    magic s
    string-append: "Hello, ", s, "!"
  when
    magic: a, b
    add: a, b
  when
    magic: a, ...
    list: 'a, ...

check equal?
  magic "Leo"
  "Hello, Leo!"

check equal?
  magic: 10, 20
  30

check equal?
  magic: apple, orange, banana
  list: 'apple, 'orange, 'banana
```
