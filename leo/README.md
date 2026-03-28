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

display-line exclamated comma-separated: hello, leo
```

### Local bindings
```leo
display-line let
  hello "Hello"
  world "world"
  in string-append: hello, ", ", world, "!"

display-line let*
  hello "Hello"
  world "world"
  hello-world string-append: hello, ", ", world
  in string-append: hello-world, "!"
```

### Control Flow
```leo
if
  greater?: 10, 5
  display-line "10 is greater than 5"
  display-line "10 is not greater than 5"
```

### Lists
```leo
define numbers list: 1, 2, 3
define fruits list: 'apple, 'banana, 'orange
define various list: true, char a, and numbers

write `lists
  numbers` numbers
  fruits` fruits
  various` various
  numbers and fruits` append: numbers, fruits
  strings` map: number->string, numbers
  odd numbers` filter: odd?, numbers
  sum` fold-left: add, 0, numbers
```

### Characters
```leo
write list
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

write my-vector
write vector-length my-vector
write vector-ref: my-vector, 1

vector-set!: my-vector, 0, "bar"
write my-vector
```

### Bytevectors
```leo
define my-bytevector bytevector: 10, 20, 30, 40

write my-bytevector
write bytevector-length my-bytevector
write bytevector-ref: my-bytevector, 1

bytevector-set!: my-bytevector, 1, 50
write my-bytevector
```

### Quoting
```leo
write 'quoted add: 2, 2

write `quoted and unquoted` add: 2, 2

write `quoted and unquoted from list`... list: 1, 2, 3, 4

write `deeply quoted circle
  radius` sqrt 4
  center point
    x` sin 10
    y` cos 10
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

write magic "world"
write magic: 10, 20
write magic: apple, orange, banana
```
