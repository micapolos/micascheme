# Leo Scheme Programming Language

Leo Scheme is a dialect of Scheme which uses indentation instead of parentheses.

## Quick Start

### Overview
* **Indentation:** Leo translates indentation into standard Scheme s-expressions.
* **Nesting:** Each indented block represents a new level of nesting.

### Hello, Leo!
```leo
displayln "Hello, Leo!"
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

displayln exclamated comma-separated: hello, leo
```

### Control Flow
```leo
if
  greater?: 10, 5
  displayln "10 is greater than 5"
  displayln "10 is not greater than 5"
```

### Lists
```leo
define numbers list: 1, 2, 3, 4, 5, 6

write numbers
write map: number->string, numbers
write filter: odd?, numbers
write fold-left: +, 0, numbers
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
  char integer 128512
  char 😀
```

### Vectors
```leo
define my-vector vector: "foo", #char a, 3.14

write my-vector
write vector-length my-vector
write vector-ref my-vector 1

vector-set! my-vector 0 "bar"
write my-vector
```

### Bytevectors
```leo
define my-bytevector bytevector: 10, 20, 30, 40

write my-bytevector
write bytevector-length my-bytevector
write bytevector-ref my-bytevector 1

bytevector-set! my-bytevector 1 50
write my-bytevector
```

### Quoting
```leo
write 'this is quoted'
write 'my lucky number is' string-length "bananas"
write 'circle
  radius' sqrt 4
  center point
    x' sin 10
    y' cos 10
```

### Macros
```leo
define-macro
  when
    my-macro s
    string-append: "Hello, ", s, "!"
  when
    my-macro: a, b
    add: a, b
  when
    my-macro: a, ...
    list: quote a, ...

write my-macro "world"
write my-macro: 10, 20
write my-macro: apple, orange, banana
```
