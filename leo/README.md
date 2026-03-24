# Leo Scheme Programming Language

Leo Scheme is a dialect of Scheme which uses indentation instead of parentheses.

## Quick Start

### Overview
* **Indentation:** Leo translates indentation into standard Scheme s-expressions.
* **Nesting:** Each indented block represents a new level of nesting.
* **Quotes:** Text and code are separated using `<<` and `>>` quotes.
* **Examples:** `example` forms contain code snippets to execute.

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
write
  if
    greater?: 10, 5
    quote 10 is greater than 5
    quote 10 is not greater than 5
```

### Lists
```leo
define numbers list: 1, 2, 3, 4, 5, 6

write numbers
write map: number->string, numbers
write filter: odd?, numbers
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
