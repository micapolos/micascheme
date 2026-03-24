# Leo Scheme Programming Language

Leo Scheme is a dialect of Scheme which uses indentation instead of parentheses.

## Quick Start

### Overview
* **Indentation:** Leo translates indentation into standard Scheme s-expressions.
* **Nesting:** Each indented block represents a new level of nesting.
* **Quotes:** Text and code are separated using `<<` and `>>` quotes.
* **Examples:** `example` forms contain code snippets to execute.

### Basic Syntax
```leo
define hello "Hello"
define world "world"

write string-append: hello, ", ", world, "!"
```

### Functions
```leo
define
  exclamated s
  string-append: s, "!!!"

write exclamated "Cool"
write exclamated "LOL"
write exclamated "Wow"
```

### Control Flow
```leo
displayln
  if
    greater?: 10, 5
    "Obviously, 10 is greater than 5."
    "What? 10 is not greater than 5?"
```

### Lists and Maps
```leo
define numbers list: 1, 2, 3, 4, 5, 6

write numbers
write map: number->string, numbers
write filter: odd?, numbers
```

### Lambdas
```leo
write map
  lambda
    with x
    << mapping
      original >> x
      ncremented >> add: x, 1
      doubled >> multiply: x, 2
      to string >> number->string x
  list: 1, 2, 3, 4
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

write my-macro "world"
write my-macro: 10, 20
```
