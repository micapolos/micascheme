# Leo Scheme Programming Language

Leo Scheme is a dialect of Scheme which uses indentation instead of parentheses.

## Quick Start

### Hello, Leo!

Every programming journey starts with a friendly "Hello!".
In Leo Scheme, we use the `display-line` function to talk to the outside world.
It takes the text you give it and prints it to the screen, automatically adding a newline at the end so your terminal stays neat and organized for the next command.

```leo
display-line "Hello, Leo!"
```

### Syntax

Leo swaps traditional Scheme parentheses for a clean combination of **indentation**, **spaces**, **colons**, and **commas**.

One of the best ways to think about Leo is that it follows the **natural rules of written language**. Just like in a book or an essay, Leo uses spaces to separate "words" and punctuation to structure "sentences."

* **Standard Spacing:** Use a space after a comma and a space after a colon.
* **Indentation:** To keep things consistent, Leo uses exactly **2 spaces** per indentation level.
* **Clean Lines:** Leo doesn't allow spaces at the end of a line. It's a good idea to configure your editor to "trim trailing whitespace" automatically!

Empty lines are perfectly fine—feel free to use them to breathe some space between different logical blocks of your code.

#### The "Tall" Way (Pure Indentation)

Here is a "sentence" describing a circle with a radius of *10* and a center point at *(20, 30)* using only indentation. Notice how each new detail moves two spaces to the right, creating a clear hierarchy:

```leo
circle
  radius
    10
  center
    point
      x
        20
      y
        30
```

#### The "Medium" Way (Using Spaces)

If a sentence only has one sub-sentence, you can save vertical space by putting it on the same line with a single space:

```leo
circle
  radius 10
  center point
    x 20
    y 30
```

#### The "Wide" Way (Colons and Commas)

When you have multiple sub-sentences you want to fit onto a single line, use a colon followed by a space to start the list, and commas with spaces to separate them:

```leo
circle
  radius 10
  center point: x 20, y 30
```

#### The "Single Line" Way (Parentheses)

While Leo is designed to be parenthesized-free, you can still use them if you absolutely need to fit an entire sentence onto one line (like in a terminal command). Just remember to put a space before the opening parenthesis.

_Note: This isn't the recommended style for writing your .leo files, but it's there if you need it!_

```leo
circle (radius 10, center point (x 20, y 30))
```

### Testing

Leo Scheme has a built-in way to test code using the `check` keyword.
In the example below, we are asking Leo to verify if the result of `add: 2, 2` is equal to `4`.
If the results match, the test passes and the program continues smoothly.
If they don't, the test fails and the program is interrupted—making it easy to catch bugs early!

```leo
check equal?
  add: 2, 2
  4
```

We will use this check pattern throughout the following sections to visualize the expected results of each example.

### Quoting

In Scheme, "quoting" is how we tell the computer: "Don't run this code; just treat it as a piece of data." Leo offers three ways to do this, depending on how much of the "sentence" you want to freeze.

#### The Single Quote (')

The single quote is the simplest way to quote. It marks the entire sentence (and everything indented under it) as a literal piece of data.

In this example, we aren't adding 2 and 2; we are simply checking if the "sentence" itself matches another one.

```leo
check equal?
  'add: 2, 2
  'add: 2, 2
```

#### The Backtick (`)

Sometimes you only want to quote a specific word or part of a sentence while letting the rest run normally. A backtick appearing at the beginning of a word starts a quote and at the end of a word ends it.

Here, we quote the word `result`, but we let the `add: 2, 2` part actually calculate the number *4*.

```leo
check equal?
  `result` add: 2, 2
  'result 4
```

#### The Expansion (`...)

If you want to quote a label but treat everything that follows it as a list of items, use the expansion quote (`...). This "closes" the quote but tells Leo to expand the right side of the sentence into a list.

```leo
check equal?
  `results`... list: 1, 2, 3, 4
  'results 1 2 3 4
```

#### Mixing Styles in Complex Structures

You can combine these to handle complex data. In this circle example, we use backticks to quote words like `circle`, `radius`, `x`, and `y`, while allowing Leo to run functions like `sqrt` and `add` to fill in the actual values.

```leo
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

check equal?
  string-append: hello, ", ", leo, "!"
  "Hello, Leo!"
```

```leo
define
  exclamated string
  string-append: string, "!"

define
  comma-separated: first-string, second-string
  string-append: first-string, ", ", second-string

check equal?
  exclamated comma-separated: "Hello", "Leo"
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
```

```leo
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
