# Leo Scheme Programming Language

Leo Scheme is a friendly, modern dialect of Scheme designed for humans who love the power of Scheme but prefer the clarity of a clean page.
By swapping traditional parentheses for a natural system of **indentation** and **punctuation**, Leo turns dense code into readable prose.

## Why choose Leo over standard Scheme?

* **Readability First:** Say goodbye to "parenthesis counting." Leo’s structure mirrors the way we naturally write and organize thoughts, making your logic stand out rather than the syntax.
* **Structured Depth:** While standard Scheme can become a "wall of brackets," Leo uses **indentation** to create a clear visual hierarchy, helping you navigate complex nesting at a glance.
* **Minimalist Aesthetic:** Leo removes the visual "noise," allowing you to focus on the "verbs" (functions) and "nouns" (values) of your program.
* **Fully Compatible:** Under the hood, Leo remains a true Scheme. You get all the power of first-class functions, recursion, and macros, just in a more elegant suit.

## Table of Contents

* [Getting Started](#getting-started)
* [Hello, Leo!](#hello-leo)
* [How the Syntax Works](#how-the-syntax-works)
* [Testing](#testing)
* [Definitions](#definitions)
* [Local Names](#local-names)
* [Lists](#lists)
* [Quoting](#quoting)
* [Control Flow](#control-flow)
* [Strings](#strings)
* [Characters](#characters)
* [Vectors](#vectors)
* [Bytevectors](#bytevectors)
* [Macros](#macros)

## Getting Started

Before writing your first program, you need to install Leo Scheme and learn how to run your code.

You can grab the latest macOS or Linux binaries directly from the [GitHub Releases](https://github.com/micapolos/micascheme/releases).

If you are on *macOS* and use *Homebrew*, you can install it by running:

```bash
brew install micapolos/leo/leo
```

### Running Leo

There are two primary ways to run Leo code: interactively via the REPL, or by executing a saved script file.

#### The REPL (Interactive Mode)

To experiment with Leo in real-time, you can start a Read-Eval-Print Loop (REPL). Simply type `leo` in your terminal without any arguments.

Once inside the REPL, you can type Leo expressions. Because Leo uses indentation to define blocks of code:
* **Pressing Enter once** allows you to continue typing multi-line indented blocks without executing them yet.
* **Pressing Enter twice** tells the REPL you are done with the expression and evaluates the code.

To exit the REPL, use `Ctrl+D`.

#### 2. Running an Input File

For larger projects, you will want to write your code in a text file (usually with a `.leo` extension) and run it all at once.

To run a script, pass the filename as an argument to the `leo` command:

```bash
leo script.leo
```

> **Note:** For a script file to be valid, it must begin with an `import` sentence to load the base language, such as `import leo scheme`.

## Hello, Leo!

Every journey starts with a friendly "Hello!".

```leo
write 'hello leo
```

### Explanation

The `write` function takes a **quoted** `hello leo` **sentence** and prints it to the screen.

> **Note**: Leo Scheme fully supports traditional strings using double quotes like "Hello, Leo!". However, because Leo is a language designed to easily read and manipulate its own code, we use quoting instead! This isn't just for typing text—it is a powerful feature that allows the language to treat code and data as the same thing, which we will dive into later.

## How the Syntax Works

Leo Scheme replaces traditional parentheses with a clean combination of **indentation**, **spaces**, **colons** and **commas**. This creates a syntax that follows the **natural rules of written language**—much like a book or an essay, Leo uses spaces to separate "words" and punctuation to structure "sentences."

* **Standard Spacing:** Just like in prose, use a space after a comma or colon.
* **Indentation:** To keep your structure clear, Leo uses exactly **2 spaces** per indentation level.
* **Clean Lines:** Leo doesn't allow spaces at the end of a line. It’s a good idea to configure your editor to "trim trailing whitespace" automatically!

Empty lines are perfectly fine—feel free to use them to breathe some space between paragraphs of your program.

### Structured Depth

While natural written language is great for simple descriptions, it struggles to handle deep "nesting" (sentences within sentences within sentences) without becoming a confusing mess.

Leo solves this by using **indentation**. By moving text two spaces to the right, you create a clear visual hierarchy that tells the computer exactly how deep a thought goes, keeping even the most complex structures easy to read.

### The "Tall" Way (Pure Indentation)

Here is a "sentence" describing a circle with a radius of *10* and a center point at *(20, 30)* using only indentation. We use `quote` to treat that sentence as data, and not as code to be executed. Notice how each new detail moves two spaces to the right, creating a clear visual hierarchy:

```leo
quote
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

### The "Medium" Way (Using Spaces)

If a sentence only has one sub-sentence, you can save vertical space by putting it on the same line with a single space:

```leo
quote circle
  radius 10
  center point
    x 20
    y 30
```

### The "Wide" Way (Colons and Commas)

When you have multiple sub-sentences you want to fit onto a single line, use a colon to start the list, and commas to separate them:

```leo
quote circle
  radius 10
  center point: x 20, y 30
```

### The "Ultra-Short" Way (The Quote Symbol)

If you want to make your code even more compact and readable, you can swap the word `quote` for a single quote `'` symbol.

Here is that same circle sentence written in its most concise form:

```leo
'circle
  radius 10
  center point: x 20, y 30
```

### The "Single Line" Way (Parentheses)

While Leo is designed to be parenthesized-free, you can still use them if you absolutely need to fit an entire sentence onto one line (like in a terminal command).

_Note: This isn't the recommended style for writing your `.leo` files, but it's there if you need it! Always remember to put a space before an opening parenthesis, and ensure there are no spaces on the inner sides of the parentheses (just as you would when writing regular text)._

```leo
'circle (radius 10, center point (x 20, y 30))
```

## Testing

Leo Scheme has a built-in way to test code using the `check` keyword.
In the example below, we are asking Leo to verify if the result of `add: 1, 2, 3, 4` is equal to `10`.
If the results match, the test passes and the program continues smoothly.
If they don't, the test fails and the program is interrupted—making it easy to catch bugs early!

```leo
check equal?
  add: 1, 2, 3, 4
  10
```

We will use this check pattern throughout the following sections to visualize the expected results of each example.

## Definitions

In Leo, we use `define` to give names to things. You can think of this as creating nouns (values) or verbs (functions) that you can reuse throughout your program.

### Values

The simplest use of define is to save a piece of prose or a number for later. This keeps your code clean and avoids repeating yourself.

```leo
define hello "Hello"
define leo "Leo"

define hello-leo string-append: hello, ", ", leo, "!"

check equal?
  hello-leo
  "Hello, Leo!"
```

### Functions

When you want to define a new action (a function), you use sub-sentences.
The first sub-sentence describes the "signature"—the name of the action and the inputs it needs.
The following sub-sentences are the "body"—the actual logic that runs.

In the example below, we define `exclamated` which takes a string and adds an exclamation mark to it. We also define `comma-separated` which takes two strings and joins them with a comma.

```leo
define lambda
  exclamated string
  string-append: string, "!"

define lambda
  comma-separated: first-string, second-string
  string-append: first-string, ", ", second-string

check equal?
  exclamated comma-separated: "Hello", "Leo"
  "Hello, Leo!"
```

## Local names

Sometimes you need to give a name to a piece of prose just for a single calculation. In Leo, we use the `let` word to create local names that exist only `in` specific block of code. Whether you need simple parallel bindings, step-by-step sequential or recursive evaluation, these constructs keep your definitions scoped exactly where you need them.

### Basic Bindings (`let`)

Use `let` when you have several independent names you want to define at once before using them in an expression (the `in` part).

```leo
check equal?
  let
    hello "Hello"
    leo "Leo"
    in string-append: hello, ", ", leo, "!"
  "Hello, Leo!"
```

### Sequential Bindings (`let sequential`)

If one of your local names depends on a name you defined just a line above it, use `let-sequential`. This tells Leo to define the names in order, allowing you to build complex sentences step-by-step.


```leo
check equal?
  let sequential
    hello "Hello"
    hello-leo string-append: hello, ", Leo"
    in string-append: hello-leo, "!"
  "Hello, Leo!"
```

### Recursive Bindings

TODO


## Lists

In Leo, a `list` is a collection of items ordered in a single sequence. Because Leo is designed to read like a language, working with lists feels like building a long sentence out of individual words or smaller phrases.

### Creating Lists
You can mix and match different types of data—numbers, sentences, strings, and even other lists—within a single collection.

```leo
define numbers list: 1, 2, 3, 4, 5

define fruits list
  'apple
  'banana
  'orange

define various list
  123
  character A
  "foo"
  true
```

### The and Keyword (Splicing)

A unique feature of Leo is the `and` keyword within a list. It doesn't just put a list inside another list; it "unpacks" or splices the items so they become part of the new list.

```leo
check equal?
  list: true, character A, and numbers
  list: true, character A, 1, 2, 3, 4, 5
```

### Transforming and Filtering

Leo provides powerful **verbs** to manipulate these lists:

* **`append`**: Joins any number of lists into a single list.
* **`map`**: Applies a transformation to every item in the list (e.g., turning numbers into strings).
* **`filter`**: Keeps only the items that meet a certain condition (e.g., only `odd?` numbers).
* **`fold-left`**: Combines all items in a list into a single value using a starting point (e.g., adding them all up starting from `0`).

```leo
check equal?
  append: numbers, fruits
  list: 1, 2, 3, 4, 5, 'apple, 'banana, 'orange

check equal?
  map: number->string, numbers
  list: "1", "2", "3", "4", "5"

check equal?
  filter: odd?, numbers
  list: 1, 3, 5

check equal?
  fold-left: add, 0, numbers
  15
```

## Quoting

In Scheme, "quoting" is how we tell the computer: "Don't run this code as a command; just treat it as a **sentence**." Leo offers three ways to do this, depending on how much of the sentence you want to "freeze."

### The Single Quote (`'`)

The single quote is the simplest way to quote. It marks the **entire sentence** including all its sub-sentences as literal prose.

In this example, we aren't adding *2* and *2*; we are simply checking if the sentence itself matches another one.

```leo
check equal?
  'add: 2, 2
  'add: 2, 2

check not equal?
  'add: 2, 2
  4
```

### The Backtick (`` ` ``)

Sometimes you only want to quote a specific word or part of a sentence while letting the rest run normally. A backtick appearing at the beginning of a word starts a quote and at the end of a word ends it.

Here, we quote `the result`, but we let the `add: 2, 2` part actually calculate the number *4*.

```leo
check equal?
  `number` add: 2, 2
  'number 4
```

### The Expansion (`` `... ``)

If you want to quote a word but treat everything that follows it as a list of items, use the expansion quote (`` `... ``). This "closes" the quote but tells Leo to expand the right side of the sentence into a list.

```leo
check equal?
  `numbers`... list: 1, 2, 3, 4
  'numbers 1 2 3 4
```

### Mixing Styles in Complex Sentences

You can combine these to handle complex sentences. In this example, we use backticks to quote words like `circle`, `radius`, `x`, and `y`, while allowing Leo to run functions like `sqrt` and `add` to fill in the actual values.

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

_For Scheme users: Leo's punctuation is a direct mapping of standard operators: `` ' `` acts as quote, while the backtick `` ` `` serves as a toggling `quasiquote` and `unquote`, and the ellipsis version `` `... `` functions as `unquote-splicing`._

## Control Flow

TODO

```leo
check equal?
  if
    greater?: 10, 5
    'that is what I expected
    'it does not seem right
  'that is what I expected
```

## Strings

TODO

## Characters

TODO

```leo
character a
character z
character A
character Z
character 0
character 9
character space
character newline
character dot
character semicolon
character code 128512
character 😀
```

## Vectors

TODO

```leo
define my-vector vector: "foo", character a, 3.14

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

## Bytevectors

TODO

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

## Macros

TODO

```leo
define syntax
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
