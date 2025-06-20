(library (asm-2 std)
  (export
    not
    = boolean=? char=? string=? type=?
    + - << >> and or xor
    string-append string-length bytevector
    binary-append db-binary dw-binary binary->bytevector)
  (import
    (asm-2 lang)
    (prefix (only (asm-2 typed) type=?) %)
    (prefix (asm-2 binary) %)
    (prefix (micascheme) %))

  (define not (typed (function (boolean) boolean) %not))

  (define = (typed (function (integer integer) boolean) %=))
  (define boolean=? (typed (function (boolean boolean) boolean) %boolean=?))
  (define char=? (typed (function (char char) boolean) %char=?))
  (define string=? (typed (function (string string) boolean) %string=?))
  (define type=? (typed (function (type type) boolean) %type=?))

  (define + (typed (function integer integer) %+))
  (define - (typed (function (integer . integer) integer) %-))
  (define << (typed (function (integer integer) integer) %bitwise-arithmetic-shift-left))
  (define >> (typed (function (integer integer) integer) %bitwise-arithmetic-shift-right))
  (define and (typed (function integer integer) %bitwise-and))
  (define or (typed (function integer integer) %bitwise-ior))
  (define xor (typed (function integer integer) %bitwise-xor))
  (define string-append (typed (function string string) %string-append))
  (define string-length (typed (function (string) integer) %string-length))

  (define bytevector (typed (function integer bytevector) %bytevector))

  (define binary-append (typed (function binary binary) %binary-append))
  (define db-binary (typed (function integer binary) %db-binary))
  (define dw-binary (typed (function integer binary) %dw-binary))
  (define binary->bytevector (typed (function (binary) bytevector) %binary->bytevector))
)
