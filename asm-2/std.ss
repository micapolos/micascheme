(library (asm-2 std)
  (export
    not
    = boolean=? char=? string=? type=?
    + - << >> and or xor
    string-append string-length bytevector)
  (import
    (asm-2 lang)
    (prefix (only (asm-2 typed) type=?) %)
    (prefix (asm-2 binary) %)
    (prefix (micascheme) %))

  (define not (typed (function (boolean) boolean) (%$primitive 3 not)))

  (define = (typed (function (integer integer) boolean) (%$primitive 3 =)))
  (define boolean=? (typed (function (boolean boolean) boolean) (%$primitive 3 boolean=?)))
  (define char=? (typed (function (char char) boolean) (%$primitive 3 char=?)))
  (define string=? (typed (function (string string) boolean) (%$primitive 3 string=?)))
  (define type=? (typed (function (type type) boolean) (%$primitive 3 type=?)))

  (define + (typed (function integer integer) (%$primitive 3 +)))
  (define - (typed (function (integer . integer) integer) (%$primitive 3 -)))
  (define << (typed (function (integer integer) integer) (%$primitive 3 bitwise-arithmetic-shift-left)))
  (define >> (typed (function (integer integer) integer) (%$primitive 3 bitwise-arithmetic-shift-right)))
  (define and (typed (function integer integer) (%$primitive 3 bitwise-and)))
  (define or (typed (function integer integer) (%$primitive 3 bitwise-ior)))
  (define xor (typed (function integer integer) (%$primitive 3 bitwise-xor)))
  (define string-append (typed (function string string) (%$primitive 3 string-append)))
  (define string-length (typed (function (string) integer) (%$primitive 3 string-length)))

  (define bytevector (typed (function integer bytevector) (%$primitive 3 bytevector)))
)
