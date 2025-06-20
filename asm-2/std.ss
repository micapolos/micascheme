(library (asm-2 std)
  (export
    not
    = boolean=? char=? string=? type=?
    + - << >> and or xor
    string-append string-length
    bytevector)
  (import (asm-2 lang))

  (define-primitives
    (not            (function (boolean) boolean))
    (=              (function (integer integer) boolean))
    (boolean=?      (function (boolean boolean) boolean))
    (char=?         (function (char char) boolean))
    (string=?       (function (string string) boolean))
    (type=?         (function (type type) boolean))
    (+              (function integer integer))
    (-              (function (integer . integer) integer))
    (<<             (function (integer integer) integer)      bitwise-arithmetic-shift-left)
    (>>             (function (integer integer) integer)      bitwise-arithmetic-shift-right)
    (and            (function integer integer)                bitwise-and)
    (or             (function integer integer)                bitwise-ior)
    (xor            (function integer integer)                bitwise-xor)
    (string-append  (function string string))
    (string-length  (function (string) integer))
    (bytevector     (function integer bytevector)))
)
