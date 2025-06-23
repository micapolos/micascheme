(library (asm std)
  (export
    not
    = boolean=? char=? string=? type=?
    + - >> << iand ior ixor
    string-append string-length
    string->utf8 char->integer
    make-bytevector bytevector bytevector-length)
  (import (asm lang))

  (define-primitives
    (not                 (function (boolean) boolean))
    (=                   (function (integer integer) boolean))
    (boolean=?           (function (boolean boolean) boolean))
    (char=?              (function (char char) boolean))
    (string=?            (function (string string) boolean))
    (type=?              (function (type type) boolean))
    (+                   (function integer integer))
    (-                   (function (integer . integer) integer))
    (<<                  (function (integer integer) integer)     bitwise-arithmetic-shift-left)
    (>>                  (function (integer integer) integer)     bitwise-arithmetic-shift-right)
    (iand                (function integer integer)               bitwise-and)
    (ior                 (function integer integer)               bitwise-ior)
    (ixor                (function integer integer)               bitwise-xor)
    (string-append       (function string string))
    (string-length       (function (string) integer))
    (string->utf8        (function (string) bytevector))
    (char->integer       (function (char) integer))
    (bytevector-length   (function (bytevector) integer))
    (bytevector          (function integer bytevector))
    (make-bytevector     (function (integer integer) bytevector)))
)
