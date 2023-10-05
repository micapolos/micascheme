(import (micascheme) (z80-parser))

(check (equal? (syntax->argument #`0) (byte #x00)))
(check (equal? (syntax->argument #`255) (byte #xFF)))
(check (equal? (syntax->argument #`-128) (byte #x80)))

(check (equal? (syntax->argument #`00h) (byte #x00)))
(check (equal? (syntax->argument #`ABh) (byte #xAB)))

(check (equal? (syntax->argument #`0000h) (word #x0000)))
(check (equal? (syntax->argument #`1234h) (word #x1234)))
(check (equal? (syntax->argument #`FFFFh) (word #xFFFF)))

(check (equal? (syntax->argument #`$foo) (label `foo)))

(check (equal? (syntax->argument #`A) (byte-register `A)))
(check (equal? (syntax->argument #`B) (byte-register `B)))
(check (equal? (syntax->argument #`C) (byte-register `C)))
(check (equal? (syntax->argument #`D) (byte-register `D)))
(check (equal? (syntax->argument #`E) (byte-register `E)))
(check (equal? (syntax->argument #`H) (byte-register `H)))
(check (equal? (syntax->argument #`L) (byte-register `L)))

(check (equal? (syntax->argument #`BC) (word-register `BC)))
(check (equal? (syntax->argument #`DE) (word-register `DE)))
(check (equal? (syntax->argument #`HL) (word-register `HL)))

(check
  (equal?
    (syntax->bytevector
      #`(
        (call $sub-routine)
        $sub-routine
        (ret)))
    #vu8(
      #xCD #x03 #x00
      #xC9)))
