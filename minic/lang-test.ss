(import (micascheme) (minic lang))

(check (equal? (minic (int 8 const #x40)) #x40))

(check (equal? (minic (int 16 extend (int 8 const #x12))) #x12))
(check (equal? (minic (int 8 clamp (int 16 const #x1234))) #x34))

(check (equal? (minic (int 8 inc (int 8 const #xf0))) #xf1))
(check (equal? (minic (int 8 inc (int 8 const #xff))) #x00))

(check (equal? (minic (int 8 dec (int 8 const #x40))) #x3f))
(check (equal? (minic (int 8 dec (int 8 const #x00))) #xff))

(check (equal? (minic (int 8 add (int 8 const #x80) (int 8 const #x0f))) #x8f))
(check (equal? (minic (int 8 add (int 8 const #x80) (int 8 const #x98))) #x18))

(check (equal? (minic (int 8 sub (int 8 const #x10) (int 8 const #x0f))) #x01))
(check (equal? (minic (int 8 sub (int 8 const #x10) (int 8 const #x2f))) #xe1))
