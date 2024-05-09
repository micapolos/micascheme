(import (micascheme) (minic lang))

(check (equal? (minic (int 7 const #x7f)) #x7f))

(check (equal? (minic (int 7 inc (int 7 const #x70))) #x71))
(check (equal? (minic (int 7 inc (int 7 const #x7f))) #x00))

(check (equal? (minic (int 7 dec (int 7 const #x70))) #x6f))
(check (equal? (minic (int 7 dec (int 7 const #x00))) #x7f))

(check (equal? (minic (int 7 add (int 7 const #x70) (int 7 const #x0f))) #x7f))
(check (equal? (minic (int 7 add (int 7 const #x70) (int 7 const #x2f))) #x1f))

(check (equal? (minic (int 7 sub (int 7 const #x10) (int 7 const #x0f))) #x01))
(check (equal? (minic (int 7 sub (int 7 const #x10) (int 7 const #x2f))) #x61))
