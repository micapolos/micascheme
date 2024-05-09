(import (micascheme) (minic lang) (minic type))

(check (equal? (minic (type (i 8 #x80))) (int-type 8)))
(check (equal? (minic (i 8 #x80)) #x80))

(check (equal? (minic (type u8)) (function-type (list (int-type 8)) (int-type 8))))
(check (equal? (minic (type u8+1)) (function-type (list (int-type 8)) (int-type 8))))
(check (equal? (minic (type u8+)) (function-type (list (int-type 8) (int-type 8)) (int-type 8))))

(check (equal? (minic (type u16)) (function-type (list (int-type 16)) (int-type 16))))
(check (equal? (minic (type u16+1)) (function-type (list (int-type 16)) (int-type 16))))
(check (equal? (minic (type u16+)) (function-type (list (int-type 16) (int-type 16)) (int-type 16))))

(check (equal? (minic (type 123)) (syntax-type)))

(check (equal? (minic (u8 #x80)) #x80))
(check (equal? (minic (u8+1 #x80)) #x81))
(check (equal? (minic (u8+ #x80 #x90)) #x10))

(check (equal? (minic (u16 #x8010)) #x8010))
(check (equal? (minic (u16+1 #x80)) #x81))
(check (equal? (minic (u16+ #x80 #x90)) #x110))

(check (equal? (syntax->datum (minic 123)) 123))
