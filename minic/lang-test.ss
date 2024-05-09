(import (micascheme) (minic lang) (minic type))

(check (equal? (minic (type (u8 12))) (int-type 8)))
(check (equal? (minic (type u8+)) (function-type (list (int-type 8) (int-type 8)) (int-type 8))))

(check (equal? (minic (u8 #x80)) #x80))
(check (equal? (minic (u8+1 (u8 #x80))) #x81))
(check (equal? (minic (u8+ (u8 #x80) (u8 #x90))) #x10))

(check (equal? (minic (u16 #x80)) #x80))
(check (equal? (minic (u16+1 (u16 #x80))) #x81))
(check (equal? (minic (u16+ (u16 #x80) (u16 #x90))) #x110))
