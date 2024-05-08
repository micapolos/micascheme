(import (micascheme) (minic lang))

(check (equal? (minic (u8 #x80)) #x80))
(check (equal? (minic (u8+1 (u8 #x80))) #x81))
(check (equal? (minic (u8+ (u8 #x80) (u8 #x90))) #x10))

(check (equal? (minic (u16 #x80)) #x80))
(check (equal? (minic (u16+1 (u16 #x80))) #x81))
(check (equal? (minic (u16+ (u16 #x80) (u16 #x90))) #x110))
