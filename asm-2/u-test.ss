(import (micascheme) (asm-2 u))

(check (u2? #b00))
(check (u2? #b11))
(check (not (u2? #b100)))

(check (u3? #b000))
(check (u3? #b111))
(check (not (u3? #b1000)))

(check (u8? #x00))
(check (u8? #xff))
(check (not (u8? #x100)))

(check (u16? #x0000))
(check (u16? #xffff))
(check (not (u16? #x10000)))

(check (equal? (u2 #b00) #b00))
(check (equal? (u2 #b11) #b11))
(check (raises (u2 #b100)))

(check (equal? (u3 #b000) #b000))
(check (equal? (u3 #b111) #b111))
(check (raises (u3 #b1000)))

(check (equal? (u8 #x00) #x00))
(check (equal? (u8 #xff) #xff))
(check (raises (u8 #x100)))

(check (equal? (u16 #x0000) #x0000))
(check (equal? (u16 #xffff) #xffff))
(check (raises (u16 #x10000)))
