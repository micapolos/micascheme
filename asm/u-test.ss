(import (micascheme) (asm u))

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

(check (s8? -128))
(check (s8? 127))
(check (not (s8? -129)))
(check (not (s8? 128)))

(check (equal? (u2 #b00) #b00))
(check (equal? (u2 #b11) #b11))
(check (raises (u2 #b100)))

(check (equal? (u2 #b00 #'x) #b00))
(check (equal? (u2 #b11 #'x) #b11))
(check (raises (u2 #b100 #'x)))

(check (equal? (u3 #b000) #b000))
(check (equal? (u3 #b111) #b111))
(check (raises (u3 #b1000)))

(check (equal? (u3 #b000 #'x) #b000))
(check (equal? (u3 #b111 #'x) #b111))
(check (raises (u3 #b1000 #'x)))

(check (equal? (u8 #x00) #x00))
(check (equal? (u8 #xff) #xff))
(check (raises (u8 #x100)))

(check (equal? (u8 #x00 #'x) #x00))
(check (equal? (u8 #xff #'x) #xff))
(check (raises (u8 #x100 #'x)))

(check (equal? (u16 #x0000) #x0000))
(check (equal? (u16 #xffff) #xffff))
(check (raises (u16 #x10000)))

(check (equal? (u16 #x0000 #'x) #x0000))
(check (equal? (u16 #xffff #'x) #xffff))
(check (raises (u16 #x10000 #'x)))

(check (equal? (s8 -128) -128))
(check (equal? (s8 127) 127))
(check (raises (s8 -129)))
(check (raises (s8 128)))

(check (equal? (s8 -128 #'x) -128))
(check (equal? (s8 127 #'x) 127))
(check (raises (s8 -129 #'x)))
(check (raises (s8 128 #'x)))
