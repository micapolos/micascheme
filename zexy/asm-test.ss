(import (check) (zexy asm))

(define-syntax-rule (check-op $syntax ($u8s ...))
  (check
    (equal?
      (asm-bytevector
        (asm-op (empty-asm) #'$syntax))
      (u8-list->bytevector
        (list $u8s ...)))))

(check-op (nop) (#x0))
(check-op (ret) (#xc9))

(check-op (ld b c) (#b01000001))
(check-op (ld d e) (#b01010011))
(check-op (ld h l) (#b01100101))
(check-op (ld a b) (#b01111000))

(check-op (ld b #x13) (#b00000110 #x13))
(check-op (ld l #x13) (#b00101110 #x13))
(check-op (ld a #x13) (#b00111110 #x13))

(check-op (ld b (hl)) (#b01000110))
(check-op (ld l (hl)) (#b01101110))
(check-op (ld a (hl)) (#b01111110))

(check-op (ld (hl) b) (#b01110000))
(check-op (ld (hl) l) (#b01110101))
(check-op (ld (hl) a) (#b01110111))

(check-op (ld (hl) #x13) (#b00110110 #x13))

; ---------------------------------------------------

(define-syntax-rule (check-ops $syntax ... ($u8s ...))
  (check
    (equal?
      (asm-bytevector
        (asm-ops (empty-asm) (list #'$syntax ...)))
      (u8-list->bytevector
        (list $u8s ...)))))

(check-ops
  (nop)
  (ret)
  (#x0 #xc9))
