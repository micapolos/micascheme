(import (check) (zexy asm))

(define-syntax-rule (check-op $syntax ($u8s ...))
  (check
    (equal?
      (u8-list->bytevector
        (reverse
          (push-op (stack) #'$syntax)))
      (u8-list->bytevector
        (list $u8s ...)))))

(check-op (nop) (#x0))
(check-op (ret) (#xc9))

(check-op (ld b c) (#b01000001))
(check-op (ld d e) (#b01010011))
(check-op (ld h l) (#b01100101))
(check-op (ld a b) (#b01111000))

; ---------------------------------------------------

(define-syntax-rule (check-ops $syntax ... ($u8s ...))
  (check
    (equal?
      (u8-list->bytevector
        (reverse
          (push-ops (stack) (list #'$syntax ...))))
      (u8-list->bytevector
        (list $u8s ...)))))

(check-ops
  (nop)
  (ret)
  (#x0 #xc9))
