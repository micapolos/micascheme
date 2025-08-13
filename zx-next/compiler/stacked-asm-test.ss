(import (asm z80) (prefix (zx-next compiler stacked) %) (zx-next compiler stacked-asm))

; top-level
(check-stacked-asm
  (%stacked (%op 0 (nop)))
  (asm () (nop)))

; op 0
(check-stacked-asm
  (%stacked () (%op 0 (nop)))
  (asm () (nop)))

(check-stacked-asm
  (%stacked (a) (%op 0 (nop)))
  (asm (a) (nop)))

; op 1
(check-stacked-asm
  (%stacked () (%op 1 (ld a #x12)))
  (asm (a) (ld a #x12)))

(check-stacked-asm
  (%stacked (a) (%op 1 (ld a #x12)))
  (asm (a l) (ld l a) (ld a #x12)))

(check-stacked-asm
  (%stacked (a de) (%op 1 (ld a #x12)))
  (asm (a l) (push de) (ld l a) (ld a #x12)))

(check-stacked-asm
  (%stacked (hl) (%op 1 (ld a #x12)))
  (asm (a de) (ex de hl) (ld a #x12)))

(check-stacked-asm
  (%stacked (hl de) (%op 1 (ld a #x12)))
  (asm (a de) (push de) (ex de hl) (ld a #x12)))

(check-stacked-asm
  (%stacked (lde) (%op 1 (ld a #x12)))
  (asm (a ) (push lde) (ld a #x12)))

(check-stacked-asm
  (%stacked (hlde) (%op 1 (ld a #x12)))
  (asm (a ) (push hlde) (ld a #x12)))

; op 2
(check-stacked-asm
  (%stacked () (%op 2 (ld hl #x1234)))
  (asm (hl) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (a) (%op 2 (ld hl #x1234)))
  (asm (hl) (push a) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (a l) (%op 2 (ld hl #x1234)))
  (asm (hl) (ld h a) (push hl) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (a de) (%op 2 (ld hl #x1234)))
  (asm (hl) (push de) (push a) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (hl) (%op 2 (ld hl #x1234)))
  (asm (hl de) (ex de hl) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (hl de) (%op 2 (ld hl #x1234)))
  (asm (hl de) (push de) (ex de hl) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (lde) (%op 2 (ld hl #x1234)))
  (asm (hl) (push lde) (ld hl #x1234)))

(check-stacked-asm
  (%stacked (hlde) (%op 2 (ld hl #x1234)))
  (asm (hl) (push hlde) (ld hl #x1234)))

; op 3
(check-stacked-asm
  (%stacked () (%op 3 (ld lde #x123456)))
  (asm (lde) (ld lde #x123456)))

(check-stacked-asm
  (%stacked (a) (%op 3 (ld lde #x123456)))
  (asm (lde) (push a) (ld lde #x123456)))

(check-stacked-asm
  (%stacked (a l) (%op 3 (ld lde #x123456)))
  (asm (lde) (ld h a) (push hl) (ld lde #x123456)))

(check-stacked-asm
  (%stacked (hl) (%op 3 (ld lde #x123456)))
  (asm (lde) (push hl) (ld lde #x123456)))

(check-stacked-asm
  (%stacked (hl de) (%op 3 (ld lde #x123456)))
  (asm (lde) (push de) (push hl) (ld lde #x123456)))

(check-stacked-asm
  (%stacked (lde) (%op 3 (ld lde #x123456)))
  (asm (lde) (push lde) (ld lde #x123456)))

(check-stacked-asm
  (%stacked (hlde) (%op 3 (ld lde #x123456)))
  (asm (hlde) (push hlde) (ld lde #x123456)))

; op 4
(check-stacked-asm
  (%stacked () (%op 4 (ld hlde #x12345678)))
  (asm (hlde) (ld hlde #x12345678)))

(check-stacked-asm
  (%stacked (a) (%op 4 (ld hlde #x12345678)))
  (asm (hlde) (push a) (ld hlde #x12345678)))

(check-stacked-asm
  (%stacked (a l) (%op 4 (ld hlde #x12345678)))
  (asm (hlde) (ld h a) (push hl) (ld hlde #x12345678)))

(check-stacked-asm
  (%stacked (hl) (%op 4 (ld hlde #x12345678)))
  (asm (hlde) (push hl) (ld hlde #x12345678)))

(check-stacked-asm
  (%stacked (hl de) (%op 4 (ld hlde #x12345678)))
  (asm (hlde) (push de) (push hl) (ld hlde #x12345678)))

(check-stacked-asm
  (%stacked (lde) (%op 4 (ld hlde #x12345678)))
  (asm (hlde) (push lde) (ld hlde #x12345678)))

(check-stacked-asm
  (%stacked (hlde) (%op 4 (ld hlde #x12345678)))
  (asm (hlde) (push hlde) (ld hlde #x12345678)))

; op 1 1
(check-stacked-asm
  (%stacked () (%op 1 1 (inc a)))
  (asm (a) (pop a) (inc a)))

(check-stacked-asm
  (%stacked (a) (%op 1 1 (inc a)))
  (asm (a) (inc a)))

(check-stacked-asm
  (%stacked (a l) (%op 1 1 (inc a)))
  (asm (a l) (inc a)))

(check-stacked-asm
  (%stacked (a de) (%op 1 1 (inc a)))
  (asm (a de) (inc a)))

; op 1 1 1
(check-stacked-asm
  (%stacked () (%op 1 1 1 (add l)))
  (asm (a) (pop hl) (ld a l) (ld l h) (add l)))

(check-stacked-asm
  (%stacked (a) (%op 1 1 1 (add l)))
  (asm (a) (pop l) (add l)))

(check-stacked-asm
  (%stacked (a l) (%op 1 1 1 (add l)))
  (asm (a) (add l)))

; append
; (check-stacked-asm
;   (%stacked ()
;     (%op 1 (ld a #x12))
;     (%op 1 (ld a #x13))
;     (%op 1 1 1 (add l))
;     (%op 1 0 (write a)))
;   (asm (a) (ld a 18) (write-char a)))
