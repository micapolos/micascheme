(import (asm z80) (prefix (zx-next compiler stacked) %) (zx-next compiler stacked-asm))

; const 1
(check-stacked-asm
  (%stacked () (%const 1 #x12))
  (asm (a) (ld a #x12)))

(check-stacked-asm
  (%stacked (a) (%const 1 #x12))
  (asm (a l) (ld l a) (ld a #x12)))

(check-stacked-asm
  (%stacked (a de) (%const 1 #x12))
  (asm (a l) (push de) (ld l a) (ld a #x12)))

(check-stacked-asm
  (%stacked (hl) (%const 1 #x12))
  (asm (a de) (ex de hl) (ld a #x12)))

(check-stacked-asm
  (%stacked (hl de) (%const 1 #x12))
  (asm (a de) (push de) (ex de hl) (ld a #x12)))

(check-stacked-asm
  (%stacked (hlde) (%const 1 #x12))
  (asm (a ) (push hlde) (ld a #x12)))

; op2 1
(check-stacked-asm
  (%stacked () (%op2 add 1))
  (asm (a) (pop hl) (ld a l) (add h)))

(check-stacked-asm
  (%stacked (a) (%op2 add 1))
  (asm (a) (pop l) (add l)))

(check-stacked-asm
  (%stacked (a l) (%op2 add 1))
  (asm (a) (add l)))
