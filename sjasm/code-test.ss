(import (micascheme) (code) (sjasm code) (sjasm keywords))

(define-rule-syntax (check-expr expr string)
  (check (equal? (code-string (expr-code #'expr)) string)))

(define-rule-syntax (check-instr instr string ...)
  (check (equal? (code-string (instr-code #'instr)) (lines-string0 string ...))))

(define-rule-syntax (check-line line string ...)
  (check (equal? (code-string (line-code #'line)) (lines-string0 string ...))))

(check-expr id "id")
(check-expr foo_bar "foo_bar")
(check-expr 123 "123")
(check-expr (+ ix 2) "ix + 2")
(check-expr (hl) "(hl)")

(check-instr (nop) "nop")
(check-instr (ld a b) "ld a, b")
(check-instr (ld (hl) #xff) "ld (hl), 255")
(check-instr (ld ((+ ix 2)) sp) "ld (ix + 2), sp")
(check-instr (savenex open "foo" 123) "savenex open \"foo\", 123")
(check-instr (include "../foo.asm") "include ../foo.asm")

(check-line foo "foo")
(check-line (foo : nop) "foo nop")
(check-line (nop) "  nop")
(check-line (savenex open "foo" 123) "  savenex open \"foo\", 123")
