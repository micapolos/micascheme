(import (micascheme) (minic runtime) (minic syntax-type) (minic type))

(check (equal? (syntax->type #'(int 7 const #x40)) (int-type 7)))
(check (equal? (syntax->type #'(int 7 inc (int 7 const #x40))) (int-type 7)))
(check (equal? (syntax->type #'(int 7 dec (int 7 const #x40))) (int-type 7)))
(check (equal? (syntax->type #'(int 7 add (int 7 const #x40) (int 7 const #x50))) (int-type 7)))
(check (equal? (syntax->type #'(int 7 sub (int 7 const #x40) (int 7 const #x50))) (int-type 7)))

(check (raises (syntax->type #'"foo"))) ; untyped
(check (raises (syntax->type #'(int 100 const #x40)))) ; bits outside of range
(check (raises (syntax->type #'(int 7 const #x80)))) ; value outside of range
(check (raises (syntax->type #'(int 7 inc (int 8 const #x40))))) ; type mismatch
(check (raises (syntax->type #'(int 7 dec (int 8 const #x40))))) ; type mismatch
(check (raises (syntax->type #'(int 7 add (int 7 const #x40) (int 8 const #x50))))) ; type mismatch
(check (raises (syntax->type #'(int 7 sub (int 8 const #x40) (int 8 const #x50))))) ; type mismatch
