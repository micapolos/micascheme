(library (sjasm z80)
  (export ld a bc de)
  (import (micascheme))

  (define-syntax (a $syntax)
    (syntax-case $syntax ()
      (id
        (identifier? #'id)
        #''id)))

  (define-syntax (bc $syntax)
    (syntax-case $syntax ()
      (id
        (identifier? #'id)
        #''id)))

  (define-syntax (de $syntax)
    (syntax-case $syntax ()
      (id
        (identifier? #'id)
        #''id)))

  (define-rule-syntax (ld l r)
    `(ld ,l ,r))

  (define-rule-syntax (ld bc de)
    `(begin
      (ld ,c ,d)
      (ld ,d ,e)))
)
