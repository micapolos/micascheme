(library (asm macro-z80)
  (export
    b c d e h l a
    ixh ixl iyh iyl
    bc de hl af
    ix iy
    r)
  (import
    (micascheme)
    (labs macro))

  (define-rule-syntax (define-registers $r ...)
    (begin
      (define-aux-keyword $r) ...
      (define-syntax-literal? $r) ...))

  (define-registers b c d e h l a)
  (define-registers ixh ixl iyh iyl)

  (define-registers bc de hl af)
  (define-registers ix iy)

  (define-aux-keywords r)

  (define-syntax-matcher (r $pattern)
    (syntax-case $pattern ()
      ((r $code)
        (values
          (list #'$code)
          #`(lambda ($syntax)
            (macro-case-opt $syntax
              (b (list #'#b000))
              (c (list #'#b001))
              (d (list #'#b010))
              (e (list #'#b011))
              (h (list #'#b100))
              (l (list #'#b101))
              (a (list #'#b111))))))))
)
