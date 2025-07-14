(library (asm-2 lang)
  (export
    define-fragment
    fragment-bytevector)
  (import (micascheme) (asm-2 assembled) (asm-2 fragment))

  (define-rule-syntax (define-fragment id fragment)
    (define-syntax id (make-compile-time-value fragment)))

  (define-syntax (fragment-bytevector $syntax $lookup)
    (syntax-case $syntax ()
      ((_ id org)
        (identifier? #'id)
        (fragment->bytevector
          (lambda ($label)
            (switch ($lookup $label)
              ((false? _) (syntax-error $label "fragment not found"))
              ((fragment? $fragment) $fragment)
              ((else $other) (syntax-error $label "not a fragment"))))
          #'id
          (datum org)))))
)
