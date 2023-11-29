(library (proven)
  (export
    define-proof
    proof
    proven)
  (import
    (micascheme)
    (proof))

  ; TODO: Make sure not already proven
  (define-syntax-rule (define-proof $name $proof)
    (define-property $name proof $proof))

  (define-syntax proof
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ $item)
            (lets
              ($value ($lookup #'$item #'proof))
              #`'#,(datum->syntax #'* $value)))))))

  (define-syntax proven
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ $expr)
            (let ()
              (syntax->proof
                (lambda ($identifier) ($lookup $identifier #'proof))
                #'$expr)
              #'$expr))))))
)
