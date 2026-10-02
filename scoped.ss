(library (scoped)
  (export
    scoped
    define-scoped)
  (import
    (scheme)
    (syntax))

  (define-keyword scoped)

  (define-rule-syntax (define-scoped (id . params) (var make) destroy)
    (begin
      (define (fn . params) make)
      (define-syntax (id $syntax)
        (syntax-error #'id "not in scope"))
      (define-property id scoped
        (lambda ($syntax)
          (syntax-case $syntax ()
            ((_ ((var (_ . args))) body)
              #'(let ((var (fn . args)))
                (dynamic-wind
                  (lambda () #f)
                  (lambda () body)
                  (lambda () destroy)))))))))
)
