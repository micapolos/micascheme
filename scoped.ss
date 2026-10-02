(library (scoped)
  (export
    scoped
    define-scoped)
  (import (scheme))

  (define-syntax (scoped $syntax)
    (syntax-error $syntax "misplaced keyword"))

  (define-syntax define-scoped
    (syntax-rules ()
      ((_ (id . params) (var make) destroy)
        (and (identifier? #'id) (identifier? #'var))
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
                      (lambda () destroy)))))))))))
)
