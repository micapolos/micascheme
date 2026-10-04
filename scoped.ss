(library (scoped)
  (export
    scoped
    define-scoped)
  (import (scheme))

  (define-syntax (scoped $syntax)
    (syntax-error $syntax "misplaced keyword"))

  (define-syntax define-scoped
    (syntax-rules ()
      ((_ id make init destroy)
        (identifier? #'id)
        (begin
          (define-syntax (id $syntax)
            (syntax-error #'id "not in scope"))
          (define-property id scoped
            (lambda ($syntax)
              (syntax-case $syntax ()
                ((_ ((var (_ . args))) body)
                  #'(let ((var (make . args)))
                    (init var)
                    (dynamic-wind
                      (lambda () #f)
                      (lambda () body)
                      (lambda () (destroy var))))))))))
      ((_ (id . params) (var make-body) init-body destroy-body)
        (and (identifier? #'id) (identifier? #'var))
        (define-scoped id
          (lambda params make-body)
          (lambda (var) init-body)
          (lambda (var) destroy-body)))))
)
