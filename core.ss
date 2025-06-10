(library (core)
  (export
    cons!
    push!
    define-set-op!
    set+!
    set-!
    set*!)
  (import (scheme))

  (define-syntax cons!
    (syntax-rules ()
      ((_ expr var)
        (identifier? #'var)
        (set! var (cons expr var)))))

  (define-syntax push!
    (syntax-rules ()
      ((_ var expr)
        (identifier? #'var)
        (set! var (cons expr var)))))

  (define-syntax define-set-op!
    (syntax-rules ()
      ((_ id op)
        (define-syntax id
          (syntax-rules ()
            ((_ var arg (... ...))
              (identifier? #'var)
              (set! var (op var arg (... ...)))))))))

  (define-set-op! set+! +)
  (define-set-op! set-! -)
  (define-set-op! set*! *)
)
