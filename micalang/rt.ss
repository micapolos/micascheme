(library (micalang rt)
  (export
    make-v-pi v-pi? v-pi-arg-type v-pi-body
    make-v-neut v-neut? v-neut-head v-neut-args
    inc dec
    do-apply

    Type Bool Nat
    pi)
  (import (scheme) (only (micascheme) define-rules-syntax))

  (define-record-type v-pi   (fields arg-type body))

  (define-record-type v-neut (fields head args))

  (define (do-apply f a)
   (cond [(procedure? f)
            (cond
              [(v-neut? a) (make-v-neut f (list a))]
              [else (f a)])]
         [(v-pi? f) ((v-pi-body f) a)]
         [(v-neut? f) (make-v-neut (v-neut-head f) (append (v-neut-args f) (list a)))]
         [else (error 'do-apply "Bad app" f)]))

  (define (inc x) (+ x 1))
  (define (dec x) (- x 1))

  (define Type 'Type)
  (define Bool 'Bool)
  (define Nat 'Nat)

  (define-rules-syntax
    ((pi (var type) body)
      (make-v-pi type (lambda (var) body)))
    ((pi type body)
      (pi (_ type) body))
    ((pi decl decls ... body)
      (pi decl (pi decls ... body))))
)
