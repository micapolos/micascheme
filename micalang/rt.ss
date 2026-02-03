(library (micalang rt)
  (export
    make-v-pi v-pi? v-pi-arg-type v-pi-body
    make-v-neut v-neut? v-neut-head v-neut-args
    inc dec
    do-apply

    Type Bool Nat
    (rename (%v-pi v-pi) (%v-neut v-neut)))
  (import (scheme) (only (micascheme) define-rules-syntax literals))

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

  (define-rules-syntax (literals let)
    ((%v-pi (let var type) body)
      (make-v-pi type (lambda (var) body)))
    ((%v-pi type body)
      (%v-pi (let _ type) body))
    ((%v-pi decl decls ... body)
      (%v-pi decl (%v-pi decls ... body))))

  (define-rules-syntax
    ((%v-neut head arg ...)
      (make-v-neut head (list arg ...))))
)
