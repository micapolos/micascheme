(library (micalang rt)
  (export
    make-v-pi v-pi? v-pi-arg-type v-pi-body
    make-v-neut v-neut? v-neut-head v-neut-args
    do-apply)
  (import (scheme))

  (define-record-type v-pi   (fields arg-type body))

  (define-record-type v-neut (fields head args))

  (define (do-apply f a)
   (cond [(procedure? f) (f a)]
         [(v-pi? f) ((v-pi-body f) a)]
         [(v-neut? f) (make-v-neut (v-neut-head f) (append (v-neut-args f) (list a)))]
         [else (error 'do-apply "Bad app" f)])))
