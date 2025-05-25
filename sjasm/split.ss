(library (sjasm split)
  (export splita)
  (import (micascheme))

  (define (splita $syntaxes)
    (lets
      ($split (vector '() '() '()))
      (run (for-each (partial split-add! $split) $syntaxes))
      #`(let ()
        #,@(reverse (vector-ref $split 0))
        #,@(reverse (vector-ref $split 1))
        #,@(reverse (vector-ref $split 2))
        (void))))

  (define (split-add! $split $syntax)
    (syntax-case $syntax (define)
      ((define id)
        (vector-set! $split 0
          (cons
            #'(define id (random 1))
            (vector-ref $split 0))))
      ((define id expr)
        (vector-set! $split 1
          (cons $syntax
            (vector-ref $split 1))))
      (other
        (vector-set! $split 2
          (cons $syntax
            (vector-ref $split 2))))))
)
