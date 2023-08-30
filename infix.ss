(library (infix)
  (export 
    : 
    infix infix: 
    := ->
    is being)
  (import (scheme) (base))

  ; --------------------------------------------------

  (define-aux-keyword :)

  (define-syntax infix
    (lambda (stx)
      (data (complete stxs))
      (data (partial stxs))
      (data (colon stxs))

      (define (compile-stxs stx)
        (define (result+ result stx)
          (switch result
            ((complete? complete)
              (syntax-case stx (:)
                (:
                  (colon (complete-stxs complete)))
                (_ 
                  (partial 
                    (append 
                      (compile-stxs stx) 
                      (complete-stxs complete))))))
            ((partial? partial) 
              (complete 
                (list 
                  #`(
                    #,@(partial-stxs partial)
                    #,@(compile-stxs stx)))))
            ((colon? c) 
              (colon 
                (append
                  (colon-stxs c)
                  (compile-stxs stx)))))) ; TODO: O(n^2) cost!!!
        (syntax-case stx (:)
          (() (list))
          ((: item ...)
            (apply append (map compile-stxs (syntax->list #`(item ...)))))
          ((first rest ...) 
            (switch (fold-left result+ (complete (compile-stxs #`first)) (syntax->list #`(rest ...)))
              ((complete? complete) 
                (complete-stxs complete))
              ((partial? partial)
                (list 
                  #`(
                    #,@(partial-stxs partial))))
              ((colon? colon)
                (list 
                  #`(
                    #,@(colon-stxs colon))))))
          (other 
            (list #`other))))

      (syntax-case stx ()
        ((_ item ...)
          #`(begin
            #,@(compile-stxs #`(item ...)))))))

  (define-syntax infix:
    (lambda (stx)
      (syntax-case stx ()
        ((_ item ...)
          #`(infix (: item ...))))))

  ; --------------------------------------

  (define-syntax :=
    (lambda (stx)
      (syntax-case stx ()
        ((_ body ...) 
          #`(define body ...)))))

  (define-syntax ->
    (lambda (stx)
      (syntax-case stx (as)
        ((_ body ...)
          #`(lambda body ...)))))

  (define-syntax is
    (lambda (stx)
      (syntax-case stx ()
        ((_ body ...) 
          #`(define body ...)))))

  (define-syntax being
    (lambda (stx)
      (syntax-case stx (as)
        ((_ body ...)
          #`(lambda body ...)))))

  ; --------------------------------------

  (assert (equal? (infix "a") "a"))
  
  (assert (equal? (infix "abc" string-length) 3))
  (assert (equal? (infix "abc" string-length ()) 3))
  (assert (equal? (infix "abc" string-length () number->string) "3"))
  (assert (equal? (infix "abc" string-length () number->string ()) "3"))
  
  (assert (equal? (infix () string-length "abc") 3))

  (assert (equal? (infix "a" string-append) "a"))
  (assert (equal? (infix "a" string-append ()) "a"))
  (assert (equal? (infix "a" string-append "b") "ab"))
  (assert (equal? (infix "a" string-append (: "b")) "ab"))
  (assert (equal? (infix "a" string-append (: "b" "c")) "abc"))

  (assert (equal? (infix ("a" string-append "b") string-append ("c" + "d")) "abcd"))
)