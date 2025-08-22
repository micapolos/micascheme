/(library (zx-next compiler named-indexed)
  (export
    lets indexed
    named->indexed
    check-named->indexed)
  (import
    (rename (micascheme)
      (lets %lets)
      (indexed %indexed)))

  (define-keywords lets indexed)

  (define (named->indexed $named)
    (syntax-case $named (indexed lets)
      ((_ (indexed x))
        #`(() x))
      ((() x)
        (identifier? #'x)
        #`((x) x))
      (((id id* ...) x)
        (identifier? #'x)
        (if (free-identifier=? #'id #'x)
          #`(() #,(length #'(id* ...)))
          (named->indexed #`((id* ...) x))))
      ((ids (lets (id expr) ... body))
        (syntax-case
          (map-with
            ($id #'(id ...))
            ($expr #'(expr ...))
            #`(#,$id . #,(named->indexed #`(ids #,$expr)))) ()
          (((id (free-id ...) expr) ...)
            (syntax-case (named->indexed #`((id ... . ids) body)) ()
              (((body-free-id ...) body)
                #`(#,(dedup free-identifier=? #'(free-id ... ... body-free-id ...))
                  (lets #,(length #'(id ...)) body)))))))
      ((ids (fn arg ...))
        (syntax-case (named->indexed #'(ids fn)) ()
          (((fn-free-id ...) indexed-fn)
            (syntax-case (map-with ($arg #'(arg ...)) (named->indexed #`(ids #,$arg))) ()
              ((((arg-free-id ...) indexed-arg) ...)
                #`(#,(dedup free-identifier=? #'(fn-free-id ... arg-free-id ... ...))
                  (indexed-fn indexed-arg ...)))))))))

  (define-rule-syntax (check-named->indexed in out)
    (check (equal? (syntax->datum (named->indexed #'in)) 'out)))
)
