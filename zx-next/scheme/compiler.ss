(library (zx-next scheme compiler)
  (export
    compile-define
    compile-op
    check-compile-define
    check-compile-op)
  (import
    (micascheme)
    (u)
    (prefix (zx-next scheme keywords) %)
    (prefix (zx-next core) %%)
    (prefix (zx-next scheme value) %%)
    (prefix (zx-next scheme prims) %%)
    (prefix (zx-next scheme write) %%))

  (define (compile-define $lookup $syntax)
    (syntax-case $syntax ()
      ((_ id x)
        (identifier? #'id)
        (syntax-case (compile-op $lookup #'x) ()
          ((begin def ... body)
            #`(begin def ...
              (%%define-proc (id)
                (%%inc %%d)
                (%%inc %%d)
                body)))))))

  (define (compile-op $lookup $syntax)
    (syntax-case $syntax (%begin %quote %quote %void %cons %car %cdr %write %put-char %put-string)
      (()
        #`(begin (%%load-value (%%null-value))))
      (n
        (u8? (datum n))
        #`(begin (%%load-value (%%byte-value n))))
      (nn
        (u16? (datum nn))
        #`(begin (%%load-value (%%word-value nn))))
      (#f
        #`(begin (%%load-value (%%false-value))))
      (#t
        #`(begin (%%load-value (%%true-value))))
      (ch
        (char? (datum ch))
        #`(begin (%%load-value (%%char-value ch))))
      (s
        (string? (datum s))
        (lets
          ($tmp (generate-identifier #'$string))
          #`(begin
            (%%define-fragment #,$tmp (%%dz s))
            (%%load-value (%%string-value #,$tmp)))))
      ((%quote s)
        (symbol? (datum s))
        (lets
          ($tmp (generate-identifier #'$symbol))
          #`(begin
            (%%define-fragment #,$tmp (%%dz #,(symbol->string (datum s))))
            (%%load-value (%%symbol-value #,$tmp)))))
      ((%begin x ...)
        (syntax-case (map (partial compile-op $lookup) #'(x ...)) ()
          (((_ def ... op) ...)
            #`(begin
              def ... ...
              (%%begin op ...)))))
      ((%cons a b)
        (syntax-case (compile-op $lookup #'b) (begin)
          ((begin def-a ... body-a)
            (syntax-case (compile-op $lookup #'a) (begin)
              ((begin def-b ... body-b)
                #`(begin def-a ... def-b ...
                  (%%begin
                    body-b
                    (%%push-top)
                    body-a
                    (%%cons))))))))
      ((%car a)
        (syntax-case (compile-op $lookup #'a) ()
          ((begin def ... body)
            #`(begin def ...
              (%%begin body (%%car))))))
      ((%cdr a)
        (syntax-case (compile-op $lookup #'a) ()
          ((begin def ... body)
            #`(begin def ...
              (%%begin body (%%cdr))))))
      ((%put-char x)
        (syntax-case (compile-op $lookup #'x) ()
          ((begin def ... body)
            #`(begin def ...
              (%%begin body (%%put-char))))))
      ((%put-string x)
        (syntax-case (compile-op $lookup #'x) ()
          ((begin def ... body)
            #`(begin def ...
              (%%begin body (%%put-string))))))
      ((%write s)
        (syntax-case (compile-op $lookup #'s) ()
          ((begin def ... body)
            #`(begin def ...
              (%%begin body (%%write))))))))

  (define-rule-syntax (check-compile-define lookup in out)
    (check (equal? (syntax->datum (compile-define lookup #'in)) 'out)))

  (define-rule-syntax (check-compile-op lookup in out)
    (check (equal? (syntax->datum (compile-op lookup #'in)) 'out)))
)
