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
        (syntax-case (compile-op $lookup #'(2 x)) ()
          ((begin def ... body)
            #`(begin def ...
              (%%define-proc (id) body)))))))

  (define (compile-op $lookup $syntax)
    (syntax-case $syntax ()
      ((offset expr)
        (syntax-case #'expr (%begin %quote %quote %void %cons %car %cdr %write %put-char %put-string)
          (()
            #`(begin (%%load-value (%%null-value offset))))
          (n
            (u8? (datum n))
            #`(begin (%%load-value (%%byte-value offset n))))
          (nn
            (u16? (datum nn))
            #`(begin (%%load-value (%%word-value offset nn))))
          (#f
            #`(begin (%%load-value (%%false-value offset))))
          (#t
            #`(begin (%%load-value (%%true-value offset))))
          (ch
            (char? (datum ch))
            #`(begin (%%load-value (%%char-value offset ch))))
          (s
            (string? (datum s))
            (lets
              ($tmp (generate-identifier #'$string))
              #`(begin
                (%%define-fragment #,$tmp (%%dz s))
                (%%load-value (%%string-value offset #,$tmp)))))
          ((%quote s)
            (symbol? (datum s))
            (lets
              ($tmp (generate-identifier #'$symbol))
              #`(begin
                (%%define-fragment #,$tmp (%%dz #,(symbol->string (datum s))))
                (%%load-value (%%symbol-value offset #,$tmp)))))
          ((%begin)
            #`(begin (%%begin)))
          ((%begin x x* ...)
            (syntax-case (compile-op $lookup #'(offset x)) ()
              ((begin def-1 ... body-1)
                (syntax-case (compile-op $lookup #'(0 (%begin x* ...))) ()
                  ((begin def-2 ... body-2)
                    #`(begin def-1 ... def-2 ...
                      (%%begin body-1 body-2)))))))
          ((%cons a b)
            (syntax-case (compile-op $lookup #'(0 b)) (begin)
              ((begin def-a ... body-a)
                (syntax-case (compile-op $lookup #'(offset a)) (begin)
                  ((begin def-b ... body-b)
                    #`(begin def-a ... def-b ...
                      (%%begin
                        body-b
                        (%%push-top)
                        body-a
                        (%%cons))))))))
          ((%car a)
            (syntax-case (compile-op $lookup #'(offset a)) ()
              ((begin def ... body)
                #`(begin def ...
                  (%%begin body (%%car))))))
          ((%cdr a)
            (syntax-case (compile-op $lookup #'(offset a)) ()
              ((begin def ... body)
                #`(begin def ...
                  (%%begin body (%%cdr))))))
          ((%put-char x)
            (syntax-case (compile-op $lookup #'(offset x)) ()
              ((begin def ... body)
                #`(begin def ...
                  (%%begin body (%%put-char))))))
          ((%put-string x)
            (syntax-case (compile-op $lookup #'(offset x)) ()
              ((begin def ... body)
                #`(begin def ...
                  (%%begin body (%%put-string))))))
          ((%write s)
            (syntax-case (compile-op $lookup #'(offset s)) ()
              ((begin def ... body)
                #`(begin def ...
                  (%%begin body (%%write))))))))))

  (define-rule-syntax (check-compile-define lookup in out)
    (check (equal? (syntax->datum (compile-define lookup #'in)) 'out)))

  (define-rule-syntax (check-compile-op lookup in out)
    (check (equal? (syntax->datum (compile-op lookup #'in)) 'out)))
)
