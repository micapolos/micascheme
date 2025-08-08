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
    (syntax-case $syntax
      (
        %begin %quote %throw
        %null? %void? %boolean? %byte? %word? %char? %symbol? %string? %pair?
        %void %box %cons %car %cdr
        %write
        %put-char %put-string)
      ((%quote ())
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
      ((%throw a) (compile-op-1 $lookup #'%%throw #'a))
      ((%null? a) (compile-op-1 $lookup #'%%null? #'a))
      ((%void? a) (compile-op-1 $lookup #'%%void? #'a))
      ((%boolean? a) (compile-op-1 $lookup #'%%boolean? #'a))
      ((%byte? a) (compile-op-1 $lookup #'%%byte? #'a))
      ((%word? a) (compile-op-1 $lookup #'%%word? #'a))
      ((%char? a) (compile-op-1 $lookup #'%%char? #'a))
      ((%symbol? a) (compile-op-1 $lookup #'%%symbol? #'a))
      ((%string? a) (compile-op-1 $lookup #'%%string? #'a))
      ((%pair? a) (compile-op-1 $lookup #'%%pair? #'a))
      ((%void) #'(%%void))
      ((%box a) (compile-op-1 $lookup #'%%box #'a))
      ((%cons a b) (compile-op-2 $lookup #'%%cons #'a #'b))
      ((%car a) (compile-op-1 $lookup #'%%car #'a))
      ((%cdr a) (compile-op-1 $lookup #'%%cdr #'a))
      ((%put-char a) (compile-op-1 $lookup #'%%put-char #'a))
      ((%put-string a) (compile-op-1 $lookup #'%%put-string #'a))
      ((%write a) (compile-op-1 $lookup #'%%write #'a))))

  (define (compile-op-1 $lookup $op $arg)
    (syntax-case (compile-op $lookup $arg) ()
      ((begin def ... body)
        #`(begin def ...
          (%%begin body (#,$op))))))

  (define (compile-op-2 $lookup $op $arg-a $arg-b)
    (syntax-case (compile-op $lookup $arg-b) (begin)
      ((begin def-b ... body-b)
        (syntax-case (compile-op $lookup $arg-a) (begin)
          ((begin def-a ... body-a)
            #`(begin def-a ... def-b ...
              (%%begin
                body-b
                (%%push-top)
                body-a
                (#,$op))))))))

  (define-rule-syntax (check-compile-define lookup in out)
    (check (equal? (syntax->datum (compile-define lookup #'in)) 'out)))

  (define-rule-syntax (check-compile-op lookup in out)
    (check (equal? (syntax->datum (compile-op lookup #'in)) 'out)))
)
