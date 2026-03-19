(library (leo scheme)
  (export
    null
    library import
    define lambda
    let letrec let-values
    let* letrec* let*-values
    let-syntax letrec-syntax
    if then cond
    switch any?
    make-read-lambda
    write
    displayln
    pretty-print
    define-language
    define-syntax
    define-macro
    syntax-case)
  (import
    (prefix (chezscheme) %)
    (only (chezscheme) export)
    (only (micascheme) define-rules-syntaxes define-keywords keywords ...)
    (prefix (only (micascheme) make-read-lambda switch) %)
    (only (keyword) keyword? keyword...?)
    (leo transform)
    (prefix (language) %)
    (writing)
    (leo reader)
    (leo datum)
    (leo writing-reader))
  (export
    (import
      (except (chezscheme)
        library import
        define lambda
        let letrec let-values
        let* letrec* let*-values
        let-syntax letrec-syntax
        if cond pretty-print write
        define-syntax
        syntax-case)
      (only (micascheme) char true false keywords)
      (only (leo transform) from with)))

  ; TODO: Implement this entire file in .leo

  (%define-syntax library transform-library)
  (%define-syntax import transform-import)

  (%define (any? _) #t)
  (%define null (%quote ()))

  (define-keywords then)

  (%define write
    (%case-lambda
      ((x)
        (write x (%current-output-port)))
      ((x port)
        ; TODO: Implement reader which would write directly to port.
        (%put-string port
          (writing-string
            (reader-end
              (reader-read-list (writing-reader)
                (%list (->datum x)))))))))

  (%define (pretty-print . xs)
    (%for-each write xs))

  (%define (displayln x)
    (%display x)
    (%newline))

  (define-rules-syntaxes (keywords with then %else %when %list keywords)
    ((define (name x))
      (%define name x))
    ((define (name param ... (l ellipses)) x xs ...)
      (keyword...? ellipses)
      (%define (name param ... . l) x xs ...))
    ((define (name param ...) x xs ...)
      (%define (name param ...) x xs ...))

    ((lambda (with param ... (l ellipses)) x xs ...)
      (keyword...? ellipses)
      (%lambda (param ... . l) x xs ...))
    ((lambda (with param ...) x xs ...)
      (%lambda (param ...) x xs ...))
    ((lambda x xs ...)
      (%lambda () x xs ...))

    ((define-syntax (name x))
      (%define-syntax name x))

    ((define-syntax (name s) x xs ...)
      (%define-syntax (name s) x xs ...))

    ((syntax-case expr (keywords k ...) (%when pattern x xs ...) ...)
      (%syntax-case expr (k ...)
        (pattern x xs ...) ...))

    ((define-macro (keywords k ...) (%when pattern x xs ...) ...)
      (define-rules-syntaxes (keywords k ...)
        (pattern x xs ...) ...))

    ((define-macro (%when pattern x xs ...) ...)
      (define-rules-syntaxes
        (pattern x xs ...) ...))

    ((make-read-lambda (with param ...) x xs ...)
      (%make-read-lambda (param ...) x xs ...))

    ((let (name (with binding ...) x xs ...))
      (keyword? name)
      (%let name (binding ...) x xs ...))
    ((let (with binding ...) x xs ...)
      (%let (binding ...) x xs ...))
    ((let-values (binding ...) x xs ...)
      (%let-values (with binding ...) x xs ...))
    ((let* (with binding ...) x xs ...)
      (%let* (binding ...) x xs ...))
    ((let*-values (with binding ...) x xs ...)
      (%let-values* (binding ...) x xs ...))
    ((letrec (with binding ...) x xs ...)
      (%letrec (binding ...) x xs ...))
    ((letrec* (with binding ...) x xs ...)
      (%letrec* (binding ...) x xs ...))
    ((let-syntax (with binding ...) x xs ...)
      (%let-syntax (binding ...) x xs ...))
    ((letrec-syntax (with binding ...) x xs ...)
      (%let-syntax (binding ...) x xs ...))

    ((if a (then b ...) (%else c ...))
      (%if a (%begin b ...) (%begin c ...)))

    ((cond (%when a b bs ...) ... (%else c cs ...))
      (%cond (a b bs ...) ... (%else c cs ...)))
    ((cond (%when a b bs ...) ...)
      (%cond (a b bs ...) ...))

    ((switch x (%when (a b) c) ... (%else d))
      (%switch x ((a b) c) ... ((%else _) d)))
    ((switch x (%when (a b) c) ...)
      (%switch x ((a b) c) ...))

    ((define-language (x l))
      (%define-language x l)))
)
