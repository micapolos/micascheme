(library (leo scheme)
  (export
    the
    library import
    define lambda
    let letrec let-values
    let* letrec* let*-values
    let-syntax letrec-syntax
    if then cond
    switch any?
    make-read-lambda
    write
    pretty-print)
  (import
    (prefix (chezscheme) %)
    (only (chezscheme) export define-syntax)
    (only (micascheme) define-rules-syntaxes define-keywords keywords ...)
    (prefix (only (micascheme) make-read-lambda switch) %)
    (only (keyword) keyword?)
    (leo transform)
    (writing)
    (leo reader)
    (leo writing-reader))
  (export
    (import
      (except (chezscheme)
        library import
        define lambda
        let letrec let-values
        let* letrec* let*-values
        let-syntax letrec-syntax
        if cond pretty-print write)
      (only (micascheme) char true false)
      (only (leo transform) from with)))

  (define-syntax library transform-library)
  (define-syntax import transform-import)

  (%define (any? _) #t)

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
                (%list x))))))))

  (%define (pretty-print . xs)
    (%for-each write xs))

  (define-rules-syntaxes (keywords with then %else %when %list)
    ((the x ...)
      (x ...))
    ((define (name x))
      (%define name x))
    ((define (name param ...) body ...)
      (%define (name param ...) body ...))

    ((lambda (with param ... (%list l)) x xs ...)
      (%lambda (param ... . l) x xs ...))
    ((lambda (with param ...) x xs ...)
      (%lambda (param ...) x xs ...))
    ((lambda x xs ...)
      (%lambda () x xs ...))

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
      (%switch x ((a b) c) ...)))
)
