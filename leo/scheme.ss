(library (leo scheme)
  (export
    null in
    define lambda
    let letrec let-values
    let* letrec* let*-values
    let-syntax letrec-syntax
    if then cond
    switch any?
    make-read-lambda
    list closed-list open-list
    display-line
    define-language
    define-syntax
    syntax-case
    logging
    load load-program

    add subtract multiply divide
    number=? less? less/equal? greater? greater/equal?
    parameterize)
  (import
    (prefix (chezscheme) %)
    (only (micascheme) run define-rules-syntaxes define-keywords keywords ...)
    (prefix (only (micascheme) make-read-lambda switch logging) %)
    (only (keyword) keyword? keyword...?)
    (leo transform)
    (prefix (language) %)
    (writing)
    (code)
    (leo code)
    (leo quotify)
    (leo reader)
    (leo datum)
    (leo load)
    (leo writing-reader))
  (%export
    (import
      (leo library)
      (except (chezscheme)
        library import export
        top-level-program
        except only rename alias prefix add-prefix drop-prefix
        load load-program
        define lambda
        let letrec let-values
        let* letrec* let*-values
        let-syntax letrec-syntax
        if cond write
        define-syntax
        syntax-case
        parameterize
        list)
      (only (micascheme) integer char true false keywords run)
      (only (leo transform) with)
      (only (char) code)
      (leo check)
      (leo write)
      (leo test)
      (only (leo code) code-pretty? code-line-limit)
      (rename (leo version) (version leo-version))))

  (%define (any? _) #t)
  (%define null (%quote ()))

  (define-keywords then in)

  (%define (display-line x)
    (%display x)
    (%newline))

  ; TODO: Move it to a separate library
  (%define add %+)
  (%define subtract %-)
  (%define multiply %*)
  (%define divide %/)

  (%define number=? %=)
  (%define less? %<)
  (%define less/equal? %<=)
  (%define greater? %>)
  (%define greater/equal? %>=)

  (%define closed-list %list)
  (%define open-list %list*)

  (%define load load-leo)
  (%define load-program load-leo-program)

  (define-rules-syntaxes (keywords with then %else %when %list %and keywords %values in)
    ((define (name x))
      (%define name x))
    ((define (name param ... (%and last)) x xs ...)
      (%define (name param ... . last) x xs ...))
    ((define (name param ...) x xs ...)
      (%define (name param ...) x xs ...))

    ((lambda (with param ... (%and last)) x xs ...)
      (%lambda (param ... . last) x xs ...))
    ((lambda (with param ...) x xs ...)
      (%lambda (param ...) x xs ...))
    ((lambda x xs ...)
      (%lambda () x xs ...))

    ((define-syntax (keywords k ...) (%when pattern x xs ...) ...)
      (define-rules-syntaxes (keywords k ...)
        (pattern x xs ...) ...))

    ((define-syntax (%when pattern x xs ...) ...)
      (define-syntax (keywords) (%when pattern x xs ...) ...))

    ((define-syntax (name x))
      (keyword? name)
      (%define-syntax name x))

    ((define-syntax (name s) x xs ...)
      (keyword? name)
      (%define-syntax (name s) x xs ...))

    ((syntax-case expr (keywords k ...) (%when pattern x xs ...) ...)
      (%syntax-case expr (k ...)
        (pattern x xs ...) ...))

    ((make-read-lambda (with param ...) x xs ...)
      (%make-read-lambda (param ...) x xs ...))

    ((let (name binding ... (in x xs ...)))
      (keyword? name)
      (%let name (binding ...) x xs ...))
    ((let binding ... (in x xs ...))
      (%let (binding ...) x xs ...))
    ((let-values (with (%values id ...) expr) ... (in x xs ...))
      (%let-values (((id ...) expr) ...) x xs ...))
    ((let* binding ... (in x xs ...))
      (%let* (binding ...) x xs ...))
    ((let*-values (with (%values id ...) expr) ... (in x xs ...))
      (%let*-values (((id ...) expr) ...) x xs ...))
    ((letrec binding ... (in x xs ...))
      (%letrec (binding ...) x xs ...))
    ((letrec* binding ... (in x xs ...))
      (%letrec* (binding ...) x xs ...))
    ((let-syntax binding ... (in x xs ...))
      (%let-syntax (binding ...) x xs ...))
    ((letrec-syntax binding ... (in x xs ...))
      (%let-syntax (binding ...) x xs ...))

    ((if a b c) (%if a b c))

    ((cond (%when a b bs ...) ... (%else c cs ...))
      (%cond (a b bs ...) ... (%else c cs ...)))
    ((cond (%when a b bs ...) ...)
      (%cond (a b bs ...) ...))

    ((switch x (%when (a b) c) ... (%else d))
      (%switch x ((a b) c) ... ((%else _) d)))
    ((switch x (%when (a b) c) ...)
      (%switch x ((a b) c) ...))

    ((list xs ... (%and last)) (open-list xs ... last))
    ((list xs ... ) (closed-list xs ...))

    ((logging x)
      (let
        (with (val x))
        (write val)
        val))
    ((logging label x)
      (let
        (with (val x))
        (write (%list (%quote label) val))
        val))

    ((parameterize binding ... (in x xs ...))
      (%parameterize (binding ...) x xs ...))

    ((define-language (x l))
      (%define-language x l)))
)
