(library (leo scheme)
  (export
    null in
    define lambda value
    named recursive sequential
    syntax-case
    let
    eval
    if then cond
    switch any?
    make-read-lambda
    list closed-list open-list
    display-line
    define-language
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
    (leo write)
    (leo expand)
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
        if cond write eval
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

  (define-keywords then in named recursive sequential value)

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

  (%define eval
    (%case-lambda
      ((x)
        (eval x (%interaction-environment)))
      ((x env)
        (%eval
          (%cons leo-expand x)
          env))))

  (define-rules-syntaxes (keywords value lambda with then %else %when %list %and keywords %values in named recursive sequential %syntax)
    ((define-1 (value (name x)))
      (keyword? name)
      (%define name x))

    ((define-1 (lambda (name param ... (%and last)) x xs ...))
      (%define (name param ... . last) x xs ...))
    ((define-1 (lambda (name param ...) x xs ...))
      (%define (name param ...) x xs ...))

    ((define-1 (%syntax (keywords k ...) (%when pattern x xs ...) ...))
      (define-rules-syntaxes (keywords k ...)
        (pattern x xs ...) ...))

    ((define-1 (%syntax (%when pattern x xs ...) ...))
      (define (%syntax (keywords) (%when pattern x xs ...) ...)))

    ((define-1 (%syntax (name x)))
      (keyword? name)
      (%define-syntax name x))

    ((define-1 (%syntax (name s) x xs ...))
      (keyword? name)
      (%define-syntax (name s) x xs ...))

    ((define-1 (name x))
      (define-1 (value (name x))))

    ((define x ...)
      (%begin
        (define-1 x)
        ...))

    ((syntax-case expr (keywords k ...) (%when pattern x xs ...) ...)
      (%syntax-case expr (k ...)
        (pattern x xs ...) ...))

    ((lambda (with param ... (%and last)) x xs ...)
      (%lambda (param ... . last) x xs ...))
    ((lambda (with param ...) x xs ...)
      (%lambda (param ...) x xs ...))
    ((lambda x xs ...)
      (%lambda () x xs ...))

    ((make-read-lambda (with param ...) x xs ...)
      (%make-read-lambda (param ...) x xs ...))

    ((let (with (%values id ...) expr) ... (in x xs ...))
      (%let-values (((id ...) expr) ...) x xs ...))
    ((let (sequential (recursive binding ... (in x xs ...))))
      (%letrec* (binding ...) x xs ...))
    ((let (sequential (with (%values id ...) expr) ... (in x xs ...)))
      (%let*-values (((id ...) expr) ...) x xs ...))
    ((let (sequential binding ... (in x xs ...)))
      (%let* (binding ...) x xs ...))
    ((let (recursive (%syntax binding ... (in x xs ...))))
      (%let-syntax (binding ...) x xs ...))
    ((let (recursive binding ... (in x xs ...)))
      (%letrec (binding ...) x xs ...))
    ((let (%syntax binding ... (in x xs ...)))
      (%let-syntax (binding ...) x xs ...))
    ((let (name binding ... (in x xs ...)))
      (keyword? name)
      (%let name (binding ...) x xs ...))
    ((let binding ... (in x xs ...))
      (%let (binding ...) x xs ...))

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
        (val x)
        (in
          (write val)
          val)))
    ((logging label x)
      (let
        (val x)
        (in
          (write (%list (%quote label) val))
          val)))

    ((parameterize binding ... (in x xs ...))
      (%parameterize (binding ...) x xs ...))

    ((define-language (x l))
      (%define-language x l)))
)
