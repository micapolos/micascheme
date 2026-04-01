(library (leo scheme)
  (export
    null in
    named recursive sequential
    syntax-case
    let
    eval
    any?
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
    (prefix (only (micascheme) make-read-lambda logging) %)
    (only (keyword) keyword? keyword...?)
    (leo transform)
    (prefix (language) %)
    (writing)
    (code)
    (leo code)
    (leo quotify)
    (leo reader)
    (leo datum)
    (leo with)
    (leo lambda)
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
        if write eval
        define-syntax
        syntax-case
        parameterize
        list)
      (only (micascheme) integer char true false keywords run)
      (only (syntax-keywords) keywords)
      (only (char) code)
      (leo with)
      (leo check)
      (leo write)
      (leo test)
      (leo if)
      (leo document)
      (leo lambda)
      (leo define)
      (leo switch)
      (void)
      (only (leo code) code-pretty? code-line-limit)
      (rename (leo version) (version leo-version))))

  (%define (any? _) #t)
  (%define null (%quote ()))

  (define-keywords then in named recursive sequential)

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

  (define-rules-syntaxes (keywords with then %else %when %list %and keywords %values in named recursive sequential %syntax)
    ((syntax-case expr (keywords k ...) (%when pattern x xs ...) ...)
      (%syntax-case expr (k ...)
        (pattern x xs ...) ...))

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
