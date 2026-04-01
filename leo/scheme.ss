(library (leo scheme)
  (export
    null
    syntax-case
    eval
    any?
    make-read-lambda
    list closed-list open-list
    define-language
    logging
    load load-program

    parameterize)
  (import
    (prefix (scheme) %)
    (only (scheme) ...)
    (syntax-keywords)
    (keyword)
    (syntax)
    (syntaxes)
    (prefix (system) %)
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
    (leo let)
    (leo in)
    (leo writing-reader))
  (%export
    (import
      (leo library)
      (except (chezscheme)
        library import export display display-string
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
      (char)
      (keyword)
      (syntaxes)
      (syntax-keywords)
      (boolean)
      (leo in)
      (leo recursive)
      (leo sequential)
      (leo with)
      (leo check)
      (leo write)
      (leo test)
      (leo if)
      (leo then)
      (leo document)
      (leo lambda)
      (leo define)
      (leo let)
      (leo switch)
      (leo math)
      (leo display)
      (void)
      (only (leo code) code-pretty? code-line-limit)
      (rename (leo version) (version leo-version))))

  (%define (any? _) #t)
  (%define null (%quote ()))

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

  (define-rules-syntaxes (keywords with then %else %when %list %and %values in recursive sequential %syntax)
    ((syntax-case expr (keywords k ...) (%when pattern x xs ...) ...)
      (%syntax-case expr (k ...)
        (pattern x xs ...) ...))

    ((make-read-lambda (with param ...) x xs ...)
      (%make-read-lambda (param ...) x xs ...))

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
