(library (leo scheme)
  (export
    make-read-lambda
    define-language
    load load-program)
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
      (leo list)
      (leo parameterize)
      (leo syntax-case)
      (leo eval)
      (void)
      (only (leo code) code-pretty? code-line-limit)
      (rename (leo version) (version leo-version))))

  (%define load load-leo)
  (%define load-program load-leo-program)

  (define-rules-syntaxes (keywords with then %else %when %list %and %values in recursive sequential %syntax)
    ((make-read-lambda (with param ...) x xs ...)
      (%make-read-lambda (param ...) x xs ...))

    ((define-language (x l))
      (%define-language x l)))
)
