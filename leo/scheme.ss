(library (leo scheme)
  (export)
  (import (scheme))
  (export
    (import
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
        list
        predicate set!)
      (leo library)
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
      (leo todo)
      (leo comment)
      (leo parameterize)
      (leo syntax-case)
      (leo eval)
      (leo logging)
      (leo maker)
      (leo make)
      (leo predicate)
      (leo is?)
      (leo getter-leo)
      (leo get)
      (leo setter!)
      (leo set!)
      (leo record)
      (void)
      (rename (leo load)
        (load-leo load)
        (load-leo-program load-program))
      (only (leo code) code-pretty? code-line-limit)
      (rename (leo version)
        (version leo-version))))
)
