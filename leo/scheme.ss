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
        parameterize define-record define-syntax
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
      (leo definer)
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
      (leo define-record)
      (leo define-syntax)
      (void)
      (rename (leo load)
        (load-leo load)
        (load-leo-program load-program))
      (only (leo code) code-pretty? code-line-limit)
      (rename (leo version)
        (version leo-version))))
)
