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
        case cond
        if write eval
        define-syntax
        define-ftype
        define-property
        ftype-ref ftype-&ref ftype-any-ref
        ftype-set! ftype-any-set!
        syntax-case
        parameterize define-record define-syntax
        list
        predicate set!
        syntax-error
        meta)
      (leo library)
      (char)
      (keyword)
      (syntax-keywords)
      (boolean)
      (leo in)
      (leo recursive)
      (leo sequential)
      (leo with)
      (leo check)
      (leo case)
      (leo cond)
      (leo write)
      (leo print)
      (leo test)
      (leo if)
      (leo set)
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
      (leo define-property)
      (leo ftype)
      (leo syntax-error)
      (leo meta)
      (keyword)
      (fixnum)
      (eof)
      (only (procedure) partial)
      (only (identifier) identifier-append)
      (char)
      (void)
      (syntax-keywords)
      (rename
        (only (syntax)
          define-keyword
          define-keywords
          syntax=?)
        (syntax=? free-syntax=?))
      (rename (leo load)
        (load-leo load)
        (load-leo-program load-program))
      (only (leo code) code-pretty? code-line-limit)
      (rename (leo version)
        (version leo-version))))
)
