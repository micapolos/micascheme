(library (leo scheme)
  (export
    library import
    define lambda
    let letrec let-values
    let* letrec* let*-values
    let-syntax letrec-syntax)
  (import
    (prefix (chezscheme) %)
    (only (chezscheme) export define-syntax)
    (only (micascheme) define-rules-syntaxes keywords ...)
    (only (keyword) keyword?)
    (leo transform))
  (export
    (import
      (except (chezscheme)
        library import
        define lambda
        let letrec let-values
        let* letrec* let*-values
        let-syntax letrec-syntax)
      (only (leo transform) from with)))

  (define-syntax library transform-library)
  (define-syntax import transform-import)

  (define-rules-syntaxes (keywords with)
    ((define (name x))
      (%define name x))
    ((define (name param ...) body ...)
      (%define (name param ...) body ...))
    ((lambda (with param ...) x xs ...)
      (%lambda (param ...) x xs ...))
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
      (%let-syntax (binding ...) x xs ...)))
)
