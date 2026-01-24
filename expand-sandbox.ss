(import (micascheme))

; (define (rozwal x)
;   (syntax-case x (dawaj napisz libka definiuj ładuj)
;     (s (string? (datum s)) #'s)
;     (s (symbol? (datum s)) #'s)
;     (("noexpand" . _) x)
;     ((dawaj x ...) `(begin ,@(map rozwal #'(x ...))))
;     ((napisz x) `(display ,(rozwal #'x)))
;     ((ładuj name) `(import (,#'name)))
;     ((libka name (definiuj id value) ...)
;       `(library (,#'name)
;         (export ,@#'(id ...))
;         (import (scheme))
;         ,@(map
;           (lambda ($id $value)
;             `(define ,$id ,(rozwal $value)))
;           #'(id ...)
;           #'(value ...))
;         (display "Libka załadowana\n")))))

(define (rozwal $x)
  (switch (datum/annotation-expression $x)
    ((string? $string) $string)
    ((symbol? $symbol) $symbol)
    ((pair? $pair)
      (lets
        ($car (car $pair))
        ($cdr (cdr $pair))
        (case $car
          (("noexpand") $x)
          ((dawaj) `(begin ,@(map rozwal $cdr)))
          ((napisz) `(display ,@(map rozwal $cdr)))
          ((ładuj) `(import ,@(map (lambda (x) `(,x)) $cdr)))
          (else (syntax-error $x "dupa")))))))

(current-expand
  (lambda (x env . args)
    (define rx (rozwal x))
    (displayln "=== Start")
    (pretty-print `(,x ,env ,@args))
    (displayln "=== Rozwalone")
    (pretty-print rx)
    (displayln "=== End")
    (apply sc-expand rx env args)))

(eval
  `(dawaj
    ; (libka my-lib
    ;   (definiuj x "foo\n")
    ;   (definiuj y "bar\n")
    ;   (definiuj z "zar\n"))
    ;(ładuj my-lib)
    (napisz "jajco\n")
    (napisz "burak\n")
    ;(napisz x)
  )
  (copy-environment (scheme-environment)))
