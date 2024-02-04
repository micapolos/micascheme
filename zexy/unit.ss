(library (zexy unit)
  (export
    define-unit)
  (import
    (except (micascheme) define)
    (only (scheme) define))

  (define-syntax define-unit
    (lambda ($syntax)
      (syntax-case $syntax (fields runner init update)
        ((_ $name
          (fields ($ftype $fname) ...))
          #'(define-record $name ()
            (
              (($ftype $fname) 0) ...))))))
)
