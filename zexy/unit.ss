(library (zexy unit)
  (export
    define-unit)
  (import
    (except (micascheme) define)
    (only (scheme) define))

  (define-syntax (define-unit $syntax)
    (syntax-case $syntax (fields runner init update)
      ((_ $name (fields ($ftype $fname) ...))
        (identifier-named? #'fields fields)
        #'(define-record $name ()
          (
            (($ftype $fname) 0) ...)))))
)
