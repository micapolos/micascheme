(import (micascheme) (tim type) (tim parser))

(define-syntax (typed! $syntax)
  (syntax-case $syntax ()
    ((_ $expr)
      (lets
        ($typed (typed-expr (lambda (_) #f) #'$expr))
        #`(typed
          #,(type->syntax (typed-type $typed))
          #,(typed-value $typed))))))

; type of types
(check
  (equal?
    (typed! type)
    (typed (universe 1) (universe 0))))

; types
(check
  (equal?
    (typed! (index 256))
    (typed (universe 0) (index-type 256))))

(check
  (equal?
    (typed! (array string 256))
    (typed (universe 0) (array-type (string-type) 256))))

(check
  (equal?
    (typed! string)
    (typed (universe 0) (string-type))))

; values
(check
  (equal?
    (typed! (index #x100 #xff))
    (typed (index-type #x100) #xff)))

(check
  (equal?
    (typed! "foo")
    (typed (string-type) "foo")))
