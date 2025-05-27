(library (sjasm sjasm)
  (export sjasm :)
  (import (micascheme))

  (define-keywords :)

  (define-syntax (sjasm $syntax)
    (begin
      (define (split $syntax)
        (syntax-case $syntax (:)
          (label
            (identifier? #'label)
            (cons
              #'(label 'label)
              #''label))
          ((label : . line)
            (identifier? #'label)
            (cons
              #'(label 'label)
              #'(begin 'label line)))
          (line
            (cons
              #f
              #'line))))
      (syntax-case $syntax ()
        ((_ line ...)
          (lets
            ($split (map split #'(line ...)))
            ($defs (filter-opts (map car $split)))
            ($lines (map cdr $split))
            #`(let (#,@$defs) #,@$lines))))))
)
