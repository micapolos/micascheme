(library (emu unit)
  (export define-unit fields)
  (import (scheme) (syntax) (syntaxes) (identifier) (lets) (emu internal))

  (define-syntax (define-unit $syntax)
    (syntax-case $syntax ()
      (($ (id param ...) (field-id field-init) ...)
        (lets
          ($tmp #'$)
          ($id #'id)
          ($field-ids (syntax->list #'(field-id ...)))
          ($field-inits (syntax->list #'(field-init ...)))
          ($internal-ids
            (map
              (lambda ($field-id)
                (identifier-append $tmp $id #'- $field-id))
              $field-ids))
          ($internal-defs
            (map
              (lambda ($internal-id)
                #`(define-internal #,$internal-id))
              $internal-ids))
          ($internal-define-ids
            (map
              (lambda ($internal-id)
                (identifier-append $tmp #'define- $internal-id))
              $internal-ids))
          ($define-id
            (identifier-append $tmp #'define- $id))
          ($internal-initializers
            (map
              (lambda ($define-id $field-init)
                #`(#,$define-id #,$id #,$field-init))
              $internal-define-ids
              $field-inits))
          #`(begin
            #,@$internal-defs
            (define-rules-syntaxes
              ((#,$define-id id param ...)
                (begin
                  (define-aux-keyword id)
                  #,@$internal-initializers))))))))
)
