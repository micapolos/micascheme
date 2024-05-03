(library (emu unit)
  (export define-unit fields)
  (import (scheme) (syntax) (syntaxes) (identifier) (lets) (list-syntax) (emu internal))

  (define-syntax (define-unit $syntax)
    (syntax-case $syntax ()
      (($ (id param ...) (field init) ...)
        (lets
          ($tmp #'$)
          ($id #'id)
          ($params (syntax->list #'(param ...)))
          ($fields (syntax->list #'(field ...)))
          ($ids (append $params $fields))
          ($inits (syntax->list #'(init ...)))
          ($internal-ids
            (map-with
              ($field-id $ids)
              (identifier-append $tmp $id #'- $field-id)))
          ($internal-defs
            (map-with
              ($internal-id $internal-ids)
              #`(define-internal #,$internal-id)))
          ($internal-define-ids
            (map-with
              ($internal-id $internal-ids)
              (identifier-append $tmp #'define- $internal-id)))
          ($define-id
            (identifier-append $tmp #'define- $id))
          ($internal-initializers
            (map-with
              ($define-id $internal-define-ids)
              ($field-init (append $params $inits))
              #`(#,$define-id #,$id #,$field-init)))
          #`(begin
            #,@$internal-defs
            (define-rules-syntaxes
              ((#,$define-id id param ...)
                (begin
                  (define-aux-keyword id)
                  #,@$internal-initializers))))))))
)
