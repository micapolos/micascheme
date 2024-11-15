(library (micac expanded)
  (export
    expanded expanded? expanded-env expanded-value
    expanded+
    expanded-ref
    expanded-gen
    expanded-transformer
    expanded-transform
    expanded-with
    expanded-map)
  (import
    (micascheme)
    (micac env))

  (data (expanded env value))

  (define (expanded+ $expanded $id $item)
    (expanded
      (env+ (expanded-env $expanded) $id $item)
      (expanded-value $expanded)))

  (define (expanded-ref $expanded $id)
    (env-ref (expanded-env $expanded) $id))

  (define (expanded-gen $env $id)
    (lets
      ((pair $env $identifier) (env-gen $env $id))
      (expanded $env $identifier)))

  (define (expanded-transformer $expanded $id)
    (env-transformer (expanded-env $expanded) $id))

  (define (expanded-transform $expanded $transformer $syntax)
    (env-transform (expanded-env $expanded) $transformer $syntax))

  (define-rule-syntax (expanded-with expanded-expr value)
    (expanded (expanded-env expanded-expr) value))

  (define-rules-syntax
    ((expanded-map (value expanded-expr) body)
      (lets
        ($expanded expanded-expr)
        (value (expanded-value $expanded))
        (expanded (expanded-env $expanded) body)))
    ((expanded-map decl decls ... body)
      (expanded-map decls ...
        (expanded-value (expanded-map decl body)))))
)
