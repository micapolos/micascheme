(library (leo record-type)
  (export
    record-field-index
    record-type-getter
    record-type-setter!)
  (import
    (except (chezscheme) let syntax-error)
    (only (rnrs) record-type-name)
    (lets)
    (switch)
    (list)
    (procedure)
    (leo syntax-error))

  (define (record-field-index $rtd $field-identifier)
    (lets
      ($record-symbol (record-type-name $rtd))
      ($field-symbol (syntax->datum $field-identifier))
      (switch (find-index (partial symbol=? $field-symbol) (vector->list (record-type-field-names $rtd)))
        ((integer? $index) $index)
        ((else _)
          (syntax-error $field-identifier
            `(undefined (field (,$record-symbol ,$field-symbol))))))))

  (define (record-type-getter $rtd)
    (lambda ($identifier)
      (record-accessor $rtd (record-field-index $rtd $identifier))))

  (define (record-type-setter! $rtd)
    (lambda ($identifier)
      (record-mutator $rtd (record-field-index $rtd $identifier))))
)
