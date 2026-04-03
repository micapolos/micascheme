(library (leo record-type)
  (export
    record-field-index
    record-type-getter)
  (import
    (except (chezscheme) let syntax-error)
    (only (rnrs) record-type-name)
    (lets)
    (switch)
    (list)
    (procedure)
    (leo syntax-error))

  (define (record-field-index $rtd $field-id)
    (lets
      ($record-symbol (record-type-name $rtd))
      ($field-symbol (syntax->datum $field-id))
      (switch (find-index (partial symbol=? $field-symbol) (vector->list (record-type-field-names $rtd)))
        ((integer? $index) $index)
        ((else _)
          (syntax-error $field-id
            `(undefined (field (,$record-symbol ,$field-symbol))))))))

  (define (record-type-getter $rtd)
    (lambda ($id)
      (record-accessor $rtd (record-field-index $rtd $id))))
)
