(library (leo record-type)
  (export
    record-type-getter)
  (import
    (except (chezscheme) let syntax-error)
    (only (rnrs) record-type-name)
    (lets)
    (switch)
    (list)
    (procedure)
    (leo syntax-error))

  (define (record-type-getter $rtd)
    (lambda ($id)
      (lets
        ($record-symbol (record-type-name $rtd))
        ($field-symbol (syntax->datum $id))
        (switch (find-index (partial symbol=? $field-symbol) (vector->list (record-type-field-names $rtd)))
          ((integer? $index)
            (record-accessor $rtd $index))
          ((else _)
            (syntax-error $id
              `(undefined (field (,$record-symbol ,$field-symbol)))))))))
)
