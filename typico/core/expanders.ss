(library (typico core expanders)
  (export
    core-expander
    check-expand-core
    check-expand-core-raises)
  (import
    (typico base)
    (typico expander)
    (typico typed)
    (typico type)
    (typico id)
    (typico core types)
    (asm u))

  (define-rule-syntax (check-expand-core in out)
    (check-expand core-expander in out))

  (define-rule-syntax (check-expand-core-raises in)
    (check-expand-raises core-expander in))

  (define core-expander
    (or-expander
      (predicate-expander boolean? boolean-type)
      (predicate-expander integer? integer-type)
      (predicate-expander char? char-type)
      (predicate-expander string? string-type)

      (case-expander (u8 x) ($recurse)
        (syntax-case (expand-inner-value $recurse integer-type #'x) ()
          (u8
            (u8? (datum u8))
            (typed u8-type (datum u8)))))

      (case-expander (if condition true false) ($recurse)
        (lets
          ($condition-value (expand-inner-value $recurse boolean-type #'condition))
          ((typed $type $true-value) (expand-inner $recurse #'true))
          ($false-value (expand-inner-value $recurse $type #'false))
          (typed $type
            (cond
              ((boolean? $condition-value) (if $condition-value $true-value $false-value))
              (else `(if ,$condition-value ,$true-value ,$false-value))))))

      (case-expander (dynamic x) ($recurse)
        (typed-map-value
          (lambda ($value) `(dynamic ,$value))
          (expand-inner $recurse #'x)))

      (case-expander (let ((id expr) ...) body) ($recurse)
        (lets
          ($ids #'(id ...))
          ($typed-list (map (partial expand-inner $recurse) #'(expr ...)))
          ($recurse
            (fold-left
              (lambda ($recurse $id $type)
                (lambda ($syntax)
                  (or
                    (and
                      (id? $syntax)
                      (id=? $syntax $id)
                      (typed $type (id->symbol $syntax)))
                    ($recurse $syntax))))
              $recurse
              $ids
              (map typed-type $typed-list)))
          ($typed-body (expand-inner $recurse #'body))
          (typed
            (typed-type $typed-body)
            `(let
              (,@(map list
                (map id->symbol $ids)
                (map typed-value $typed-list)))
              ,(typed-value $typed-body)))))

      (case-expander integer-zero (typed integer-type 0))
      (case-expander integer-one (typed integer-type 1))

      (function-expander (+ integer-type integer-type ...) integer-type    ($primitive 3 +))
      (function-expander (- integer-type integer-type ...) integer-type    ($primitive 3 -))
      (function-expander (append string-type string-type ...) string-type  ($primitive 3 string-append))

      (function-expander (string char-type ...) string-type                ($primitive 3 string))
      (function-expander (length string-type) integer-type                 ($primitive 3 string-length))

      (function-expander (and boolean-type boolean-type ...) boolean-type  and)
      (function-expander (or boolean-type boolean-type ...) boolean-type   or)

      (function-expander (and integer-type integer-type ...) integer-type  ($primitive 3 bitwise-and))
      (function-expander (or integer-type integer-type ...) integer-type   ($primitive 3 bitwise-ior))
      (function-expander (xor integer-type integer-type ...) integer-type  ($primitive 3 bitwise-xor))

      (function-expander (= boolean-type boolean-type) boolean-type        ($primitive 3 boolean=?))
      (function-expander (= integer-type integer-type) boolean-type        ($primitive 3 =))
      (function-expander (= string-type string-type) boolean-type          ($primitive 3 string=?))
      (function-expander (= char-type char-type) boolean-type              ($primitive 3 char=?))
      (function-expander (= bytevector-type bytevector-type) boolean-type  ($primitive 3 bytevector=?))))
)
