(library (typico core expanders)
  (export
    core-expander
    check-expand-core
    check-expand-core-type
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

  (define-case-syntax (check-expand-core-type in out)
    #`(check
      (equal?
        (expand-value core-expander type-type 'in)
        out)))

  (define-rule-syntax (check-expand-core-raises in)
    (check-expand-raises core-expander in))

  (define core-expander
    (or-expander
      ; primitive types
      (case-expander      boolean (typed type-type boolean-type))
      (case-expander      integer (typed type-type integer-type))
      (case-expander      char    (typed type-type char-type))
      (case-expander      string  (typed type-type string-type))
      (case-expander      datum   (typed type-type datum-type))

      ; function type
      (case-expander (function (param ... vararg-param dots) result) ($expander)
        (and
          (equal? (datum dots) '...)
          (for-all id? #'(param ... vararg-param))
          (typed type-type
            (function-type
              (append
                (map (partial expand-value $expander type-type) #'(param ...))
                (expand-value $expander type-type #'vararg-param))
              (expand-value $expander type-type #'result)))))

      (case-expander (function (param ...) result) ($expander)
        (and
          (for-all id? #'(param ...))
          (typed type-type
            (function-type
              (map (partial expand-value $expander type-type) #'(param ...))
              (expand-value $expander type-type #'result)))))

      ; lambda
      (case-expander (lambda ((type id) ... (vararg-type vararg-id) dots) body) ($expander)
        (and
          (equal? (datum dots) '...)
          (for-all id? #'(id ... vararg-id))
          (lets
            ($types (map (partial expand-value $expander type-type) #'(type ...)))
            ($vararg-type (expand-value $expander type-type #'vararg-type))
            ($expander
              (or-expander
                (list->expander (map id-expander #'(id ...) $types))
                ; TODO: list-of type
                (id-expander #'vararg-id $vararg-type)
                $expander))
            ($typed-body (expand $expander #'body))
            (typed
              (function-type (append $types $vararg-type) (typed-type $typed-body))
              `(lambda (,@(map id->symbol #'(id ...)) . ,(id->symbol #'vararg-id))
                ,(typed-value $typed-body))))))

      (case-expander (lambda ((type id) ...) body) ($expander)
        (and
          (for-all id? #'(id ...))
          (lets
            ($types (map (partial expand-value $expander type-type) #'(type ...)))
            ($expander
              (or-expander
                (list->expander (map id-expander #'(id ...) $types))
                $expander))
            ($typed-body (expand $expander #'body))
            (typed
              (function-type $types (typed-type $typed-body))
              `(lambda (,@(map id->symbol #'(id ...)))
                ,(typed-value $typed-body))))))

      (predicate-expander boolean? boolean-type)
      (predicate-expander (and? integer? exact?) integer-type)
      (predicate-expander char? char-type)
      (predicate-expander string? string-type)

      (case-expander (u8 x) ($expander)
        (syntax-case (expand-value $expander integer-type #'x) ()
          (u8
            (u8? (datum u8))
            (typed u8-type (datum u8)))))

      (case-expander (if condition true false) ($expander)
        (lets
          ($condition-value (expand-value $expander boolean-type #'condition))
          ((typed $type $true-value) (expand $expander #'true))
          ($false-value (expand-value $expander $type #'false))
          (typed $type
            (cond
              ((boolean? $condition-value) (if $condition-value $true-value $false-value))
              (else `(if ,$condition-value ,$true-value ,$false-value))))))

      (case-expander (dynamic x) ($expander)
        (typed-map-value
          (lambda ($value) `(dynamic ,$value))
          (expand $expander #'x)))

      (case-expander (let (id expr) ... body) ($expander)
        (and
          (for-all id? #'(id ...))
          (lets
            ($ids #'(id ...))
            ($typed-list (map (partial expand $expander) #'(expr ...)))
            ($expander
              (or-expander
                (list->expander (map id-expander $ids (map typed-type $typed-list)))
                $expander))
            ($typed-body (expand $expander #'body))
            (typed
              (typed-type $typed-body)
              `(let
                (,@(map list
                  (map id->symbol $ids)
                  (map typed-value $typed-list)))
                ,(typed-value $typed-body))))))

      (case-expander integer-zero (typed integer-type 0))
      (case-expander integer-one (typed integer-type 1))

      (datum-expander integer+      (function-type (list* integer-type) integer-type)               ($primitive 3 +))
      (datum-expander integer-      (function-type (list* integer-type integer-type) integer-type)  ($primitive 3 -))
      (datum-expander string-append (function-type (list* string-type) string-type)                 ($primitive 3 string-append))

      (datum-expander (+ integer-type integer-type ...)    (integer-type  ($primitive 3 +)))
      (datum-expander (- integer-type integer-type ...)    (integer-type  ($primitive 3 -)))

      (datum-expander (append string-type string-type ...) (string-type   ($primitive 3 string-append)))
      (datum-expander (string char-type ...)               (string-type   ($primitive 3 string)))
      (datum-expander (length string-type)                 (integer-type  ($primitive 3 string-length)))

      (datum-expander (and boolean-type boolean-type ...)  (boolean-type  and))
      (datum-expander (or boolean-type boolean-type ...)   (boolean-type  or))

      (datum-expander (and integer-type integer-type ...)  (integer-type  ($primitive 3 bitwise-and)))
      (datum-expander (or integer-type integer-type ...)   (integer-type  ($primitive 3 bitwise-ior)))
      (datum-expander (xor integer-type integer-type ...)  (integer-type  ($primitive 3 bitwise-xor)))

      (datum-expander (= boolean-type boolean-type)        (boolean-type  ($primitive 3 boolean=?)))
      (datum-expander (= integer-type integer-type)        (boolean-type  ($primitive 3 =)))
      (datum-expander (= string-type string-type)          (boolean-type  ($primitive 3 string=?)))
      (datum-expander (= char-type char-type)              (boolean-type  ($primitive 3 char=?)))
      (datum-expander (= bytevector-type bytevector-type)  (boolean-type  ($primitive 3 bytevector=?)))

      ; application (must be the last one)
      (expander ($expander $syntax)
        (syntax-case? $syntax ()
          ((fn arg ...)
            (lets
              ((typed $fn-type $fn-value) (expand-function $expander #'fn))
              ($arg-values
                (map*
                  (partial expand-value $expander)
                  (lambda ($type $args)
                    (map (partial expand-value $expander $type) $args))
                  (function-type-param-types $fn-type)
                  #'(arg ...)))
              (typed
                (function-type-result-type $fn-type)
                `(,$fn-value ,@$arg-values))))))))
)
