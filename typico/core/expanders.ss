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
    (typico fragment)
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

  (define (expand-list-of-type-type? $expander $syntax)
    (lets?
      ($type (expand-value? $expander type-type $syntax))
      (list-of-item? $type)))

  (define core-expander
    (or-expander
      ; primitive types
      (case-expander      boolean (typed type-type boolean-type))
      (case-expander      integer (typed type-type integer-type))
      (case-expander      char    (typed type-type char-type))
      (case-expander      string  (typed type-type string-type))
      (case-expander      datum   (typed type-type datum-type))

      ; syntax
      (case-expander (syntax expr) ($expander)
        (typed
          (syntax-type $expander #'expr)
          #f))

      ; eval syntax
      (case-expander (eval expr) ($expander)
        (lets?
          ($syntax-type (expand-syntax-type? $expander #'expr))
          (expand
            (syntax-type-expander $syntax-type)
            (syntax-type-datum $syntax-type))))

      ; function type
      (case-expander (-> param ... vararg-param dots result) ($expander)
        (and
          (equal? (datum dots) '...)
          (for-all id? #'(param ... vararg-param))
          (typed type-type
            (function-type
              (append
                (map (partial expand-value $expander type-type) #'(param ...))
                (expand-value $expander type-type #'vararg-param))
              (expand-value $expander type-type #'result)))))

      (case-expander (-> param ... result) ($expander)
        (and
          (for-all id? #'(param ...))
          (typed type-type
            (function-type
              (map (partial expand-value $expander type-type) #'(param ...))
              (expand-value $expander type-type #'result)))))

      ; list-of type
      (case-expander (list-of expr) ($expander)
        (typed type-type
          (list-of-type
            (expand-value $expander type-type #'expr))))

      ; unsafe type
      (case-expander (unsafe expr) ($expander)
        (typed type-type
          (unsafe-type
            (expand-value $expander type-type #'expr))))

      ; optional type
      (case-expander (optional expr) ($expander)
        (typed type-type
          (optional-type
            (expand-value $expander type-type #'expr))))

      ; type classes
      (case-expander unsafe ($expander) (typed type-type generic-unsafe-type))
      (case-expander optional ($expander) (typed type-type generic-optional-type))
      (case-expander list-of ($expander) (typed type-type generic-list-of-type))

      ; optional type
      (case-expander (optional expr) ($expander)
        (typed type-type
          (optional-type
            (expand-value $expander type-type #'expr))))

      ; lambda
      (case-expander (=> (id type) ... (vararg-id vararg-type) dots body) ($expander)
        (and
          (equal? (datum dots) '...)
          (for-all id? #'(id ... vararg-id))
          (lets
            ($ids #'(id ...))
            ($vararg-id #'vararg-id)
            ($types (map (partial expand-value $expander type-type) #'(type ...)))
            ($vararg-type (expand-value $expander type-type #'vararg-type))
            ($expander
              (or-expander
                (list->expander (map id-expander $ids $types))
                (id-expander #'vararg-id (list-of-type $vararg-type))
                $expander))
            ($typed-body (expand $expander #'body))
            (typed
              (function-type (append $types $vararg-type) (typed-type $typed-body))
              (fragment-bind-with
                ($lambda (fragment (import (scheme)) lambda))
                ($body (typed-value $typed-body))
                (pure-fragment
                  `(,$lambda (,@(map id->symbol $ids) . ,(id->symbol $vararg-id))
                    ,$body)))))))

      (case-expander (=> (id type) ... body) ($expander)
        (and
          (for-all id? #'(id ...))
          (lets
            ($ids #'(id ...))
            ($types (map (partial expand-value $expander type-type) #'(type ...)))
            ($expander
              (or-expander
                (list->expander (map id-expander $ids $types))
                $expander))
            ($typed-body (expand $expander #'body))
            (typed
              (function-type $types (typed-type $typed-body))
              (fragment-bind-with
                ($lambda (fragment (import (scheme)) lambda))
                ($body (typed-value $typed-body))
                (pure-fragment `(,$lambda (,@(map id->symbol $ids)) ,$body)))))))

      (predicate-expander boolean? boolean-type)
      (predicate-expander (and? integer? exact?) integer-type)
      (predicate-expander char? char-type)
      (predicate-expander string? string-type)

      (case-expander (u8 x) ($expander)
        (lets
          ($fragment (expand-value $expander integer-type #'x))
          (switch? (fragment-obj $fragment)
            ((u8? $u8) (typed u8-type $fragment)))))

      (case-expander (if condition true false) ($expander)
        (lets
          ($condition-fragment (expand-value $expander boolean-type #'condition))
          ((typed $type $true-fragment) (expand $expander #'true))
          ($false-fragment (expand-value $expander $type #'false))
          (typed $type
            (fragment-bind-with
              ($if-value (fragment (import (scheme)) if))
              ($condition-value $condition-fragment)
              ($true-value $true-fragment)
              ($false-value $false-fragment)
              (pure-fragment `(,$if-value ,$condition-value ,$true-value ,$false-value))))))

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
              (fragment-bind-with
                ($let (fragment (import (scheme)) let))
                ($values (list->fragment (map typed-value $typed-list)))
                ($body (typed-value $typed-body))
                (pure-fragment
                  `(let
                    (,@(map list
                      (map id->symbol $ids)
                      $values))
                    ,$body)))))))

      (macro-expander (lets) (lets item) item)
      (macro-expander (lets) (lets item item* ...) (let item (lets item* ...)))

      (case-expander integer-zero (typed integer-type 0))
      (case-expander integer-one (typed integer-type 1))

      (primitive-expander integer+      (function-type (list* integer-type) integer-type)               +)
      (primitive-expander integer-      (function-type (list* integer-type integer-type) integer-type)  -)
      (primitive-expander string-append (function-type (list* string-type) string-type)                 string-append)

      (datum-expander (+ integer-type integer-type ... integer-type)     (fragment (import (scheme)) ($primitive 3 +)))
      (datum-expander (- integer-type integer-type ... integer-type)     (fragment (import (scheme)) ($primitive 3 -)))

      (datum-expander (append string-type string-type ... string-type)   (fragment (import (scheme)) ($primitive 3 string-append)))
      (datum-expander (length string-type integer-type)                  (fragment (import (scheme)) ($primitive 3 string-length)))

      (datum-expander (string char-type ... string-type)                 (fragment (import (scheme)) ($primitive 3 string)))
      (datum-expander (string integer-type string-type)                  (fragment (import (scheme)) ($primitive 3 number->string)))

      (datum-expander (and boolean-type boolean-type ... boolean-type)   (fragment (import (scheme)) and))
      (datum-expander (or boolean-type boolean-type ... boolean-type)    (fragment (import (scheme)) or))

      (datum-expander (and integer-type integer-type ... integer-type)   (fragment (import (scheme)) ($primitive 3 bitwise-and)))
      (datum-expander (or integer-type integer-type ... integer-type)    (fragment (import (scheme)) ($primitive 3 bitwise-ior)))
      (datum-expander (xor integer-type integer-type ... integer-type)   (fragment (import (scheme)) ($primitive 3 bitwise-xor)))

      (datum-expander (= boolean-type boolean-type boolean-type)         (fragment (import (scheme)) ($primitive 3 boolean=?)))
      (datum-expander (= integer-type integer-type boolean-type)         (fragment (import (scheme)) ($primitive 3 =)))
      (datum-expander (= string-type string-type boolean-type)           (fragment (import (scheme)) ($primitive 3 string=?)))
      (datum-expander (= char-type char-type boolean-type)               (fragment (import (scheme)) ($primitive 3 char=?)))
      (datum-expander (= bytevector-type bytevector-type boolean-type)   (fragment (import (scheme)) ($primitive 3 bytevector=?)))

      (macro-expander (cond else) (cond (else x)) x)
      (macro-expander (cond) (cond (condition body) rest ...)
        (if condition body (cond rest ...)))

      ; list

      (case-expander (empty expr) ($expander)
        (lets?
          ($type (expand-list-of-type-type? $expander #'expr))
          (typed
            (list-of-type $type)
            (pure-fragment ''()))))

      (case-expander (list head tail) ($expander)
        (lets
          ($typed-head (expand $expander #'head))
          ($type (typed-type $typed-head))
          (lets?
            ($tail-fragment (expand-value? $expander (list-of-type $type) #'tail))
            (typed
              (list-of-type $type)
              (fragment-bind-with
                ($cons (fragment (import (scheme)) ($primitive 3 cons)))
                ($head (typed-value $typed-head))
                ($tail $tail-fragment)
                (pure-fragment `(,$cons ,$head ,$tail)))))))

      (case-expander (list head xs ...) ($expander)
        (lets
          ($typed-head (expand $expander #'head))
          ($type (typed-type $typed-head))
          ($xs-fragments (map (partial expand-value? $expander $type) #'(xs ...)))
          (and
            (for-all identity $xs-fragments)
            (typed
              (list-of-type $type)
              (fragment-bind-with
                ($list (fragment (import (scheme)) ($primitive 3 list)))
                ($head (typed-value $typed-head))
                ($xs (list->fragment $xs-fragments))
                (pure-fragment `(,$list ,$head ,@$xs)))))))

      (case-expander (length expr) ($expander)
        (lets
          ($typed-expr (expand $expander #'expr))
          (lets?
            ($type (list-of-item? (typed-type $typed-expr)))
            (typed integer-type
              (fragment-bind-with
                ($length (fragment (import (scheme)) ($primitive 3 length)))
                ($list (typed-value $typed-expr))
                (pure-fragment `(,$length ,$list)))))))

      ; pure (HARDCODED!!!)

      (case-expander (pure type expr) ($expander)
        (lets?
          ($type (expand-value? $expander type-type #'type))
          ($typed (expand $expander #'expr))
          (cond
            ((type=? $type generic-unsafe-type)
              (typed
                (unsafe-type (typed-type $typed))
                (typed-value $typed)))
            ((type=? $type generic-optional-type)
              (typed
                (optional-type (typed-type $typed))
                (typed-value $typed)))
            ((type=? $type generic-list-of-type)
              (typed
                (list-of-type (typed-type $typed))
                (fragment-bind-with
                  ($list (fragment (import (scheme)) list))
                  ($value (typed-value $typed))
                  (pure-fragment `(,$list ,$value))))))))

      ; application (must be the last one)
      (expander ($expander $syntax)
        (syntax-case? $syntax ()
          ((fn arg ...)
            (lets
              ((typed $fn-type $fn-fragment) (expand-function $expander #'fn))
              ($arg-fragments
                (map*
                  (partial expand-value $expander)
                  (lambda ($type $args)
                    (map (partial expand-value $expander $type) $args))
                  (function-type-param-types $fn-type)
                  #'(arg ...)))
              (typed
                (function-type-result-type $fn-type)
                (fragment-bind-with
                  ($fn $fn-fragment)
                  ($args (list->fragment $arg-fragments))
                  (pure-fragment `(,$fn ,@$args))))))))))
)
