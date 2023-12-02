(library (tico expression)
  (export
    lambda-expression
    apply-expression
    let-expression

    tuple-expression
    tuple-ref-expression

    selector-expression
    switch-expression)
  (import (micascheme))

  (function (lambda-expression $arity $fn)
    (lets
      ($params (generate-symbols $arity))
      `(lambda (,@$params)
        ,($fn $params))))

  (function (apply-expression $target $args)
    `(,$target ,@$args))

  (function (let-expression $expressions $fn)
    (lets
      ($params (generate-symbols (length $expressions)))
      `(let
        (,@(map
          (lambda ($param $expression) `(,$param ,$expression))
          $params $expressions))
        ,($fn $params))))

  ; --- tuple packing / unpacking ---

  (function (tuple-expression $expressions)
    (case (length $expressions)
      ((0) #f)
      ((1) (car $expressions))
      ((2) `(cons ,(car $expressions) ,(cadr $expressions)))
      (else `(vector ,@$expressions))))

  (function (tuple-ref-expression $arity $tuple $index)
    (case $arity
      ((0) `(throw error))
      ((1) $tuple)
      ((2) `(,(if (zero? $index) `car `cdr) ,$tuple))
      (else `(vector-ref ,$tuple ,$index))))

  ; --- selector / switch ---

  (function (selector-expression $arity $index)
    (case $arity
      ((0) `(throw error))
      ((1) #f)
      ((2) (zero? $index))
      (else $index)))

  (function (switch-expression $selector $expressions)
    (case (length $expressions)
      ((0) `(throw error))
      ((1) (car $expressions))
      ((2) `(if ,$selector ,@$expressions))
      (else `(index-switch ,$selector ,@$expressions))))
)
