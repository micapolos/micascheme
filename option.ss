(library (option)
  (export
    option
    option=?
    option->datum
    option-map
    option-fold
    option-fold?)
  (import
    (scheme)
    (syntax)
    (monad-syntax)
    (procedure)
    (boolean))

  (define (option $value)
    (or $value
      (error 'option "option can not be #f")))

  (define-pure (option $value) (option $value))

  (define-bind (option $fn $option)
    (and $option ($fn $option)))

  (define (option=? $=? $a $b)
    (if $a
      (and $b ($=? $a $b))
      (false? $b)))

  (define (option->datum $obj->datum $option)
    (and $option ($obj->datum $option)))

  (define (option-map $proc $option . $options)
    (cond
      ((and $option (for-all identity $options))
        (apply $proc $option $options))
      (else #f)))

  (define (option-fold $proc $initial $option . $options)
    (cond
      ($option
        (assert (for-all not-false? $options))
        (apply $proc $initial $option $options))
      (else
        (assert (for-all false? $options))
        $initial)))

  (define (option-fold? $proc $initial $option . $options)
    (cond
      ((and $option (for-all identity $options))
        (apply $proc $initial $option $options))
      ((and (false? $option) (for-all false? $options))
        $initial)
      (else #f)))
)
