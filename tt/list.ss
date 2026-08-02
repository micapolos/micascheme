(library (tt list)
  (export list null link unlink length)
  (import
    (tt lang)
    (tt datum)
    (prefix (scheme) %))

  (define-class (list _))

  (define null
    (unchecked
      (forall x (list x))
      (%quote ())))

  (define link
    (unchecked
      (forall x (pi (x (list x)) (list x)))
      %cons))

  (define unlink
    (unchecked
      (forall element result
        (pi
          (
            (list element)
            (pi () result)
            (pi (element (list element)) result))
          result))
      (%lambda ($list $null-proc $link-proc)
        (%cond
          ((%null? $list) ($null-proc))
          (%else ($link-proc (%car $list) (%cdr $list)))))))

  (define length
    (unchecked
      (forall x (pi ((list x)) number))
      %length))
)
