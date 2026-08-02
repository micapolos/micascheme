(library (tt list)
  (export list null link unlink length make-list list=?)
  (import
    (tt lang)
    (tt datum)
    (prefix (scheme) %)
    (prefix (list) %)
    (only (scheme) syntax quasisyntax unsyntax unsyntax-splicing ...))

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

  (define-syntax make-list
    (%lambda ($syntax)
      (%syntax-case $syntax ()
        ((_ x ...)
          (%fold-right
            (%lambda ($x $y)
              #`(link #,$x #,$y))
            #'null
            #'(x ...))))))

  (define length
    (unchecked
      (forall x (pi ((list x)) number))
      %length))

  (define list=?
    (unchecked
      (forall x (pi ((pi (x x) boolean) (list x) (list x)) boolean))
      %for-all*))
)
