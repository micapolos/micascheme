(library (tt list)
  (export
    list null link unlink length list=?
    map fold push intercalate)
  (import
    (tt lang)
    (prefix (scheme) %)
    (prefix (list) %)
    (prefix (stack) %)
    (prefix (tt hoas-compiler) %)
    (only (scheme) syntax quasisyntax unsyntax unsyntax-splicing))

  (define-class (list _))

  (define null
    (unchecked
      (forall x (list x))
      (%quote ())))

  (define link
    (unchecked
      (forall x (pi (x (list x)) (list x)))
      %cons))

  (define push
    (unchecked
      (forall x (pi ((list x) x) (list x)))
      %push))

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

  (define list
    (unchecked
      (forall x (pi (x ...) (list x)))
      %list))

  (define length
    (unchecked
      (forall x (pi ((list x)) number))
      %length))

  (define list=?
    (unchecked
      (forall x (pi ((pi (x x) boolean) (list x) (list x)) boolean))
      %for-all*))

  (define map
    (unchecked
      (forall a b (pi ((pi (a) b) (list a)) (list b)))
      %map))

  (define fold
    (unchecked
      (forall folded element (pi ((pi (folded element) folded) folded (list element)) folded))
      %fold-left))

  (define intercalate
    (unchecked
      (forall x (pi ((list x) x) (list x)))
      %intercalate))
)
