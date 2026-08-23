(library (leo3 word)
  (export
    join-words
    word-split)
  (import
    (scheme)
    (list))

  (define (join-words $words)
    (map
      (partial))
    (splitp
      (partial string=? "to")
      $words)
    (switch $words
      ((null? $words) "")
      ((pair? $pair)
        (lets
          ((pair $car $cdr) $pair)
          (cond
            ((string=? $car "is") ))
    (fold-left
      (lambda ($folded $word)
        (cond
          ((string=? $word "is")
            (string-append $folded "->"))
          ((string=? $word "as")
            (string-append $folded "->"))
          (else )))
      ""
      $words))
)
