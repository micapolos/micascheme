(library (leo2 code)
  (export
    datum-length-max?

    atom?
    phrase?

    atom->code
    phrase->code

    check-atom->code=?
    check-phrase->code=?)
  (import
    (leo2 base)
    (code))

  (define max-line-atom-count 7)
  (define max-atom-string-length 14)

  (define (datum-length-max? $x $max)
    (switch-exhaustive $x
      ((atom? _)
        (and (>= $max 1) 1))
      ((pair? $pair)
        (switch (cdr $pair)
          ((null? _)
            (and (>= $max 1) 1))
          ((else $tail)
            (switch? (datum-length-max? (car $pair) $max)
              ((number? $car-length)
                (switch? (datum-length-max? $tail (- $max $car-length))
                  ((number? $number)
                    (lets
                      ($length (+ $car-length $number))
                      (and (>= $max $length) $length)))))))))))

  (define (atom-count x)
    (switch-exhaustive x
      ((atom? _) 1)
      ((pair? $pair)
        (+ (atom-count (cdr $pair)) 1))))

  (define (atom? x)
    (switch-exhaustive x
      ((null? _) #t)
      ((boolean? _) #t)
      ((number? _) #t)
      ((char? _) #t)
      ((symbol? _) #t)
      ((string? s)
        (<=
          (string-length s)
          max-atom-string-length))
      ((pair? _) #f)))

  (define (phrase? x)
    (switch-exhaustive x
      ((atom? _) #t)
      ((pair? $pair)
        (and
          (length<= $pair max-line-atom-count)
          (for-all atom? $pair)))))

  (define (atom->code $atom)
    (string-code
      (format "~s" $atom)))

  (define (phrase->code $phrase)
    (switch $phrase
      ((atom? $atom)
        (atom->code $atom))
      ((pair? $pair)
        (list->separated-code
          (code " ")
          (map phrase-element->code $pair)))))

  (define (phrase-element->code $x)
    (switch-exhaustive $x
      ((atom? $atom)
        (atom->code $atom))
      ((pair? $pair)
        (code-in-round-brackets
          (phrase->code $pair)))))

  ; (define (script->code $script)
  ;   (switch-exhaustive $script
  ;     ((null? _) "")
  ;     ((atom? $atom)
  ;       (newline-ended-code (atom->code $atom)))
  ;     ((pair? $pair)
  ;       (newline-ended-list->separated-code
  ;         (code ", ")
  ;         (map line-element->code $pair)))))

  (define (script-element->code? $max $script-element)
    (and
      (positive? $max)
      (switch-exhaustive $script-element
        ((atom? $atom)
          (atom->code $atom))
        ((pair? $pair)
          (phrase->code $pair)))))

  ; (define (leo->code $x)
  ;   (switch $x
  ;     ((atom? $atom)
  ;       (newline-ended-code (atom->code $atom)))
  ;     ((else $list)
  ;       (newline-separated-code
  ;         (map block->code $list)))))

  (define-rules-syntaxes
    ((check-atom->code=? in out)
      (check-code=? (atom->code in) out))
    ((check-phrase->code=? in out)
      (check-code=? (phrase->code in) out)))
)
