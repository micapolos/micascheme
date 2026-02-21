(library (leo2 code)
  (export
    atom?
    phrase?

    atom->code
    phrase->code
    line->code
    inline->code

    check-atom->code=?
    check-phrase->code=?
    check-line->code=?
    check-inline->code=?)
  (import
    (leo2 base)
    (code))

  (define max-line-atom-count 7)
  (define max-atom-string-length 14)

  (define (atom-count x)
    (switch x
      ((atom? _) 1)
      ((else pair)
        (+
          (atom-count (car pair))
          (atom-count (cdr pair))))))

  (define (atom? x)
    (switch? x
      ((boolean? _) #t)
      ((number? _) #t)
      ((char? _) #t)
      ((symbol? _) #t)
      ((string? s)
        (<=
          (string-length s)
          max-atom-string-length))))

  (define (phrase? x)
    (switch? x
      ((null? _) #f)
      ((list? list)
        (and
          (length<= list max-line-atom-count)
          (for-all atom? list)))))

  (define (sentence? $x)
    (and
      (list? $x)
      (not (null? $x))
      (for-all atom? $x)))

  (define (atom->code $atom)
    (string-code
      (format "~s" $atom)))

  (define (phrase->code $phrase)
    (switch-exhaustive $phrase
      ((null? $phrase)
        (code "()"))
      ((list? $list)
        (list->separated-code
          (code " ")
          (map atom->code $list)))
      ((atom? $atom)
        (atom->code $atom))))

  (define (line->code $phrase)
    (switch $phrase
      ((atom? $atom)
        (atom->code $atom))
      ((else $list)
        (list->separated-code
          (code ", ")
          (map inline->code $list)))))

  (define (inline->code $x)
    (switch $x
      ((atom? $atom)
        (atom->code $atom))
      ((else $list)
        (code-in-round-brackets
          (list->separated-code
            (code " ")
            (map inline->code $list))))))

  (define (block->code $x)
    (switch $x
      ((atom? $atom)
        (newline-ended-code (atom->code $atom)))
      ((else $list)
        (newline-separated-code
          (map inline->code $list)))))

  (define (leo->code $x)
    (switch $x
      ((atom? $atom)
        (newline-ended-code (atom->code $atom)))
      ((else $list)
        (newline-separated-code
          (map block->code $list)))))

  (define-rules-syntaxes
    ((check-atom->code=? in out)
      (check-code=? (atom->code in) out))
    ((check-phrase->code=? in out)
      (check-code=? (phrase->code in) out))
    ((check-line->code=? in out)
      (check-code=? (line->code in) out))
    ((check-inline->code=? in out)
      (check-code=? (inline->code in) out)))
)
