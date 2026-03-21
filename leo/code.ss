(library (leo code)
  (export lim+leo-length?)
  (import
    (micascheme)
    (leo datum)
    (code))

  (define (lim+leo-length? $lim $leo)
    (switch $leo
      ((null? _) $lim)
      ((pair? $pair)
        (lets?
          ($lim (lim+leo-length? $lim (car $pair)))
          (lim+leo-length? $lim (cdr $pair))))
      ((char? $char)
        (lim+? $lim 2))
      ((bytevector? $bytevector)
        (lim+? $lim (+ (bytevector-length $bytevector) 1)))
      ((vector? $vector)
        (fold-left
          (lambda ($lim? $leo)
            (and $lim?
              (lim+leo-length? $lim? $leo)))
          (lim+? $lim 1)
          (vector->list $vector)))
      ((else $other)
        (lim+? $lim 1))))

  (define (leo-code $leo)
    (switch $leo
      ((null? _) (leo-code null-datum))
      ((boolean? $boolean)
        (leo-code (boolean->datum $boolean)))
      ((char? $char)
        (leo-code (char->datum $char)))
      ((pair? $pair)
        (todo))
      ((bytevector? $bytevector)
        (leo-code (bytevector->datum $bytevector)))
      ((vector? $vector)
        (leo-code (vector->datum $vector)))
      ((else $other)
        (code (format "~s" $other)))))
)
