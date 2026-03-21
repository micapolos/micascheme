(library (leo code)
  (export
    limited-length+?
    limited-length+leo?
    limited-length+leos?)
  (import
    (micascheme)
    (leo datum)
    (code))

  (define (limited-length+? $limited-length $number)
    (make-limited?
      (+ (limited-ref $limited-length) $number)
      (- (limited-limit $limited-length) $number)))

  (define (limited-length+leo? $limited-length $leo)
    (switch $leo
      ((null? _) $limited-length)
      ((pair? $pair)
        (lets?
          ($limited-length (limited-length+leo? $limited-length (car $pair)))
          (limited-length+leo? $limited-length (cdr $pair))))
      ((char? $char)
        (limited-length+? $limited-length 2))
      ((bytevector? $bytevector)
        (limited-length+? $limited-length
          (+ (bytevector-length $bytevector) 1)))
      ((vector? $vector)
        (lets?
          ($limited-length (limited-length+? $limited-length 1))
          (limited-length+leos? $limited-length (vector->list $vector))))
      ((else $other)
        (limited-length+? $limited-length 1))))

  (define (limited-length+leos? $limited-length $leos)
    (fold-left
      (lambda ($limited-length? $leo)
        (and $limited-length?
          (limited-length+leo? $limited-length? $leo)))
      $limited-length
      $leos))

  ; (define (inline-limited-code? $leo $limit)
  ;   (switch $leo
  ;     ((null? _)
  ;       (make-limited?
  ;         (code null-datum)
  ;         1))
  ;     ((boolean? $boolean)
  ;       (leo-code (boolean->datum $boolean)))
  ;     ((char? $char)
  ;       (leo-code (char->datum $char)))
  ;     ((pair? $pair)
  ;       (switch $pair)
  ;       (todo))
  ;     ((bytevector? $bytevector)
  ;       (leo-code (bytevector->datum $bytevector)))
  ;     ((vector? $vector)
  ;       (leo-code (vector->datum $vector)))
  ;     ((else $other)
  ;       (code (format "~s" $other)))))
)
