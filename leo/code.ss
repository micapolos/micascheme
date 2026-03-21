(library (leo code)
  (export lim+line-length?)
  (import (micascheme))

  (define (lim+line-length? $lim $line)
    (switch $line
      ((null? _) $lim)
      ((pair? $pair)
        (lets?
          ($lim (lim+line-length? $lim (car $pair)))
          (lim+line-length? $lim (cdr $pair))))
      ((bytevector? $bytevector)
        (lim+? $lim (+ (bytevector-length $bytevector) 1)))
      ((vector? $vector)
        (fold-left
          (lambda ($lim? $line)
            (and $lim?
              (lim+line-length? $lim? $line)))
          $lim
          (vector->list $vector)))
      ((else $other)
        (lim+? $lim 1))))
)
