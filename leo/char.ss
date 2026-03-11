(library (leo char)
  (export
    char-letter?
    char-digit?
    char-constituent?
    char-special-initial?
    char-special-subsequent?
    char-subsequent-category?)
  (import (micascheme))

  (define (char-letter? $char)
    (lets
      ($code (char->integer $char))
      (or
        (and
          (>= $code (char->integer #\a))
          (<= $code (char->integer #\z)))
        (and
          (>= $code (char->integer #\A))
          (<= $code (char->integer #\Z))))))

  (define char-digit? char-numeric?)

  (define constituent-char-categories
    '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))

  (define (char-constituent? $char)
    (or
      (char-letter? $char)
      (and
        (> (char->integer $char) 127)
        (memv (char-general-category $char) constituent-char-categories))))

  (define special-initial-chars
    (list #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))

  (define (char-special-initial? $char)
    (memv $char special-initial-chars))

  (define special-subsequent-chars
    (list #\+ #\- #\. #\@))

  (define (char-special-subsequent? $char)
    (memv $char special-subsequent-chars))

  (define subsequent-char-categories
    '(Nd Mc Me))

  (define (char-subsequent-category? $char)
    (memv (char-general-category $char) subsequent-char-categories))
)
