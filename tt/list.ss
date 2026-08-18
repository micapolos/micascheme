(library (tt list)
  (export
    list null link unlink length list=?
    map fold push intercalate reverse)
  (import
    (tt lang)
    (prefix (scheme) %)
    (prefix (list) %)
    (prefix (stack) %)
    (prefix (tt compiler) %)
    (only (scheme) syntax quasisyntax unsyntax unsyntax-splicing))

  (define-class (list _))

  (define list (unchecked (lambda (x) (pi (x ...) (list x))) %list))
  (define length (unchecked (lambda (x) (pi ((list x)) number)) %length))
  (define null (unchecked (lambda (x) (list x)) %null))
  (define link (unchecked (lambda (x) (pi (x (list x)) (list x))) %cons))
  (define push (unchecked (lambda (x) (pi ((list x) x) (list x))) %push))
  (define intercalate (unchecked (lambda (x) (pi ((list x) x) (list x))) %intercalate))
  (define reverse (unchecked (lambda (x) (pi ((list x)) (list x))) %reverse))

  (define unlink
    (unchecked
      (lambda (a b) (pi ((list a) (pi () b) (pi (a (list a)) b)) b))
      %unlink))

  (define list=?
    (unchecked
      (lambda (x) (pi ((pi (x x) boolean) (list x) (list x)) boolean))
      %for-all*))

  (define map
    (unchecked
      (lambda (a b) (pi ((pi (a) b) (list a)) (list b)))
      %map))

  (define fold
    (unchecked
      (lambda (a b) (pi ((pi (a b) a) a (list b)) a))
      %fold-left))
)
