(library (leo parser)
  (export
    parser parser? parser parser-push-fn parser-finish-fn
    parser-push parser-finish
    parser-with parser-bind parser-map
    parser-lets
    parse

    make-parser
    define-parser

    char-parser
    string-parser
    digit-parser
    exact-parser
    line-parser
    positive-integer-parser
    word-parser
    oneof-parser
    fold-parser
    stack-parser
    indent-parser

    parsed skip
  )

  (import (micascheme))

  ; ----------------------------------------------------------

  (define-aux-keyword parsed)
  (define-aux-keyword skip)

  (data (parser push-fn finish-fn))

  (define (parser-push $parser $char)
    ((parser-push-fn $parser) $char))

  (define (parser-finish $parser)
    ((parser-finish-fn $parser)))

  (define (parser-with $value)
    (parser 
      (lambda ($char) #f) 
      (lambda () $value)))

  (define (parser-bind $parser $fn)
    (parser
      (lambda ($char)
        (lets
          ($push-parser (parser-push $parser $char))
          (if $push-parser
            (parser-bind $push-parser $fn)
            (opt-lift parser-push
              (opt-lift $fn (parser-finish $parser))
              $char))))
      (lambda ()
        (opt-lift parser-finish
          (opt-lift $fn 
            (parser-finish $parser))))))

  (define (parser-map $parser $fn)
    (parser-bind $parser 
      (lambda ($item) 
        (parser-with ($fn $item)))))

  ; ----------------------------------------------------------

  (define-syntax parser-lets
    (lambda ($syntax)
      (syntax-case $syntax (skip parser)
        ((_ (skip $expr) $decl ... $body)
          #`(parser-bind $expr
            (lambda (_)
              (parser-lets $decl ... $body))))
        ((_ ($var (parser $expr)) $decl ... $body)
          #`(lets ($var $expr)
            (parser-lets $decl ... $body)))
        ((_ ($var $expr) $decl ... $body)
          #`(parser-bind $expr
            (lambda ($var)
              (parser-lets $decl ... $body))))
        ((_ (parser $body)) 
          #`(parser-with $body))
        ((_ $body)
          #`$body))))

  ; ----------------------------------------------------------

  (meta define (parser-identifier $id)
    (build-identifier ($string $id) 
      (string-append $string "-parser")))

  (define-syntax make-parser
    (lambda ($syntax)
      (syntax-case $syntax (skip parsed lets)
        ((_ (parsed $body)) 
          #`(parser-with $body))
        ((_ (lets $body))
          #`(make-parser $body))
        ((_ (lets (skip $expr) $decl ... $body))
          #`(parser-bind 
            (make-parser $expr)
            (lambda (_)
              (make-parser (lets $decl ... $body)))))
        ((_ (lets ($var $expr) $decl ... $body))
          #`(parser-bind
            (make-parser $expr)
            (lambda ($var)
              (make-parser (lets $decl ... $body)))))
        ((_ ($id $arg ...)) (identifier? #`$id)
          #`(
            #,(parser-identifier #`$id)
            #,@(map
              (lambda ($arg) #`(make-parser #,$arg))
              (syntax->list #`($arg ...)))))
        ((_ $id) (identifier? #`$id)
          #`(make-parser ($id)))
        ((_ $constant)
          (switch (syntax->datum #`$constant)
            ((string? $string) #`(exact-parser (quote #,$string)))
            ((else $other) $other))))))

  (define-syntax define-parser
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($name $arg ...) $body) (identifier? #`$name)
          #`(define (#,(parser-identifier #`$name) $arg ...) 
            (make-parser $body)))
        ((_ $name $body)
          #`(define-parser ($name) $body)))))

  ; ----------------------------------------------------------

  (define (parse $parser $string)
    (opt-lift parser-finish
      (fold-left
        (lambda ($parser-opt $char)
          (opt-lift parser-push $parser-opt (opt $char)))
        $parser
        (string->list $string))))

  ; ----------------------------------------------------------

  (define (make-char-parser $char-opt)
    (parser
      (lambda ($char) 
        (and 
          (not $char-opt) 
          (make-char-parser $char)))
      (lambda () 
        $char-opt)))

  (define (char-parser)
    (make-char-parser #f))

  ; ----------------------------------------------------------

  (define (make-string-parser $char-stack)
    (parser
      (lambda ($char)
        (make-string-parser (push $char-stack $char)))
      (lambda ()
        (list->string (reverse $char-stack)))))

  (define (string-parser)
    (make-string-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-exact-parser $string $index)
    (parser
      (lambda ($char)
        (and 
          (< $index (string-length $string))
          (char=? $char (string-ref $string $index))
          (make-exact-parser $string (+ $index 1))))
      (lambda ()
        (and
          (= $index (string-length $string))
          $string))))

  (define (exact-parser $string)
    (make-exact-parser $string 0))

  ; ----------------------------------------------------------

  (define (make-line-parser $char-stack-or-line)
    (parser
      (lambda ($char)
        (and (not (string? $char-stack-or-line))
          (make-line-parser
            (case $char
              ((#\newline) (list->string (reverse $char-stack-or-line)))
              (else (push $char-stack-or-line $char))))))
      (lambda ()
        (and (string? $char-stack-or-line) $char-stack-or-line))))

  (define (line-parser)
    (make-line-parser (stack)))

  ; ----------------------------------------------------------

  (define (digit-parser)
    (parser-bind (char-parser)
      (lambda ($char)
        (and
          (char-numeric? $char)
          (parser-with (- (char->integer $char) (char->integer #\0)))))))

  ; ----------------------------------------------------------

  (define (make-positive-integer-parser $digit-stack)
    (parser
      (lambda ($char)
        (and
          (char-numeric? $char)
          (make-positive-integer-parser 
            (push 
              $digit-stack
              (- (char->integer $char) (char->integer #\0))))))
      (lambda () 
        (and
          (not (null? $digit-stack))
          (fold-left 
            (lambda ($integer $digit) (+ (* $integer 10) $digit))
            0
            (reverse $digit-stack))))))

  (define (positive-integer-parser)
    (make-positive-integer-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-word-parser $letter-stack)
    (parser
      (lambda ($char)
        (and
          (char-alphabetic? $char)
          (make-word-parser (push $letter-stack $char))))
      (lambda () 
        (and
          (not (null? $letter-stack))
          (string->symbol (list->string (reverse $letter-stack)))))))

  (define (word-parser)
    (make-word-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-oneof-parser $parsers)
    (and (not (null? $parsers))
      (parser
        (lambda ($char)
          (make-oneof-parser
            (filter-opts
              (map
                (lambda ($parser) (parser-push $parser $char))
                $parsers))))
        (lambda ()
          (single (filter-opts (map parser-finish $parsers)))))))

  (define-syntax-rule (oneof-parser $parser ...)
    (make-oneof-parser (list $parser ...)))

  ; ----------------------------------------------------------

  (define (fold-parser $folded $parser $fn)
    (oneof-parser
      (parser-with $folded)
      (parser-bind $parser
        (lambda ($item)
          (fold-parser ($fn $folded $item) $parser $fn)))))

  (define (stack-parser $parser)
    (fold-parser (stack) $parser push))

  ; ----------------------------------------------------------

  (define indent-size 2)

  (define (make-indent-parser $indent $parser)
    (parser
      (lambda ($char)
        (case $char
          ((#\space)
            (if (< $indent indent-size)
              (make-indent-parser (+ $indent 1) $parser)
              (opt-lift make-indent-parser 
                (opt $indent)
                (parser-push $parser $char))))
          ((#\newline)
            (and 
              (or (= $indent 0) (= $indent indent-size)) 0)
              (opt-lift make-indent-parser 0 (parser-push $parser $char)))
          (else 
            (and
              (= $indent indent-size)
              (opt-lift make-indent-parser
                (opt $indent)
                (parser-push $parser $char))))))
      (lambda ()
        (and 
          (zero? $indent)
          (parser-finish $parser)))))

  (define (indent-parser $parser)
    (make-indent-parser 0 $parser))
)