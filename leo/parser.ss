(library (leo parser)
  (export
    parser parse-error
    parser-push parser-finish
    parser parser-bind parser-map
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
    non-empty-stack-parser
    indent-parser

    parsed pure skip

    string-literal-char-parser
    literal-string-parser
  )

  (import (micascheme))

  ; ----------------------------------------------------------

  (define-aux-keyword parsed)
  (define-aux-keyword pure)
  (define-aux-keyword skip)

  ; ----------------------------------------------------------

  (data (thunk push-fn finish-fn))

  (define (thunk-push $thunk $char)
    ((thunk-push-fn $thunk) $char))

  (define (thunk-finish $thunk)
    ((thunk-finish-fn $thunk)))

  ; ----------------------------------------------------------

  (define-syntax-rule (parser-thunk-do ($thunk $parser) $body)
    (and-lets ($thunk $parser) $body))

  (define (parser-push $parser $char)
    (parser-thunk-do ($thunk $parser)
      (thunk-push $thunk $char)))

  (define (parser-finish $parser)
    (parser-thunk-do ($thunk $parser)
      (thunk-finish $thunk)))

  (define (parse-error) #f)

  (define (parser $value)
    (thunk 
      (lambda ($char) #f) 
      (lambda () $value)))

  (define (parser-bind $parser $fn)
    (parser-thunk-do ($thunk $parser)
      (thunk
        (lambda ($char)
          (lets
            ($push-parser (thunk-push $thunk $char))
            (if $push-parser
              (parser-bind $push-parser $fn)
              (parser-push
                (opt-lift $fn (thunk-finish $thunk))
                $char))))
        (lambda ()
          (parser-finish
            (opt-lift $fn
              (thunk-finish $thunk)))))))

  (define (parser-map $parser $fn)
    (parser-bind $parser 
      (lambda ($item) 
        (parser ($fn $item)))))

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
          #`(parser $body))
        ((_ $body)
          #`$body))))

  ; ----------------------------------------------------------

  (meta define (parser-identifier $id)
    (build-identifier ($string $id) 
      (string-append $string "-parser")))

  (define-syntax make-parser
    (lambda ($syntax)
      (syntax-case $syntax (skip pure parsed lets)
        ((_ (pure $body))
          #`$body)
        ((_ (parsed $body)) 
          #`(parser $body))
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
    (parser-finish
      (fold-left parser-push $parser (string->list $string))))

  ; ----------------------------------------------------------

  (define (make-char-parser $char-opt)
    (thunk
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
    (thunk
      (lambda ($char)
        (make-string-parser (push $char-stack $char)))
      (lambda ()
        (list->string (reverse $char-stack)))))

  (define (string-parser)
    (make-string-parser (stack)))

  ; ----------------------------------------------------------

  (define (make-exact-parser $string $index)
    (thunk
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
    (thunk
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
          (parser (- (char->integer $char) (char->integer #\0)))))))

  ; ----------------------------------------------------------

  (define (make-positive-integer-parser $digit-stack)
    (thunk
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
    (parser-lets
      ($digit-stack (non-empty-stack-parser (digit-parser)))
      (parser 
        (fold-left 
          (lambda ($integer $digit) (+ (* $integer 10) $digit))
          0
          (reverse $digit-stack)))))

  ; ----------------------------------------------------------

  (define (make-word-parser $letter-stack)
    (thunk
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
    (lets
      ($thunks (filter-opts $parsers))
      (case (length $thunks)
        ((0) #f)
        ((1) (car $thunks))
        (else 
          (thunk
            (lambda ($char)
              (make-oneof-parser
                (map 
                  (lambda ($thunk) 
                    (thunk-push $thunk $char))
                  $thunks)))
            (lambda ()
              (single 
                (filter-opts 
                  (map thunk-finish $thunks)))))))))

  (define-syntax-rule (oneof-parser $parser ...)
    (make-oneof-parser (list $parser ...)))

  ; ----------------------------------------------------------

  (define (fold-parser $folded $parser $fn)
    (oneof-parser
      (parser $folded)
      (parser-bind $parser
        (lambda ($item)
          (fold-parser ($fn $folded $item) $parser $fn)))))

  (define (stack-parser $parser)
    (fold-parser (stack) $parser push))

  (define (non-empty-stack-parser $parser)
    (parser-lets
      ($first $parser)
      (fold-parser (stack $first) $parser push)))

  ; ----------------------------------------------------------

  (define indent-size 2)

  (define (make-indent-parser $indent $parser)
    (thunk
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

  ; ----------------------------------------------------------

  (define (string-literal-char-parser)
    (parser-bind (char-parser)
      (lambda ($char)
        (case $char
          ((#\\) 
            (parser-bind (char-parser)
              (lambda ($char)
                (case $char
                  ((#\\) (parser #\\))
                  ((#\n) (parser #\newline))
                  ((#\t) (parser #\tab))
                  ((#\") (parser #\"))
                  (else #f)))))
          ((#\") #f)
          (else (parser $char))))))

  (define (literal-string-parser)
    (parser-bind (exact-parser "\"")
      (lambda (_)
        (parser-bind (stack-parser (string-literal-char-parser))
          (lambda ($char-stack)
            (parser-bind (exact-parser "\"")
              (lambda (_)
                (parser (list->string (reverse $char-stack))))))))))
    ; (parser-lets
    ;   (skip (exact-parser "\""))
    ;   ($char-stack (stack-parser (string-literal-char-parser)))
    ;   (skip (exact-parser "\""))
    ;   (parser (list->string (reverse $char-stack)))))
)