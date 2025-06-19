(library (syntax)
  (export
    identifiers?
    null-syntax
    syntax-null?
    define-rule-syntax
    define-case-syntax
    define-keyword
    define-keywords
    unbegin-syntaxes
    unbegin-syntax
    syntaxes->syntax
    expand-begin-syntaxes
    define-lookup-syntax
    syntax-selector
    syntax-pattern-id
    syntax-rule-id
    syntax-clause-id
    syntax-case-opt
    syntax-eval
    quasisyntax-eval
    inline-indexed
    fenders implicit
    syntax-rule->clause
    syntax->datum/annotation
    bytevector->syntax
    vector->syntax
    syntax?
    syntax=?
    syntax-datum=?
    syntax-replace
    syntax-replace...
    transform
    syntaxes
    syntax-subst
    syntax-case?
    syntax-cons
    syntax-car
    syntax-cdr
    syntax-ref?
    syntax-ref
    syntax-ref*
    syntax-set
    syntax-update
    syntax-remove
    syntax-add
    syntax-false?
    syntax-and
    syntax-or
    list->syntax
    syntax-append
    literal->syntax
    syntax-single
    get-property
    syntax->list*)
  (import (scheme) (syntax-keywords))

  (define (identifiers? $syntax)
    (for-all identifier? (syntax->list $syntax)))

  (define (null-syntax) #'())

  (define (syntax-null? $syntax)
    (null? (syntax->datum $syntax)))

  (define (unbegin-syntaxes $syntax)
    (syntax-case $syntax (begin)
      ((begin $syntax ...)
        (syntax->list #'($syntax ...)))
      ($other
        (list #'$other))))

  (define (unbegin-syntax $syntax)
    (syntax-case $syntax (begin)
      ((begin $syntax)
        #'$syntax)
      ($other #'$other)))

  (define (syntaxes->syntax $syntaxes)
    (case (length $syntaxes)
      ((1) (car $syntaxes))
      (else #`(begin #,@$syntaxes))))

  (define (expand-begin-syntaxes $syntaxes)
    (apply append
      (map
        (lambda ($syntax)
          (syntax-case $syntax (begin)
            ((begin $syntax ...)
              (syntax->list #'($syntax ...)))
            ($other (list #'$other))))
        $syntaxes)))

  (define-syntax define-rule-syntax
    (syntax-rules ()
      ((_ (literal ...) (name param ...) body rest ...)
        (identifier? #'name)
        (define-syntax name
          (syntax-rules (literal ...)
            ((_ param ...) body rest ...))))
      ((_ name expr)
        (identifier? #'name)
        (define-syntax (name $syntax)
          (syntax-case $syntax ()
            (_ #'expr))))
      ((_ body ...)
        (define-rule-syntax ()
          body ...))))

  (define-syntax define-case-syntax
    (syntax-rules ()
      ((_ ($name $param ...) $body)
        (define-case-syntax ($name $param ...) () $body))
      ((_ ($name $param ...) $keywords $body)
        (define-case-syntax $name $keywords
          (($name $param ...) $body)))
      ((_ $name $keywords $case ...)
        (define-syntax $name
          (lambda ($tmp)
            (syntax-case $tmp $keywords
              $case ...))))))

  (define-rule-syntax (define-keyword aux)
    (define-rule-syntax aux
      (syntax-error #'aux "misplaced aux keyword")))

  (define-rule-syntax (define-keywords aux ...)
    (begin (define-keyword aux) ...))

  (define-syntax define-lookup-syntax
    (syntax-rules ()
      ((_ $name $body)
        (identifier? #'$name)
        (define-syntax $name $body))
      ((_ ($name $syntax) $body)
        (and (identifier? #'$name) (identifier? #'$syntax))
        (define-syntax ($name $syntax) $body))
      ((_ ($name $syntax $lookup) $body)
        (and (identifier? #'$name) (identifier? #'$syntax) (identifier? #'$lookup))
        (define-syntax ($name $syntax)
          (lambda ($lookup) $body)))))

  (define (syntax-selector $syntax)
    (syntax-case-opt $syntax ()
      ($id (identifier? #'$id) #'$id)
      (($id $arg ...) (identifier? #'$id) #'$id)))

  (define (syntax-pattern-id $pattern)
    (or
      (syntax-selector $pattern)
      (syntax-error $pattern "pattern syntax error")))

  (define (syntax-clause-or-rule-id $rule)
    (syntax-case $rule ()
      (($pattern . $rest)
        (syntax-pattern-id #'$pattern))))

  (define syntax-rule-id syntax-clause-or-rule-id)
  (define syntax-clause-id syntax-clause-or-rule-id)

  (define-syntax (syntax-case-opt $syntax)
    (syntax-case $syntax ()
      ((_ $expr ($literal ...) $clause ...)
        #`(syntax-case $expr ($literal ...)
          $clause ... (_ #f)))))

  (define-rule-syntax (syntax-eval body)
    (let-syntax
      ((inlined (lambda (_) body)))
      inlined))

  (define-rule-syntax (quasisyntax-eval body)
    (syntax-eval #`body))

  (define (syntax-replace $from-id $to $syntax)
    (syntax-case $syntax ()
      (x
        (and
          (identifier? #'x)
          (free-identifier=? #'x $from-id))
        $to)
      ((x . y)
        #`(
          #,(syntax-replace $from-id $to #'x)
          .
          #,(syntax-replace $from-id $to #'y)))
      (x #'x)))

  (define (syntax-replace... $from-id $tos $syntax)
    (syntax-case $syntax ()
      ((x ellipses . y)
        (and
          (identifier? #'x)
          (identifier? #'ellipses)
          (free-identifier=? #'x $from-id)
          (free-identifier=? #'ellipses #'(... ...)))
        #`(#,@$tos . #,(syntax-replace... $from-id $tos #'y)))
      ((x . y)
        (and
          (identifier? #'x)
          (free-identifier=? #'x $from-id))
        (syntax-error #'x "missing ellipses"))
      ((x . y)
        #`(
          #,(syntax-replace... $from-id $tos #'x)
          .
          #,(syntax-replace... $from-id $tos #'y)))
      (other #'other)))

  (define syntax? (record-predicate (record-rtd #`foo)))

  (define (syntax=? a b)
    (syntax-case a ()
      (()
        (syntax-case b ()
          (() #t)
          (_ #f)))
      ((a . as)
        (syntax-case b ()
          ((b . bs)
            (and
              (syntax=? #'a #'b)
              (syntax=? #'as #'bs)))
          (_ #f)))
      (a (identifier? #'a)
        (and
          (identifier? b)
          (free-identifier=? #'a b)))
      (_
        (equal?
          (syntax->datum a)
          (syntax->datum b)))))

  (define (syntax-datum=? $a $b)
    (equal?
      (syntax->datum $a)
      (syntax->datum $b)))

  (define-syntax (inline-indexed $syntax)
    (syntax-case $syntax ()
      ((_ ($id $count) $body ...)
        #`(begin
          #,@(map
            (lambda ($index)
              #`(let-syntax
                (($id (lambda ($syntax) #'#,$index)))
                $body ...))
            (iota (datum $count)))))))

  (define (syntax-rule->clause $rule)
    (syntax-case $rule (fenders implicit)
      ((pattern (fenders $fender ...) (implicit $implicit ...) body)
        #`(pattern
          (and $fender ...)
          (with-implicit (#,(syntax-pattern-id #'pattern) $implicit ...) #'body)))
      ((pattern (implicit $implicit ...) body)
        (syntax-rule->clause
          #`(pattern (fenders) (implicit $implicit ...) body)))
      ((pattern fender body)
        (syntax-rule->clause
          #`(pattern (fenders fender) (implicit) body)))
      ((pattern body)
        (syntax-rule->clause
          #`(pattern #t body)))))

  (define (syntax->datum/annotation $syntax)
    (or
      (syntax->annotation $syntax)
      (syntax->datum $syntax)))

  (define (bytevector->syntax $bytevector)
    #`(($primitive 3 bytevector)
      #,@(map
        (lambda ($u8) (datum->syntax #'bytevector->syntax $u8))
        (bytevector->u8-list $bytevector))))

  (define (vector->syntax $vector)
    #`(($primitive 3 vector)
      #,@(vector->list $vector)))

  (define transform
    (case-lambda
      (($transformer $syntax $lookup)
        (let (($result ($transformer $syntax)))
          (if (procedure? $result)
            ($result $lookup)
            $result)))
      (($transformer $syntax)
        (transform $transformer $syntax (lambda _ #f)))))

  (define-rule-syntax (syntaxes xs ...)
    (syntax->list #'(xs ...)))

  (define (syntax-subst $from $to $syntax)
    (syntax-case #`(#,$from #,$to) ()
      ((() ())
        $syntax)
      ((x y)
        (identifier? #'x)
        (syntax-replace #'x #'y $syntax))
      (((x ellipses) y)
        (and (identifier? #'x) (identifier? #'ellipses) (free-identifier=? #'ellipses #'(... ...)))
        (syntax-replace... #'x (syntax->list #'y) $syntax))
      (((a . b) (c . d))
        (syntax-subst #'b #'d
          (syntax-subst #'a #'c $syntax)))))

  (define-rule-syntax (syntax-case? expr keywords case ...)
    (syntax-case expr keywords case ... (_ #f)))

  (define (syntax-cons $a $b)
    #`(#,$a . #,$b))

  (define (syntax-car $syntax)
    (syntax-case $syntax ()
      ((a . _) #'a)))

  (define (syntax-cdr $syntax)
    (syntax-case $syntax ()
      ((_ . b) #'b)))

  (define (syntax-add $syntax $id $value)
    (syntax-cons #`(#,$id . #,$value) $syntax))

  (define (syntax-ref* $syntax $id)
    (syntax-case $syntax ()
      (() #'())
      (((id . value) . tail)
        (if (free-identifier=? #'id $id)
          #`(value . #,(syntax-ref* #'tail $id))
          (syntax-ref* #'tail $id)))))

  (define (syntax-ref? $syntax $id)
    (syntax-case $syntax ()
      (() #f)
      (((id . value) . tail)
        (if (free-identifier=? #'id $id)
          #'value
          (syntax-ref? #'tail $id)))))

  (define (syntax-ref $syntax $id)
    (or
      (syntax-ref? $syntax $id)
      (syntax-error $syntax
        (format "~a not found in" (syntax->datum $id)))))

  (define (syntax-update $syntax $id $fn)
    (syntax-case $syntax ()
      (()
        (let (($updated ($fn #f)))
          (if $updated #`((#,$id . #,$updated)) #'())))
      (((id . value) . tail)
        (if (free-identifier=? #'id $id)
          (let (($updated ($fn #'value)))
            (if $updated
              #`((id . #,$updated) . tail)
              #'tail))
          #`((id . value) . #,(syntax-update #'tail $id $fn))))))

  (define (syntax-set $syntax $id $value)
    (syntax-update $syntax $id
      (lambda (_) $value)))

  (define (syntax-remove $syntax $id)
    (syntax-update $syntax $id
      (lambda (_) #f)))

  (define (syntax-false? $syntax)
    (syntax-case $syntax ()
      (#f #t)
      (_ #f)))

  (define (list->syntax $list)
    #`(#,@$list))

  (define (syntax-append . $list)
    (list->syntax $list))

  (define (literal->syntax $literal)
    (datum->syntax #'literal->syntax $literal))

  (define (syntax-single $syntax)
    (syntax-case $syntax ()
      ((item) #'item)))

  (define (syntax-and $a $b)
    (if (syntax-false? $a) $a $b))

  (define (syntax-or $a $b)
    (if (syntax-false? $a) $b $a))

  (define-syntax get-property
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ id key)
            #`'#,(datum->syntax #'get-property ($lookup #'id #'key)))))))

  (define (syntax->list* $syntax)
    (syntax-case $syntax ()
      ((x ... . y)
        (append #'(x ...) (if (null? (datum y)) '() #'y)))))
)
