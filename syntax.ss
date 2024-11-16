(library (syntax)
  (export
    identifiers?
    syntax-null?
    define-rule-syntax
    define-case-syntax
    define-aux-keyword
    define-aux-keywords
    unbegin-syntaxes
    unbegin-syntax
    expand-begin-syntaxes
    define-lookup-syntax
    syntax-selector
    syntax-pattern-id
    syntax-rule-id
    syntax-clause-id
    syntax-case-opt
    syntax-inline
    inline-indexed
    fenders implicit
    syntax-rule->clause
    syntax->datum/annotation
    bytevector->syntax
    vector->syntax
    syntax=?
    syntax-replace
    transform
    syntaxes
    syntax-subst
    syntax-contains?)
  (import (scheme) (syntax-keywords))

  (define (identifiers? $syntax)
    (for-all identifier? (syntax->list $syntax)))

  (define (syntax-null? $syntax)
    (null? (syntax->datum $syntax)))

  (define (unbegin-syntaxes $syntax)
    (syntax-case $syntax (begin)
      ((begin $syntax ...)
        (syntax->list #'($syntax ...)))
      ($other (list #'$other))))

  (define (unbegin-syntax $syntax)
    (syntax-case $syntax (begin)
      ((begin $syntax)
        #'$syntax)
      ($other #'$other)))

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
    (syntax-rules (literals)
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
          ((_ $param ...) $body)))
      ((_ $name $keywords $case ...)
        (define-syntax $name
          (lambda ($tmp)
            (syntax-case $tmp $keywords
              $case ...))))))

  (define-rule-syntax (define-aux-keyword aux)
    (define-rule-syntax aux
      (syntax-error #'aux "misplaced aux keyword")))

  (define-rule-syntax (define-aux-keywords aux ...)
    (begin (define-aux-keyword aux) ...))

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

  (define-syntax (syntax-inline $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #'(let-syntax
          ((inlined (lambda (_) $expr)))
          inlined))))

  (define (syntax-replace $from-id $to-id $syntax)
    (syntax-case $syntax ()
      (x
        (and
          (identifier? #'x)
          (free-identifier=? #'x $from-id))
        $to-id)
      ((x . y)
        #`(
          #,(syntax-replace $from-id $to-id #'x)
          .
          #,(syntax-replace $from-id $to-id #'y)))
      (x #'x)))

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
        (syntax-case b ()
          (b (identifier? #'b)
            (free-identifier=? #'a #'b))))
      (_
        (equal?
          (syntax->datum a)
          (syntax->datum b)))))

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
      (((a . b) (c . d))
        (syntax-subst #'b #'d
          (syntax-subst #'a #'c $syntax)))))

  (define (syntax-contains? $syntax $id)
    (syntax-case $syntax ()
      (()
        #f)
      (x (identifier? #'x)
        (free-identifier=? #'x $id))
      ((x . y)
        (or
          (syntax-contains? #'x $id)
          (syntax-contains? #'y $id)))))
)
