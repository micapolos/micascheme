(library (monad)
  (export
    monad monad? monad-pure-fn monad-bind-fn
    monad-pure monad-bind monad-map monad-sequence monad-lift monad-apply
    monadic pure bind define-monadic
    monad-lets
    monad-stack-box
    option-monad 
    fallible-monad
    cons-monad
    listing listing-bind listing-run listing-monad
    define-monad)
  (import
    (scheme)
    (data)
    (generate)
    (identifier)
    (lets)
    (procedure)
    (throw)
    (switch)
    (failure)
    (pair)
    (syntax))

  (data (monad pure-fn bind-fn))

  ; combinators

  (define (monad-pure $monad $value)
    ((monad-pure-fn $monad) $value))

  (define (monad-bind $monad $monadic $fn)
    ((monad-bind-fn $monad) $monadic $fn))

  (define (monad-map $monad $monadic $fn)
    (monad-bind $monad $monadic
      (lambda ($value)
        (monad-pure $monad ($fn $value)))))

  (define (monad-sequence $monad $monadic-list)
    (cond
      ((null? $monadic-list) (monad-pure $monad (list)))
      (else 
        (monad-bind $monad (car $monadic-list)
          (lambda ($car)
            (monad-map $monad (monad-sequence $monad (cdr $monadic-list))
              (lambda ($cdr)
                (cons $car $cdr))))))))

  (define (monad-lift $monad $fn . $monadic-args)
    (monad-map $monad (monad-sequence $monad $monadic-args)
      (lambda ($args)
        (apply $fn $args))))

  (define (monad-apply $monad $monadic-fn . $monadic-args)
    (monad-bind $monad $monadic-fn
      (lambda ($fn)
        (monad-map $monad (monad-sequence $monad $monadic-args)
          (lambda ($args)
            (apply $fn $args))))))

  (define-syntax monad-lets
    (lambda ($syntax)
      (syntax-case $syntax (pure)
        ((_ $monad (pure $result))
          #`(monad-pure $monad $result))
        ((_ $monad $result)
          #`$result)
        ((_ $monad ($var (pure $body)) $decl ... $result)
          #`(monad-bind $monad (monad-pure $monad $body)
            (lambda ($var)
              (monad-lets $monad $decl ... $result))))
        ((_ $monad ($var $body) $decl ... $result)
          #`(monad-bind $monad $body
            (lambda ($var)
              (monad-lets $monad $decl ... $result)))))))

  (define-syntax define-monad
    (lambda ($syntax)
      (syntax-case $syntax (pure bind)
        ((_ $name
          ((pure $pure-var) $pure-body)
          ((bind $bind-var $bind-fn) $bind-body))
          (and
            (identifier? #'$name)
            (identifier? #'$pure-var)
            (identifier? #'$bind-var)
            (identifier? #'$bind-fn))
          (lets
            ($pure-name #'$name)
            ($bind-name (build-identifier ($string #'$name) (string-append $string "-bind")))
            #`(begin
              (define (#,$pure-name $pure-var) $pure-body)
              (define (#,$bind-name $bind-var $bind-fn) $bind-body)))))))

  ; monad-stack

  (define (monad-stack-boxer-opt $from-monad-stack $to-monad-stack)
    (cond
      ((null? $to-monad-stack)
        (and (null? $from-monad-stack) identity))
      (else
        (cond
          ((and (not (null? $from-monad-stack)) (equal? (car $from-monad-stack) (car $to-monad-stack)))
            (lets
              ($monad-stack-boxer-opt (monad-stack-boxer-opt (cdr $from-monad-stack) (cdr $to-monad-stack)))
              (and $monad-stack-boxer-opt
                (lambda ($monadic)
                  (monad-map (car $from-monad-stack) $monadic
                    (lambda ($value)
                      ($monad-stack-boxer-opt $value)))))))
          (else
            (lets
              ($monad-stack-boxer-opt (monad-stack-boxer-opt $from-monad-stack (cdr $to-monad-stack)))
              (and $monad-stack-boxer-opt
                (lambda ($value)
                  (monad-pure (car $to-monad-stack)
                    ($monad-stack-boxer-opt $value))))))))))

  (define (monad-stack-box $from-monad-stack $to-monad-stack $monadic)
    (lets
      ($monad-stack-boxer-opt (monad-stack-boxer-opt $from-monad-stack $to-monad-stack))
      (and $monad-stack-boxer-opt (box ($monad-stack-boxer-opt $monadic)))))

  ; syntaxes

  (define-aux-keyword pure)
  (define-aux-keyword bind)

  (define-syntax monadic
    (syntax-rules (pure lets)
      ((_ $monad (pure $value))
        (monad-pure $monad $value))
      ((_ $monad (lets ($var $expr) $binding ... $result))
        (monad-bind $monad (monadic $monad $expr)
          (lambda ($var)
            (monadic $monad (lets $binding ... $result)))))
      ((_ $monad (lets $result))
        (monadic $monad $result))
      ((_ $monad $other) 
        $other)))

  (define-syntax define-monadic
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($id $param ...) $body)
          (lets
            ($monad (generate-temporary #`monad))
            #`(define $id
              (lambda (#,$monad $param ...) 
                (monadic #,$monad $body))))))))

  ; monad-stack

  (define (cons-monad $car)
    (monad
      (lambda ($value) (cons $car $value))
      (lambda ($pair $fn) ($fn (cdr $pair)))))

  (define option-monad 
    (monad
      (lambda ($value) 
        (or $value (throw option-monad-pure $value)))
      (lambda ($option $fn)
        (and $option ($fn $option)))))

  (define fallible-monad
    (monad
      (lambda ($value)
        (switch $value
          ((failure? $failure) (throw fallible $failure))
          ((else $success) $success)))
      (lambda ($fallible $fn)
        (switch $fallible
          ((failure? $failure) $failure)
          ((else $success) ($fn $success))))))

  (define (listing $value)
    (lambda ($list) 
      (cons $value (cons $value $list))))

  (define (listing-bind $listing $fn)
    (lambda ($list)
      (lets
        ((values $value $list) (pair-values ($listing $list)))
        (($fn $value) $list))))

  (define (listing-run $listing) 
    ($listing (list)))

  (define listing-monad 
    (monad listing listing-bind))
)
