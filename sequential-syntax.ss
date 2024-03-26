(library (sequential-syntax)
  (export
    context context? context-bindings context-lookup-fn
    empty-context lookup-context context-bind context-ref

    deps deps? deps-declarations deps-updaters
    empty-deps
    deps+

    sequential sequential? sequential-deps sequential-value
    pure-sequential
    sequential-bind
    sequential-list

    template template? template-bindings template-params template-body

    syntax-sequential
    sequential-syntax
    sequential->datum
    sequential->vector-syntax
    sequential->vector
    sequential-counter
    sequential-osc

    syntax-transform
    syntax-list-transform

    sequence)
  (import (micascheme))

  (data (context bindings lookup-fn))
  (data (deps declarations updaters))
  (data (sequential deps value))
  (data (template bindings params body))

  (define (empty-context)
    (lookup-context (lambda (_) #f)))

  (define (lookup-context $fn)
    (context (stack) $fn))

  (define (context-bind $context $id $sequential)
    (context
      (push (context-bindings $context) (cons $id $sequential))
      (context-lookup-fn $context)))

  (define (context-ref $context $id)
    (or
      (lets
        ($ass (assp (partial free-identifier=? $id) (context-bindings $context)))
        (and $ass (cdr $ass)))
      ((context-lookup-fn $context) $id)))

  (define (context-clean $context)
    (context (stack) (context-lookup-fn $context)))

  (define (empty-deps)
    (deps (stack) (stack)))

  (define (deps+ $a $b)
    (deps
      (push-all (deps-declarations $a) (deps-declarations $b))
      (push-all (deps-updaters $a) (deps-updaters $b))))

  (define (pure-sequential $value)
    (sequential (empty-deps) $value))

  (define (sequential-bind $sequential $fn)
    (lets
      ($fn-sequential ($fn (sequential-value $sequential)))
      (sequential
        (deps+
          (sequential-deps $sequential)
          (sequential-deps $fn-sequential))
        (sequential-value $fn-sequential))))

  (define (sequential-list $sequentials)
    (cond
      ((null? $sequentials)
        (pure-sequential (list)))
      (else
        (sequential-bind (car $sequentials)
          (lambda ($car)
            (sequential-bind (sequential-list (cdr $sequentials))
              (lambda ($cdr)
                (pure-sequential (cons $car $cdr)))))))))

  (define (syntax-list-transform $context $syntax-list)
    #`(begin
      #,@(map (partial syntax-transform $context) $syntax-list)))

  (define (syntax-transform $context $syntax)
    (syntax-case $syntax (define)
      ((define $id $body)
        (identifier? #`$id)
        #`(begin
          (define-aux-keyword $id)
          (define-property $id sequential
            #,(sequential-syntax
              (syntax-sequential $context
                #`$body)))))
      ((define ($id $param ...) $body)
        (identifiers? #`($id $param ...))
        #`(begin
          (define-aux-keyword $id)
          (define-property $id sequential
            #,(sequential-syntax
              (template (stack) (syntax->list #`($param ...)) #`$body)))))
      ($other
        #`(writeln
          #,(sequential->vector-syntax
            (syntax-sequential $context #`$other)
            10)))))

  (define-aux-keyword sequence)

  (define (syntax-sequential $context $syntax)
    (syntax-case $syntax (sequence lets)
      ((sequence $init $var $update) (identifier? #`$var)
        (lets
          ($tmp (generate-temporary #`$var))
          ($context (context-bind $context #`$var (pure-sequential $tmp)))
          (sequential-bind (syntax-sequential $context #`$init)
            (lambda ($init)
              (sequential-bind (syntax-sequential $context #`$update)
                (lambda ($update)
                  (sequential
                    (deps
                      (stack #`(define #,$tmp #,$init))
                      (stack #`(set! #,$tmp #,$update)))
                    $tmp)))))))
      ((lets $body)
        (syntax-sequential $context #`$body))
      ((lets ($var $expr) $rest ... $body) (identifier? #`$var)
        (sequential-bind (syntax-sequential $context #`$expr)
          (lambda ($expr)
            (lets
              ($tmp (generate-temporary #`$var))
              ($context (context-bind $context #`$var (pure-sequential $tmp)))
              ($sequential
                (sequential
                  (deps
                    (stack #`(define #,$tmp #,$expr))
                    (stack #`(set! #,$tmp #,$expr)))
                  $tmp))
              (sequential-bind $sequential
                (lambda (_)
                  (syntax-sequential $context #`(lets $rest ... $body))))))))
      ((lets (($var $param ...) $expr) $rest ... $body) (identifier? #`$var)
        (lets
          ($template
            (template
              (context-bindings $context)
              (syntax->list #`($param ...))
              #`$expr))
          ($context (context-bind $context #`$var $template))
          (syntax-sequential $context #`(lets $rest ... $body))))
      (($fn $arg ...)
        (lets
          ($fn (syntax-sequential $context #`$fn))
          ($args
            (map
              (partial syntax-sequential $context)
              (syntax->list #`($arg ...))))
          (switch $fn
            ((sequential? $sequential-fn)
              (sequential-bind $sequential-fn
                (lambda ($fn)
                  (sequential-bind (sequential-list $args)
                    (lambda ($args)
                      (pure-sequential #`(#,$fn #,@$args)))))))
            ((template? $template)
              (lets
                ($params (template-params $template))
                ($context
                  (fold-left
                    context-bind
                    (context (template-bindings $template) (context-lookup-fn $context))
                    $params $args))
                (syntax-sequential $context (template-body $template)))))))
      ($id (identifier? #`$id)
        (or
          (context-ref $context #`$id)
          (pure-sequential #`$id)))
      ($item
        (pure-sequential #`$item))))

  (define (sequential-syntax $sequential)
    (switch $sequential
      ((sequential? $sequential)
        (lets
          ($deps (sequential-deps $sequential))
          ($value (sequential-value $sequential))
          ($vector (generate-temporary #`vector))
          ($index (generate-temporary #`index))
          #`(sequential
            (deps
              (stack #,@(map (lambda ($) #`(syntax #,$)) (reverse (deps-declarations $deps))))
              (stack #,@(map (lambda ($) #`(syntax #,$)) (reverse (deps-updaters $deps)))))
            (syntax #,(sequential-value $sequential)))))
      ((template? $template)
        (lets
          #`(template
            (stack) ; TODO???
            (list #,@(map (lambda ($param) #`(syntax #,$param)) (template-params $template)))
            (syntax #,(template-body $template)))))))

  (define (sequential->datum $sequential)
    (lets
      ($deps (sequential-deps $sequential))
      ($value (sequential-value $sequential))
      ($vector (generate-temporary #`vector))
      ($index (generate-temporary #`index))
      `(sequential
        (declarations ,@(reverse (map syntax->datum (deps-declarations $deps))))
        (updaters ,@(reverse (map syntax->datum (deps-updaters $deps))))
        (value ,(syntax->datum (sequential-value $sequential))))))

  (define (sequential->vector-syntax $sequential $size)
    (lets
      ($deps (sequential-deps $sequential))
      ($value (sequential-value $sequential))
      ($vector (generate-temporary #`vector))
      ($index (generate-temporary #`index))
      #`(let ()
        (define #,$vector (make-vector #,$size))
        #,@(reverse (deps-declarations $deps))
        (do
          ((#,$index 0 (+ #,$index 1)))
          ((= #,$index #,$size) #,$vector)
          (vector-set! #,$vector #,$index #,$value)
          #,@(reverse (deps-updaters $deps))))))

  (define (sequential->vector $sequential $size)
    (eval
      (syntax->datum (sequential->vector-syntax $sequential $size))
      (environment `(micascheme))))

  (define (sequential-counter)
    (lets
      ($counter (generate-temporary #`counter))
      (sequential
        (deps
          (stack #`(define #,$counter 0))
          (stack #`(set! #,$counter (+ #,$counter 1))))
        $counter)))

  (define (sequential-osc $delta)
    (sequential-bind $delta
      (lambda ($delta)
        (lets
          ($osc (generate-temporary #`osc))
          (sequential
            (deps
              (stack #`(define #,$osc 0.0))
              (stack #`(set! #,$osc (fract (+ #,$osc #,$delta)))))
            $osc)))))
)
