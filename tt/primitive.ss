(library (tt primitive)
  (export
    class
    class?
    class-id
    class-args
    generate-class

    primitive?
    primitive-switch

    primitive=?
    primitive->datum
    primitive->syntax)
  (import
    (scheme)
    (data)
    (union)
    (procedure)
    (throw)
    (syntax)
    (tt hoas))

  (data (class id args))
  (union (primitive symbol boolean number char string null pair class))

  (define (generate-class $name . $args)
    (class (gensym $name) $args))

  (define (primitive->datum $depth $primitive)
    (primitive-switch $primitive
      ((symbol? $symbol) $symbol)
      ((boolean? $boolean) $boolean)
      ((number? $number) $number)
      ((char? $char) $char)
      ((string? $string) $string)
      ((null? $null) $null)
      ((pair? $pair)
        `(
          ,(term->datum primitive->datum $depth (car $pair))
          ,(term->datum primitive->datum $depth (cdr $pair))))
      ((class? $class)
        `(class
          ,(class-id $class)
          ,@(map (partial term->datum primitive->datum $depth) (class-args $class))))))

  (define (primitive->syntax $depth $primitive)
    (primitive-switch $primitive
      ((symbol? $symbol) #`'#,(literal->syntax $symbol))
      ((boolean? $boolean) (literal->syntax $boolean))
      ((number? $number) (literal->syntax $number))
      ((char? $char) (literal->syntax $char))
      ((string? $string) (literal->syntax $string))
      ((null? $null) #'())
      ((pair? $pair)
        #`(cons
          #,(term->syntax primitive->syntax $depth (car $pair))
          #,(term->syntax primitive->syntax $depth (cdr $pair))))
      ((class? $class)
        #`(class
          #,(literal->syntax (class-id $class))
          (list #,@(map (partial term->syntax primitive->syntax $depth) (class-args $class)))))))

  (define (primitive=? $depth $lhs $rhs)
    (primitive-switch $lhs
      ((symbol? $lhs)
        (and
          (symbol? $rhs)
          (symbol=? $lhs $rhs)))
      ((boolean? $lhs)
        (and
          (boolean? $rhs)
          (boolean=? $lhs $rhs)))
      ((number? $lhs)
        (and
          (number? $rhs)
          (= $lhs $rhs)))
      ((char? $lhs)
        (and
          (char? $rhs)
          (char=? $lhs $rhs)))
      ((string? $lhs)
        (and
          (string? $rhs)
          (string=? $lhs $rhs)))
      ((null? $lhs)
        (null? $rhs))
      ((pair? $lhs)
        (and
          (pair? $rhs)
          (term=? primitive=? $depth
            (car $lhs)
            (car $rhs))
          (term=? primitive=? $depth
            (cdr $lhs)
            (cdr $rhs))))
      ((class? $lhs)
        (and
          (class? $rhs)
          (symbol=?
            (class-id $lhs)
            (class-id $rhs))
          (for-all (partial term=? primitive=? $depth)
            (class-args $lhs)
            (class-args $rhs))))))
)
