(library (tt primitive)
  (export
    atomic
    atomic?
    atomic-syntax
    atomic-ref

    class
    class?
    class-id
    class-args
    generate-class

    primitive?
    primitive-switch

    primitive=?
    primitive->datum
    primitive->syntax

    literal->atomic
    primitive-apply-term)
  (import
    (scheme)
    (data)
    (union)
    (procedure)
    (throw)
    (syntax)
    (tt hoas))

  (data (atomic syntax ref))
  (data (class id args))
  (union (primitive atomic class))

  (define (literal->atomic $literal)
    (atomic (literal->syntax $literal) $literal))

  (define (generate-class $name . $args)
    (class (gensym $name) $args))

  (define (primitive->datum $depth $primitive)
    (primitive-switch $primitive
      ((atomic? $atomic)
        (syntax->datum (atomic-syntax $atomic)))
      ((class? $class)
        `(class
          ,(class-id $class)
          ,@(map (partial term->datum primitive->datum $depth) (class-args $class))))))

  (define (primitive->syntax $depth $primitive)
    (primitive-switch $primitive
      ((atomic? $atomic)
        (atomic-syntax $atomic))
      ((class? $class)
        #`(class
          #,(literal->syntax (class-id $class))
          (list #,@(map (partial term->syntax primitive->syntax $depth) (class-args $class)))))))

  (define (primitive=? $depth $lhs $rhs)
    (primitive-switch $lhs
      ((atomic? $lhs)
        (and
          (atomic? $rhs)
          (equal?
            (atomic-ref $lhs)
            (atomic-ref $rhs))))
      ((class? $lhs)
        (and
          (class? $rhs)
          (symbol=?
            (class-id $lhs)
            (class-id $rhs))
          (for-all (partial term=? primitive=? $depth)
            (class-args $lhs)
            (class-args $rhs))))))

  (define (primitive-apply-term $fn . $args)
    (cond
      ((for-all atomic? (cons $fn $args))
        (native (literal->atomic (apply (atomic-ref $fn) (map atomic-ref $args)))))
      (else
        (apply application* (native $fn) (map native $args)))))
)
