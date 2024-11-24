(library (micalog domain)
  (export
    edges+
    event+?
    domain+)
  (import
    (micascheme)
    (micalog utils)
    (syntax scope)
    (prefix (micalog keywords) %))

  (define (edges+ $edges-a $edges-b)
    (syntax-case #`(#,$edges-a #,$edges-b) (%posedge %negedge %edge)
      ((%posedge %posedge) #'%posedge)
      ((%negedge %negedge) #'%negedge)
      ((_ _) #'%edge)))

  (define (event+? $event-a $event-b)
    (syntax-case #`(#,$event-a #,$event-b) ()
      (((edges-a signal-a) (edges-b signal-b))
        (and
          (free-identifier=? #'signal-a #'signal-b)
          #`(#,(edges+ #'edges-a #'edges-b) signal-a)))))

  (define event=? syntax=?)

  (define (domain+ $domain-a $domain-b)
    (syntax-case #`(#,$domain-a #,$domain-b) ()
      ((() _) #'())
      ((_ ()) #'())
      (((event-a . events-a) (event-b . events-b))
        (lets
          ($event? (event+? #'event-a #'event-b))
          (if $event?
            #`(#,$event?
              #,@(if (event=? $event? #'event-a)
                (domain+ #'events-a #'events-b)
                (list)))
            #'())))))

  (define (timed-domain $timed)
    (syntax-case $timed ()
      ((domain xs ...) #'domain)))

  (define (literal->timed $literal)
    (syntax-case $literal ()
      ((type value)
        #`(%async type value))))

  (define (scope-name->timed $scope $name)
    (syntax-case $name ()
      ((type name)
        #'(
          #,(scope-ref $scope (identifier name))
          type name))))

  (define (scope-expr->timed $scope $type $expr)
    (syntax-case $expr ()
      ((type value)
        (syntax-case #'value ()
          ((%and a b)
            (lets
              ($timed-a (scope-expr->timed $scope #'a))
              ($timed-b (scope-expr->timed $scope #'b))
              ($domain (domain+ (timed-domain $timed-a) (timed-domain $timed-b)))
              #`(#,$domain type value)))
          (name (identifier? #'name)
            #`(#,(scope-ref $scope #'name) type name))
          (literal
            #`(%async type literal))))))
)
