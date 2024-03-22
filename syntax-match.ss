(library (syntax-match)
  (export
    define-syntax-matcher
    syntax-match)
  (import (micascheme))

  (define-aux-keyword syntax-matcher)

  (define-syntax-rule (define-syntax-matcher $name $matcher)
    (begin
      (define-aux-keyword $name)
      (define-property $name syntax-matcher $matcher)))

  (define-syntax-matcher r
    (lambda ($syntax $match)
      (syntax-case $syntax (+ b c d e h ixh iyh l ixl iyl hl ix iy a)
        (b        ($match #f   #x000 #f))
        (c        ($match #f   #x001 #f))
        (d        ($match #f   #x010 #f))
        (e        ($match #f   #x011 #f))
        (h        ($match #f   #x100 #f))
        (ixh      ($match #xdd #x100 #f))
        (iyh      ($match #xfd #x100 #f))
        (l        ($match #f   #x101 #f))
        (ixl      ($match #xdd #x101 #f))
        (iyl      ($match #xfd #x101 #f))
        ((hl)     ($match #f   #x110 #f))
        ((+ ix d) ($match #xdd #x110 #'d))
        ((+ iy d) ($match #xfd #x110 #'d))
        (a        ($match #f   #x111 #f))
        (_        #f))))

  (define-syntax syntax-matcher
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ $name)
            (and
              (identifier? #'$name)
              ($lookup #'$name #'syntax-matcher))
            ($lookup #'$name #'syntax-matcher))))))

  (define-syntax syntax-match
    (lambda ($syntax)
      (lambda ($lookup)
        (data (builder value fenders body))

        (define (builder-syntax $builder)
          #`(
            #,(builder-pattern $builder)
            (and #,@(reverse (builder-fenders $builder)))
            #,(builder-body $builder)))

        (define (pattern-builder $pattern $fender $body)
          (syntax-case $pattern ()
            (($id $param ...)
              (and (identifier? #'$id) ($lookup #'$id #'syntax-matcher))
              (lets
                ($matcher ($lookup #'$id #'syntax-matcher))
                ($tmp (car (generate-temporaries `(tmp))))
                (builder
                  $tmp
                  (list #`($id #,$tmp))
                  ($matcher (syntax->list #'($param ...)) $body))))
            (($pattern ...)
              (lets
                ($sub-builder
                  (fold-left
                    builder+pattern
                    (builder (stack) (builder-fenders $builder) (builder-body $builder))
                    (syntax->list #'($pattern ...))))
                (builder
                  (push (builder-patterns $builder) #`(#,@(reverse (builder-patterns $sub-builder))))
                  (builder-fenders $sub-builder)
                  (builder-body $sub-builder))))
            ($other
              (builder
                (push (builder-patterns $builder) $other)
                (builder-fenders $builder)
                (builder-body $builder)))))

        (define (builder+pattern $builder $pattern)
          (syntax-case $pattern ()
            (($id $param ...)
              (and (identifier? #'$id) ($lookup #'$id #'syntax-matcher))
              (lets
                ($matcher ($lookup #'$id #'syntax-matcher))
                ($tmp (car (generate-temporaries `(tmp))))
                (builder
                  (push (builder-patterns $builder) $tmp)
                  (push (builder-fenders $builder) #`($id #,$tmp))
                  ($matcher (syntax->list #'($param ...)) (builder-body $builder)))))
            (($pattern ...)
              (lets
                ($sub-builder
                  (fold-left
                    builder+pattern
                    (builder (stack) (builder-fenders $builder) (builder-body $builder))
                    (syntax->list #'($pattern ...))))
                (builder
                  (push (builder-patterns $builder) #`(#,@(reverse (builder-patterns $sub-builder))))
                  (builder-fenders $sub-builder)
                  (builder-body $sub-builder))))
            ($other
              (builder
                (push (builder-patterns $builder) $other)
                (builder-fenders $builder)
                (builder-body $builder)))))



        (syntax-case $syntax ()
          ((_ $syntax-expr $literals $entry ...)
            #`(syntax-case $syntax-expr $literals
              #,@(map
                (rec $rec-entry
                  (lambda ($entry)
                    (syntax-case $entry ()
                      (($pattern $body)
                        ($rec-entry #'($pattern #t $body)))
                      (($pattern $fender $body)
                        (builder+pattern
                          (builder (stack) (stack #'$fender) #'$body)
                          #'$pattern)
                        (syntax-case $pattern ()
                          (($id $param ...)
                            (and (identifier? #'$id) ($lookup #'$id #'syntax-matcher))
                            (lets
                              ($matcher ($lookup #'$id #'syntax-matcher))
                              ($tmp (car (generate-temporaries `(tmp))))
                              (entry
                                (push (builder-patterns $entry) $tmp)
                                (push (builder-fenders $entry) #`($id #,$tmp))
                                ($matcher (builder-body $entry)))))
                          (($pattern ...)
                            (entry
                              (push (builder-patterns $entry) #'$other)
                              (builder-fenders $entry)
                              (builder-body $entry)))
                          ($other
                            (entry
                              (push (builder-patterns $entry) #'$other)
                              (builder-fenders $entry)
                              (builder-body $entry)))))

                        (builder-syntax
                          (syntax-case #'$pattern ()
                            (($pattern ...)
                              (fold-left
                                (lambda ($entry $pattern)
                                  (syntax-case $pattern ()
                                (entry (stack) (stack #'$fender) #'$body)
                                (syntax->list #'($pattern ...))))
                            ($other
                              (entry
                                (stack #'$pattern)
                                (stack #'$fender)
                                #'$body))))))))
                (syntax->list #'($entry ...)))))))))
)

