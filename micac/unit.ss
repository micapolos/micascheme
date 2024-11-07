(library (micac unit)
  (export
    bit
    size->value
    size->uint-type
    transform-decl
    core->unit
    core+)
  (import
    (micascheme)
    (micac syntax))

  (define-aux-keywords bit)
  (define-aux-keywords core init update pos-edge neg-edge)

  (define (size->value $size)
    (switch (syntax->datum $size)
      ((integer? $integer)
        (if (positive? $integer)
          $integer
          (syntax-error $size "not positive")))
      ((else _)
        (syntax-error $size "not integer"))))

  (define (size->uint-type $size)
    (lets
      ($bits (size->value $size))
      (cond
        ((<= $bits 8) #`uint8_t)
        ((<= $bits 16) #`uint16_t)
        ((<= $bits 32) #`uint32_t)
        ((<= $bits 64) #`uint64_t)
        (else (syntax-error $size "64 <")))))

  (define (transform-decl $decl)
    (syntax-case $decl (bit)
      ((id bit)
        #`(var uint8_t id))
      ((id (* bit n))
        #`(var #,(size->uint-type #'n) id))
      ((id (* type n))
        (syntax-case (transform-decl #'(id type)) (var)
          ((var type id)
            #`(var type (* id #,(size->value #'n))))))))

  (define (unit->micac $unit)
    (syntax-case $unit (init pos-edge neg-edge)
      (
        (_
          (init init-body ...)
          (pos-edge pos-edge-body ...)
          (neg-edge neg-edge-body ...))
        #`(micac
          (init
            init-body ...)
          (update
            pos-edge-body ...
            neg-edge-body ...)))))

  (define (core->unit $core)
    (syntax-case $core (core init pos-edge neg-edge)
      (
        (core
          (init init-body ...)
          (pos-edge pos-body ...)
          (neg-edge neg-body ...))
        #`(unit
          (init init-body ...)
          (update pos-body ... neg-body ...)))))

  (define (core+ $a $b)
    (syntax-case $a (core init pos-edge neg-edge)
      (
        (core
          (init init-body-a ...)
          (pos-edge pos-body-a ...)
          (neg-edge neg-body-a ...))
        (syntax-case $b ()
          (
            (core
              (init init-body-b ...)
              (pos-edge pos-body-b ...)
              (neg-edge neg-body-b ...))
            #`(core
              (init init-body-a ... init-body-b ...)
              (pos-edge pos-body-a ... pos-body-b ...)
              (neg-edge pos-body-a ... pos-body-b ...)))))))

  (define (phase-core+inits-updates $phase? $core $inits $updates)
    (syntax-case $core (core init pos-edge neg-edge)
      (
        (core
          (init init-body ...)
          (pos-edge pos-body ...)
          (neg-edge neg-body ...))
        #`(core
          (init init-body ... #,@$inits)
          (pos-edge pos-body ...
            #,@(syntaxes-if $phase? $updates))
          (neg-edge neg-body ...
            #,@(syntaxes-if (not $phase?) $updates))))))

  (define (phase-core+component $phase? $core $component)
    (syntax-case $component (buffer register memory)
      ((buffer id type expr)
        (phase-core+inits-updates $phase? $core
          (list #`(var type id))
          (list #`(set id expr))))
      ((register id type write? expr)
        (phase-core+inits-updates $phase? $core
          (list #`(var type id))
          (list #`(when write? (set id expr)))))
      ((neg component ...)
        (fold-left
          (partial phase-core+component (not $phase?))
          $core
          (syntax->list #'(component ...))))))

  (define (syntaxes-if $cond? $syntaxes)
    (if $cond? $syntaxes (list)))
)
