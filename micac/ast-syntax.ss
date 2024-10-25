(library (micac ast-syntax)
  (export )
  (import (micac ast))

  (data (scope offsets))
  (data (context bytevector-identifier))

  (define scoped cons)
  (define scope car)
  (define scoped cons)

  (define (context-instr->scoped-syntaxes $context $instr)
    (define (instr->scoped-syntaxes $instr)
      ()
      )


  (define (variable-syntax $variable)
    (lambda ($context)
      (lambda ($scope)
        #`(
          #,(identifier-append #'+ #'bytevector- (type-identifier $type) #'-native-ref)
          (context-bytevector-identifier $context)
          (datum->syntax #'+ (list-ref (scope-offsets $scope) (variable-index $variable)))))))

  (define (const-syntax $const)
    (datum->syntax #'+ (const-value $value)))

  (define (value-syntax $value)
    (value-switch $value
      ((const? $const) (lambda ($context) (lambda ($scope) (const-syntax $const))))
      ((variable? $variable) (variable-syntax $variable))))

  (define (ld-syntax $ld)
    ()

  (define (instr-syntax $instr)
    (instr-switch $instr
      ((ld? $ld))

  (define (type-size $type)
    (type-switch $type
      ((u8? _) 1)
      ((u16? _) 2)
      ((u32? _) 4)))

  (define (type-identifier $type)
    (type-switch $type
      ((u8? _) #'u8)
      ((u16? _) #'u16)
      ((u32? _) #'u32)))

  (define (bytevector-set!-identifier $type)
    (identifier-append #'bytevector #'bytevector- (type-identifier $type) #'-native-set!))

  (define (bytevector-ref-identifier $type)
    (identifier-append #'bytevector #'bytevector- (type-identifier $type) #'-native-ref))

  (define (generated+instr $context $generated $instr)
    (instr-switch $instr
      ((alloc? $alloc)
        (generated
          (generated-syntax $generated)
          (+ (generated-size (type-size (alloc-type $alloc))))))
      ((ld? $ld)
        (generated
          (push
            (generated-syntaxes $generated)
            #`(
              #,(bytevector-set!-identifier $type)
              (context-bytevector-identifier $context)
              (variable-index (ld-variable $ld))
)
