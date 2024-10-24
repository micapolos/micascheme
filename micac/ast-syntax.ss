(library (micac ast-syntax)
  (export )
  (import (micac ast))

  (data (generated syntaxes size))
  (data (context bytevector-identifier))

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
