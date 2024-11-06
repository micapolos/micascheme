(library (micac unit)
  (export
    bit
    size->value
    size->uint-type
    transform-decl)
  (import
    (micascheme)
    (micac syntax))

  (define-aux-keywords bit)

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
)

; ; interface
; (mem
;   (address-size (param size))
;   (data-size (param size))
;   (address-in (input (pos-edge (* wire address-size))))
;   (data-in (input (pos-edge (* wire data-size))))
;   (write (input (pos-edge wire)))
;   (address-out (input (pos-edge (* wire address-size))))
;   (data-out (output (neg-edge (* wire data-size)))))

; ; native
; (mem
;   (address-size (param size))
;   (data-size (param size))
;   (address-in (input (pos-edge (* wire address-size))))
;   (data-in (in (pos-edge (* wire data-size))))
;   (write (in (pos-edge wire)))
;   (address-out (in (pos-edge (* wire address-size))))
;   (init (alloc mem (uint data-size) (bits-size address-size)))
;   (pos-edge (when write (set (mem (address-in)) data-in)))
;   (neg-edge (set data-out (mem (address-in))))
;   (data-out ((* wire data-size) data-out-var)))

; ; instance
; (mem-16
;   (mem
;     (address-in "0000100010001010")
;     (data-in "01001001")
;     (write "1")))

; ; usage
; (mem-16 data-out)
