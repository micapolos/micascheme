(library (asm)
  (export
    org eq u8
    asm asm?
    asm-org asm-with-org
    asm-labels asm-with-labels
    asm-values asm-with-values
    asm-puts asm-puts
    empty-asm
    asm+org
    asm+label
    asm+value
    asm+put
    asm+u8
    asm+syntax
    asm->put-syntax
    asm->bytevector-syntax)
  (import (micascheme))

  (data (asm org labels values puts))

  (define-keywords org eq u8)

  (define (empty-asm)
    (asm 0 (stack) (stack) (stack)))

  (define (asm+label $asm $label)
    (asm-with-labels $asm
      (push (asm-labels $asm)
        (syntax-append $label (literal->syntax (asm-org $asm))))))

  (define (asm+org $asm $offset)
    (asm-with-org $asm
      (+ (asm-org $asm) $offset)))

  (define (asm+value $asm $id $value)
    (asm-with-values $asm
      (push (asm-values $asm)
        (syntax-append $id $value))))

  (define (asm+put $asm $put)
    (asm-with-puts $asm
      (push (asm-puts $asm) $put)))

  (define (asm+u8 $asm $u8)
    (fluent $asm
      (asm+put (lambda ($port) #`(put-u8 #,$port #,$u8)))
      (asm+org 1)))

  (define (asm->put-syntax $asm $port)
    #`(lets
      #,@(reverse (asm-labels $asm))
      #,@(reverse (asm-values $asm))
      (run
        #,@(map-with
          ($put (reverse (asm-puts $asm)))
          ($put #'$port)))))

  (define (asm->bytevector-syntax $asm)
    #`(with-bytevector-output-port $port
      #,(asm->put-syntax $asm #'$port)))

  (define (asm+syntax $asm $syntax)
    (syntax-case $syntax (eq org u8)
      (id
        (identifier? #'id)
        (asm+label $asm #'id))
      ((org expr)
        (asm-with-org $asm (datum expr)))
      ((eq id expr)
        (identifier? #'id)
        (asm+value $asm #'id #'expr))
      ((u8 expr ...)
        (fold-left asm+u8 $asm (syntaxes expr ...)))))
)
