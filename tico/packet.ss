(library (tico packet)
  (export
    packet packet? packet-datum packet-value
    literal->packet
    datum->packet
    test-packet)
  (import
    (micascheme)
    (tico datum))

  (data (packet datum value))

  (define-syntax test-packet
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $name) (identifier? #'$name)
          #`(packet (quote (quote $name)) (quote $name))))))

  (define (literal->packet $literal)
    (packet $literal $literal))

  (define (datum->packet $datum)
    (packet $datum (datum->value $datum)))
)
