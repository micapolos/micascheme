(library (tico packet)
  (export
    packet packet? packet-datum packet-value
    literal->packet)
  (import
    (micascheme))

  (data (packet datum value))

  (define (literal->packet $literal)
    (packet $literal $literal))
)
