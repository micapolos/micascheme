(library (tico packet)
  (export
    packet packet? packet-definitions packet-items
    packet-map)
  (import
    (micascheme)
    (tico definition))

  (data (packet definitions items))

  (define (packet-map $fn $packet)
    (packet
      (map (partial definition-map $fn) (packet-definitions $packet))
      (map $fn (packet-items $packet))))
)
