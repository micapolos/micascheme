(library (tico packet)
  (export
    packet packet? packet-definitions packet-items
    packet-map)
  (import
    (micascheme)
    (tico definition))

  (data (packet definitions items))

  (function (packet-map $fn (packet $definitions $items))
    (packet
      (map (partial definition-map $fn) $definitions)
      (map $fn $items)))
)
