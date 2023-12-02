(library (tico definition)
  (export
    definition definition? definition-key definition-value
    definition-map)
  (import (micascheme))

  (data (definition key value))

  (function (definition-map $fn $definition)
    (definition
      ($fn (definition-key $definition))
      ($fn (definition-value $definition))))
)
