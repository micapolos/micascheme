(library (tico thunk)
  (export
    compile-time compile-time? compile-time-value
    runtime runtime? runtime-term runtime-free-variable-count)
  (import (micascheme))

  (data (compile-time value))
  (data (runtime term free-variable-count))
  (enum (thunk comptime runtime))

  (define (runtimes-flatten $runtimes)
    (runtime
      )
)
