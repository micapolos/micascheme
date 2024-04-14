(library (wasm ast)
  (export
    nop? nop
    unreachable? unreachable

    block? block
    block-result-type block-instructions
    block-with-result-type block-with-instructions

    loop? loop
    loop-result-type loop-instructions
    loop-with-result-type loop-with-instructions

    if? if
    if-result-type if-then-instructions if-else-instructions
    if-with-result-type if-with-then-instructions if-with-else-instructions

    br? br
    br-label-index
    br-with-label-index

    br-if? br-if
    br-if-label-index
    br-if-with-label-index

    br-table? br-table
    br-table-label-index-vector br-table-default-label-index
    br-table-with-label-index-vector br-table-with-default-label-index

    return? return

    call? call
    call-function-index
    call-with-function-index

    call-indirect? call-indirect
    call-indirect-type-index
    call-indirect-with-type-index

    instruction? instruction instruction-switch)
  (import (except (micascheme) if))

  (data (nop))
  (data (unreachable))
  (data (block result-type instructions))
  (data (loop result-type instructions))
  (data (if result-type then-instructions else-instructions))
  (data (br label-index))
  (data (br-if label-index))
  (data (br-table label-index-vector default-label-index))
  (data (return))
  (data (call function-index))
  (data (call-indirect type-index))

  (enum (instruction nop unreachable block loop if br br-if br-table return call call-indirect))
)
