(library (wasm ast)
  (export
    i32? i32
    i64? i64
    i i-switch

    s? s
    u? u
    sx sx-switch

    iconst? iconst
    iconst-i iconst-value

    iadd? iadd iadd-i
    isub? isub iadd-i
    iand? iand iadd-i
    ior? ior iadd-i
    ixor? ixor iadd-i
    ieqz? ieqz
    ieq? ieq
    ine? ine
    ilt? ilt ilt-i ilt-sx
    igt? igt igt-i igt-sx
    ile? ile ile-i ile-sx
    ige? ige ige-i ige-sx

    drop? drop
    select? select

    get-local? get-local get-local-index
    set-local? set-local set-local-index
    tee-local? tee-local tee-local-index
    get-global? get-global get-global-index
    set-global? set-global set-global-index

    iload? iload iload-i iload-sx
    istore? istore istore-i istore-sx

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

    br-if? br-if
    br-if-label-index

    br-table? br-table
    br-table-label-index-vector br-table-default-label-index
    br-table-with-label-index-vector br-table-with-default-label-index

    return? return

    call? call
    call-function-index

    call-indirect? call-indirect
    call-indirect-type-index

    instruction? instruction instruction-switch)
  (import (except (micascheme) if))

  (data (i32))
  (data (i64))
  (enum (i i32 i64))

  (data (s))
  (data (u))
  (enum (sx s u))

  (data (iconst i value))
  (data (iadd i))
  (data (isub i))
  (data (iand i))
  (data (ior i))
  (data (ixor i))
  (data (ieqz i))
  (data (ieq i))
  (data (ine i))
  (data (ilt i sx))
  (data (igt i sx))
  (data (ile i sx))
  (data (ige i sx))

  (data (drop))
  (data (select))

  (data (get-local index))
  (data (set-local index))
  (data (tee-local index))
  (data (get-global index))
  (data (set-global index))

  (data (iload i sx offset align))
  (data (istore i sx offset align))

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

  (enum
    (instruction
      iconst
      iadd isub iand ior ixor
      ieqz ieq ine
      ilt ilt igt igt

      drop select

      get-local set-local tee-local
      get-global set-global

      iload istore

      nop unreachable block loop if br br-if br-table return call call-indirect))
)
