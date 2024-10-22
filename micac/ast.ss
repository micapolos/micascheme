(library (micac ast)
  (export
    bool bool?
    u8 u8?
    u16 u16?
    u32 u32?
    struct struct? struct-types

    type type? type-switch

    const const? const-value
    term term? term-switch

    expr expr? expr-type expr-term

    block block? block-types block-statements
    return return? return-expr

    statement statement? statement-switch)
  (import (micascheme))

  (data (bool))
  (data (u8))
  (data (u16))
  (data (u32))
  (data (struct types))
  (enum (type bool u8 u16 u32 struct))

  (data (const value))
  (enum (term const))

  (data (expr type term))

  (data (block types statements))
  (data (return expr))
  (enum (statement block return))
)
