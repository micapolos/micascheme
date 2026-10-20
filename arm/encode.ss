(library (arm encode)
  (export
    move-wide-immediate
    load/store-register
    load/store-immediate
    load/store-immediate++
    load/store-pair
    nop nop?)
  (import
    (scheme)
    (fixnum))

  (define (move-wide-immediate size1 op2 shift2 imm16 rd)
    (fxpack
      (1 size1)
      (2 op2)
      (6 #b100101)
      (2 shift2)
      (16 imm16)
      (5 rd)))

  (define (load/store-register size1 op9 rm option6 rn rt)
    (fxpack
      (1 1)
      (1 size1)
      (9 op9)
      (5 rm)
      (6 option6)
      (5 rn)
      (5 rt)))

  (define (load/store-immediate size1 op8 offset12 rn rt)
    (fxpack
      (1 1)
      (1 size1)
      (8 op8)
      (12 offset12)
      (5 rn)
      (5 rt)))

  (define (load/store-immediate++ size1 op9 offset9 pre1 rn rt)
    (fxpack
      (1 1)
      (1 size1)
      (9 op9)
      (9 offset9)
      (1 pre1)
      (1 1)
      (5 rn)
      (5 rt)))

  (define (load/store-pair size1 op8 offset7 rt2 rn rt)
    (fxpack
      (1 size1)
      (1 0)
      (8 op8)
      (7 offset7)
      (5 rt2)
      (5 rn)
      (5 rt)))

  (define (nop)
    (fxpack
      (3 #b110)
      (3 #b101)
      (14 #b01000000110010)
      (4 #b0000)
      (3 #b000)
      (5 #b11111)))

  (define (nop? $op)
    (fx= $op (nop)))
)
