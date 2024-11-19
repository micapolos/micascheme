(micalog
 (input clock 1)
 (input reset? 1)
 (input mouse-pressed? 1)
 (input mouse-x 16)
 (internal half-clock (register 1 0 posedge clock inv-half-clock))
 (internal previous-half-clock (register 1 0 negedge clock half-clock))
 (internal next-half-clock (inv 16 previous-half-clock))
 (output counter (register 16 0 posedge half-clock next-counter))
 (internal previous-counter (register 16 0 negedge half-clock counter))
 (internal inc-counter (inc 16 previous-counter))
 (internal dec-counter (dec 16 previous-counter))
 (internal next-counter
   (cond 16
     (reset? mouse-x)
     (mouse-pressed? inc-counter)
     (else dec-counter))))
