(import
  (micascheme)
  (tico compiled))

; --- literal->compiled

(check
  (equal?
    (type-literal->compiled (number-type) 128)
    (compiled (globals)
      (typed (number-type)
        (packet
          (comptime 128)
          (runtime (constant 128)))))))

(check
  (equal?
    (boolean->compiled #f)
    (type-literal->compiled (boolean-type) #f)))

(check
  (equal?
    (number->compiled 128)
    (type-literal->compiled (number-type) 128)))

(check
  (equal?
    (string->compiled "foo")
    (type-literal->compiled (string-type) "foo")))

(check
  (equal?
    (literal->compiled #f)
    (boolean->compiled #f)))

(check
  (equal?
    (literal->compiled 128)
    (number->compiled 128)))

(check
  (equal?
    (literal->compiled "foo")
    (string->compiled "foo")))

; --- compiled-lets

(check
  (equal?
    (compiled-lets
      ($string (compiled (stack 'g1) "foo"))
      ($number (compiled (stack 'g2) 128))
      (compiled (stack 'g3) (cons $string $number)))
    (compiled (stack 'g1 'g2 'g3) (cons "foo" 128))))

; --- compiled-globalize

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiled-globalize
        (compiled
          (globals
            (symbolic 'g1
              (packet
                (comptime 'foo)
                (runtime "foo"))))
          (typed `t
            (packet
              (comptime `bar)
              (runtime (constant "bar")))))))
    (compiled
      (globals
        (symbolic 'g1
          (packet
            (comptime 'foo)
            (runtime "foo")))
        (symbolic '$tmp-0
          (packet
            (comptime 'bar)
            (runtime "bar"))))
      (typed `t
        (packet
          (comptime '$tmp-0)
          (runtime (constant "bar")))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiled-globalize
        (compiled
          (globals
            (symbolic 'g1
              (packet
                (comptime 'foo)
                (runtime "foo"))))
          (typed `t
            (packet
              (comptime `bar)
              (runtime (variable 3)))))))
    (compiled
      (globals
        (symbolic 'g1
          (packet
            (comptime 'foo)
            (runtime "foo"))))
      (typed `t
        (packet
          (comptime `bar)
          (runtime (variable 3)))))))
