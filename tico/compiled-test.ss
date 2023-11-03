(import
  (micascheme)
  (tico type)
  (tico typed)
  (tico compiled)
  (tico expression))

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

; --- compiled-comptime

(check
  (equal?
    (compiled-comptime
      (compiled
        (globals
          (symbolic '$foo (packet (comptime 'foo) (runtime "foo")))
          (symbolic '$bar (packet (comptime 'bar) (runtime "bar"))))
        (typed `t
          (packet
            (comptime 'goo)
            (runtime (constant "goo"))))))
    '(lets
      ($foo foo)
      ($bar bar)
      goo)))

; --- struct

(check 
  (equal? 
    (variable-struct (list (variable 2) (variable 3)))
    (variable 3)))

(check
  (raises?
    (lambda ()
      (variable-struct (list)))))

(check
  (equal?
    (runtime-struct
      (globals 
        (symbolic 'foo 
          (packet (comptime "foo") (runtime "foo")))
        (symbolic 'bar
          (packet (comptime "bar") (runtime "bar"))))
      `(cons foo bar)
      (list 
        (runtime (constant "foo"))
        (runtime (constant "bar"))))
    (runtime
      (constant
        (comptime->runtime
          (globals 
            (symbolic 'foo 
              (packet (comptime "foo") (runtime "foo")))
            (symbolic 'bar
              (packet (comptime "bar") (runtime "bar"))))
          `(cons foo bar))))))

(check
  (equal?
    (runtime-struct
      (globals 
        (symbolic 'bar
          (packet (comptime "bar") (runtime "bar"))))
      `(cons foo bar)
      (list 
        (runtime (variable 3))
        (runtime (constant "bar"))))
    (runtime
      (variable 3))))

(check
  (equal?
    (comptime-struct
      (list
        (comptime 'foo)
        (comptime 'bar)))
    (comptime
      (tuple-expression
        (list 'foo 'bar)))))

(check
  (equal?
    (packet-struct
      (globals 
        (symbolic 'bar
          (packet (comptime "bar") (runtime "bar"))))
      (list
        (packet
          (comptime 'foo)
          (runtime (variable 3)))
        (packet
          (comptime 'bar)
          (runtime (constant "bar")))))
    (packet
      (comptime-struct
        (list (comptime 'foo) (comptime 'bar)))
      (runtime-struct 
        (globals
          (symbolic 'bar
            (packet (comptime "bar") (runtime "bar"))))
        (comptime-struct 
          (list 
            (comptime 'foo) 
            (comptime 'bar)))
        (list 
          (runtime (variable 3)) 
          (runtime (constant "bar")))))))

(check
  (equal?
    (typed-struct 'my-struct
      (globals
        (symbolic 'bar
          (packet (comptime "bar") (runtime "bar"))))
      (list
        (typed 
          (number-type)
          (packet
            (comptime 'foo)
            (runtime (variable 3))))
        (typed 
          (struct 'empty (list))
          #f)
        (typed 
          (string-type)
          (packet
            (comptime 'bar)
            (runtime (constant "bar"))))))
    (typed
      (struct 'my-struct
        (list
          (number-type)
          (struct 'empty (list))
          (string-type)))
      (packet-struct
        (globals
          (symbolic 'bar
            (packet (comptime "bar") (runtime "bar"))))
        (list
          (packet
            (comptime 'foo)
            (runtime (variable 3)))
          (packet
            (comptime 'bar)
            (runtime (constant "bar"))))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiled-struct 'foo
        (list
          (compiled-globalize (boolean->compiled #f))
          (compiled-globalize (number->compiled 128))
          (compiled-globalize (string->compiled "foo")))))
    (compiled
      (globals
        (symbolic '$tmp-0 (packet (comptime #f) (runtime #f)))
        (symbolic '$tmp-1 (packet (comptime 128) (runtime 128)))
        (symbolic '$tmp-2 (packet (comptime "foo") (runtime "foo"))))
      (typed
        (struct 'foo (list (boolean-type) (number-type) (string-type)))
        (packet
          (comptime '(vector $tmp-0 $tmp-1 $tmp-2))
          (runtime (constant (vector #f 128 "foo"))))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiled-struct 'foo
        (list
          (compiled-globalize (boolean->compiled #f))
          (pure-compiled (typed (number-type) (packet (comptime 'n) (runtime (variable 3)))))
          (compiled-globalize (string->compiled "foo")))))
    (compiled
      (globals
        (symbolic '$tmp-0 (packet (comptime #f) (runtime #f)))
        (symbolic '$tmp-1 (packet (comptime "foo") (runtime "foo"))))
      (typed
        (struct 'foo (list (boolean-type) (number-type) (string-type)))
        (packet
          (comptime '(vector $tmp-0 n $tmp-1))
          (runtime (variable 3)))))))

; --- typed-local

; typed-static
(check
  (equal?
    (typed-local (typed (value-type "foo") #f))
    (typed (value-type "foo") #f)))

; typed-dynamic
(check
  (equal?
    (with-generate-temporary-seed $tmp
      (typed-local
        (typed
          (string-type)
          (packet
            (comptime 'foo)
            (runtime (constant "foo"))))))
    (typed
      (string-type)
      (packet
        (comptime '$tmp-0)
        (runtime (constant "foo"))))))

; typed-variable
(check
  (equal?
    (with-generate-temporary-seed $tmp
      (typed-local
        (typed
          (string-type)
          (packet
            (comptime 'foo)
            (runtime (variable 3))))))
    (typed
      (string-type)
      (packet
        (comptime '$tmp-0)
        (runtime (hole))))))

; locals-pattern->typed-variable-opt

(let
  (($locals
    (locals
      (typed (value-type 't) #f)
      (typed (number-type) (packet (comptime '$num) (runtime (hole))))
      (typed (string-type) (packet (comptime '$str) (runtime (constant "foo")))))))

  (check
    (equal?
      (locals-pattern->typed-variable-opt $locals (value-type 't))
      (typed (value-type 't) #f)))

  (check
    (equal?
      (locals-pattern->typed-variable-opt $locals (number-type))
      (typed (number-type) (packet (comptime '$num) (runtime (variable 1))))))

  (check
    (equal?
      (locals-pattern->typed-variable-opt $locals (string-type))
      (typed (string-type) (packet (comptime '$str) (runtime (constant "foo"))))))

  (check
    (equal?
      (locals-pattern->typed-variable-opt $locals (any-type))
      (typed (string-type) (packet (comptime '$str) (runtime (constant "foo"))))))

  (check
    (equal?
      (locals-pattern->typed-variable-opt $locals (boolean-type))
      #f)))
