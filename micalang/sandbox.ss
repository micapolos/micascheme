(import (only (micascheme) logging displayln) (scheme) (micalang rt) (fib))

(optimize-level 3)

(define (depth->symbol depth)
  (string->symbol (format "v~a" depth)))

(define native-environment
  (environment '(micascheme) '(micalang rt) '(curry)))

(define (literal? expr)
  (or (number? expr) (boolean? expr)))

(define globals
  `(
    (Type ,Type                 Type)
    (Bool ,Type                 Bool)
    (Nat  ,Type                 Nat)
    (inc  ,(v-pi Nat Nat)       inc)
    (dec  ,(v-pi Nat Nat)       dec)
    (not  ,(v-pi Bool Bool)     not)
    (<    ,(v-pi Nat Nat Bool)  curry<)
    (+    ,(v-pi Nat Nat Nat)   curry+)
    (-    ,(v-pi Nat Nat Nat)   curry-)))

;; =============================================================================
;; 1. THE DUAL-MODE COMPILER (FIXED SYNTAX)
;; =============================================================================
(define (to-native expr depth fast-mode?)
  (cond
    [(literal? expr) expr]
    [(symbol? expr) (caddr (assq expr globals))]
    [(list? expr)
     (case (car expr)
       [(var)
          (depth->symbol (- depth (cadr expr) 1))]

       [(pi)
          (let ([v (depth->symbol depth)])
            `(v-pi
              (,v ,(to-native (cadr expr) depth fast-mode?))
              ,(to-native (caddr expr) (+ depth 1) fast-mode?)))]

       [(lambda)
          (let ([v (depth->symbol depth)])
            `(lambda (,v)
              ,(to-native (caddr expr) (+ depth 1) fast-mode?)))]

       [(fix)
          (let*
            ([v-self (depth->symbol depth)]
             [v-arg  (depth->symbol (+ depth 1))]
             [t (cadr expr)]
             [logic (caddr expr)])
            `(letrec
              ([,v-self
                (lambda (,v-arg)
                  ,(to-native logic (+ depth 2) fast-mode?))])
              ,v-self))]

       [(if)
          (let
            ([c (to-native (cadr expr) depth fast-mode?)]
             [a (to-native (caddr expr) depth fast-mode?)]
             [b (to-native (cadddr expr) depth fast-mode?)])
            (if fast-mode?
              `(if ,c ,a ,b)
              `(let [(cond-v ,c)]
                (cond
                  [(boolean? cond-v) (if cond-v ,a ,b)]
                  [else (v-neut 'if cond-v)]))))]

       [else
          (fold-left
            (lambda (acc arg)
              (let
                [(arg (to-native arg depth fast-mode?))]
                (if fast-mode? `(,acc ,arg) `(do-apply ,acc ,arg))))
            (to-native (car expr) depth fast-mode?)
            (cdr expr))])]
    [else expr]))

;; =============================================================================
;; 2. REIFICATION & KERNEL
;; =============================================================================

(define (quote-term depth val)
  (cond
    [(literal? val) val]
    [(symbol? val) val]
    [(v-pi? val)
     `(pi ,(quote-term depth (v-pi-arg-type val))
          ,(quote-term (+ depth 1) ((v-pi-body val) (v-neut depth))))]
    [(procedure? val)
     `(lambda unknown-type
      ,(quote-term (+ depth 1) (val (v-neut depth))))]
    [(v-neut? val)
     (let ([index (- depth (v-neut-head val) 1)])
      `(
        (var ,(max 0 index))
        ,@(map
          (lambda (x) (quote-term depth x))
          (reverse (v-neut-args val)))))]
    [else val]))

(define (eval-native expr env)
  (let* ([depth (length env)]
         [params (map (lambda (i) (depth->symbol i)) (iota depth))]
         [jit (eval `(lambda ,params ,(to-native expr depth #f)) native-environment)])
    (apply jit (reverse env))))

(define (infer context env expr)
  (cond
    [(number? expr) 'Nat]
    [(boolean? expr) 'Bool]
    [(symbol? expr)
      (cadr
        (or
          (assq expr globals)
          (error 'infer "Undefined" expr)))]
    [(list? expr)
     (case (car expr)
       [(var)
          (list-ref context (cadr expr))]
       [(pi)
          (check context env (cadr expr) 'Type)
          (let ([arg-v (eval-native (cadr expr) env)])
            (check
              (cons arg-v context)
              (cons (v-neut (length context)) env)
              (caddr expr)
              'Type)
            'Type)]
       [(fix)
          (let* ([t-expr (cadr expr)]
                 [t (eval-native `(pi ,t-expr ,t-expr) env)])
            (check context env t-expr 'Type)
            (check context env
              `(lambda (pi ,t-expr ,t-expr)
                (lambda ,t-expr
                  ,(caddr expr)))
              (v-pi t t))
            t)]
       [(if)
          (check context env (cadr expr) 'Bool)
          (let ([t (infer context env (caddr expr))])
            (check context env (cadddr expr) t)
            t)]
       [else
          (fold-left
            (lambda (acc x)
              (cond
                [(v-pi? acc)
                  (check context env (cadr expr) (v-pi-arg-type acc))
                  (do-apply acc (eval-native x env))]
                [else (error 'infer "Expected Pi type" acc)]))
            (infer context env (car expr))
            (cdr expr))]
       )]
    [else (error 'infer "Syntax error" expr)]))

(define (check context env expr expected-v)
  (cond
    ;; 1. Handle Lambda with Pi types
    [(and (list? expr) (eq? (car expr) 'lambda) (v-pi? expected-v))
      (let* ([new-v (v-neut (length context))])
        (check
          (cons (v-pi-arg-type expected-v) context)
          (cons new-v env)
          (caddr expr)
          ((v-pi-body expected-v) new-v)))]

    ;; 2. NEW: Literal equality (Crucial for Phase 1)
    ;; If the expected "type" is a number, and our expression is that same number, it passes.
    [(and (number? expected-v) (number? expr) (= expected-v expr))
      #t]

    ;; 3. Fallback to standard inference comparison
    [else
      (let* ([actual-v (infer context env expr)]
             [d (length context)]
             [s1 (quote-term d actual-v)]
             [s2 (quote-term d expected-v)])
       (unless (equal? s1 s2)
         (error 'check (format "Type Mismatch! Got ~a, expected ~a" s1 s2))))]))

;; =============================================================================
;; 3. UNIFIED ENTRY POINT
;; =============================================================================

(define (compile-and-run-native expr type-expr)
  ;; STEP 1: Type checking (Symbolic Reduction)
  (check '() '() expr (eval-native type-expr '()))
  ;; STEP 2: Compilation (Native Fast-Mode)
  (eval (logging (to-native expr 0 #t)) native-environment))

;; =============================================================================
;; 4. EXECUTION & NATIVE TYPE-CHECKING BENCHMARKS
;; =============================================================================

(define fib-program
  '(fix Nat
    (if (< (var 0) 2)
      (var 0)
      (+
        ((var 1) (- (var 0) 1))
        ((var 1) (- (var 0) 2))))))

(displayln "--- Phase 0: Small programs ---")
(displayln (compile-and-run-native '(inc (inc 0)) 'Nat))
(displayln (compile-and-run-native '(dec (dec 0)) 'Nat))
(displayln (compile-and-run-native '(not #t) 'Bool))

(displayln (compile-and-run-native '((< 10) 20) 'Bool))
(displayln (compile-and-run-native '((+ 10) 20) 'Nat))
(displayln (compile-and-run-native '((- 10) 20) 'Nat))

(displayln (compile-and-run-native '(pi Nat Nat) 'Type))
(displayln (compile-and-run-native '(pi Type (var 0)) 'Type))
(displayln (compile-and-run-native '(pi Type (pi (var 0) (var 1))) 'Type))

(newline)
(displayln "--- Phase 1: Native Type-Level Computation ---")
(displayln "Goal: Verify that '102334155' is of type Fib(40)")

;; This measures how fast the TYPE CHECKER reduces the type expression
(time
 (begin
   ;; We are checking if the value 9227465 matches the result of (fib 40)
   (compile-and-run-native 102334155 `(,fib-program 40))
   (displayln "Verified: Fib(40) type reduction completed natively.")))

(newline)
(displayln "--- Phase 2: Runtime Performance (Fast Mode) ---")
(define fib-fast (compile-and-run-native fib-program '(pi Nat Nat)))

(displayln "Mica-Fib Result (Native Optimized):")
(time (displayln (fib-fast 40)))

(displayln "Scheme-Fib Result (Library Native):")
(time (displayln (fib 40)))

(displayln "Scheme-Curry-Fib Result (Library Native):")
(time (displayln (curry-fib 40)))

(newline)
(displayln "--- Phase 3: Error Handling Test ---")
;; Using 'guard' to catch the mismatch error properly
(guard (x [else (displayln "Successfully caught Type Mismatch as expected.")])
  (displayln "Checking invalid type (expecting error)...")
  (compile-and-run-native 10 `(,fib-program 40)))
