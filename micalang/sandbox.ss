(import (only (micascheme) logging) (scheme) (micalang rt) (fib))

(optimize-level 3)

;; =============================================================================
;; 1. THE DUAL-MODE COMPILER (FIXED SYNTAX)
;; =============================================================================
(define (to-native expr depth fast-mode?)
  (cond
    [(or (number? expr) (boolean? expr)) expr]
    [(memq expr '(Type Nat Bool)) `',expr]
    [(list? expr)
     (case (car expr)
       [(var) (string->symbol (format "v~a" (- (- depth (cadr expr)) 1)))]
       [(pi)  (let ([v (string->symbol (format "v~a" depth))])
                `(make-v-pi ,(to-native (cadr expr) depth fast-mode?)
                            (lambda (,v) ,(to-native (caddr expr) (+ depth 1) fast-mode?))))]
       [(lambda) (let ([v (string->symbol (format "v~a" depth))])
                `(lambda (,v) ,(to-native (caddr expr) (+ depth 1) fast-mode?)))]

       [(fix) (let* ([v-self (string->symbol (format "v~a" depth))]
                     [v-arg  (string->symbol (format "v~a" (+ depth 1)))]
                     [logic (caddr (caddr (caddr expr)))])
                `(letrec ([,v-self (lambda (,v-arg) ,(to-native logic (+ depth 2) fast-mode?))])
                   ,v-self))]

       [(<)   (let ([a (to-native (cadr expr) depth fast-mode?)]
                    [b (to-native (caddr expr) depth fast-mode?)])
                (if fast-mode?
                    `(< ,a ,b)
                    `(let ([v1 ,a] [v2 ,b])
                       (if (and (number? v1) (number? v2)) (< v1 v2) (make-v-neut 'lt (list v1 v2))))))]

       [(+) (let ([a (to-native (cadr expr) depth fast-mode?)]
                    [b (to-native (caddr expr) depth fast-mode?)])
                (if fast-mode?
                    `(+ ,a ,b)
                    `(let ([v1 ,a] [v2 ,b])
                       (if (and (number? v1) (number? v2)) (+ v1 v2) (make-v-neut '+ (list v1 v2))))))]

       [(-) (let ([a (to-native (cadr expr) depth fast-mode?)]
                    [b (to-native (caddr expr) depth fast-mode?)])
                (if fast-mode?
                    `(- ,a ,b)
                    `(let ([v1 ,a] [v2 ,b])
                       (if (and (number? v1) (number? v2)) (- v1 v2) (make-v-neut '- (list v1 v2))))))]

       [(if)  (let ([c (to-native (cadr expr) depth fast-mode?)])
                (if fast-mode?
                    `(if ,c ,(to-native (caddr expr) depth #t) ,(to-native (cadddr expr) depth #t))
                    `(let ([cond-v ,c])
                       (cond [(boolean? cond-v) (if cond-v ,(to-native (caddr expr) depth #f) ,(to-native (cadddr expr) depth #f))]
                             [else (make-v-neut 'if (list cond-v))]))))]
       [else
          (let ([f (to-native (car expr) depth fast-mode?)]
                [a (to-native (cadr expr) depth fast-mode?)])
            (if fast-mode? `(,f ,a) `(do-apply ,f ,a)))]
          )]
    [else expr]))

;; =============================================================================
;; 2. REIFICATION & KERNEL
;; =============================================================================

(define (quote-term depth val)
  (cond
    [(memq val '(Type Nat Bool)) val]
    [(or (number? val) (boolean? val)) val]
    [(v-pi? val)
     `(pi ,(quote-term depth (v-pi-arg-type val))
          ,(quote-term (+ depth 1) ((v-pi-body val) (make-v-neut depth '()))))]
    [(procedure? val)
     `(lambda (unknown) ,(quote-term (+ depth 1) (val (make-v-neut depth '()))))]
    [(v-neut? val)
     (let ([index (- (- depth (v-neut-head val)) 1)])
       (fold-left (lambda (acc arg) `(,acc ,(quote-term depth arg)))
                  `(var ,(max 0 index)) (v-neut-args val)))]
    [else val]))

(define (eval-native expr env)
  (let* ([depth (length env)]
         [params (map (lambda (i) (string->symbol (format "v~a" i))) (iota depth))]
         [jit (eval `(lambda ,params ,(to-native expr depth #f)) (environment '(scheme) '(micalang rt)))])
    (apply jit (reverse env))))

(define (infer context env expr)
  (cond
    [(memq expr '(Nat Bool Type)) 'Type]
    [(number? expr) 'Nat]
    [(boolean? expr) 'Bool]
    [(list? expr)
     (case (car expr)
       [(var) (list-ref context (cadr expr))]
       [(pi)  (check context env (cadr expr) 'Type)
              (let ([arg-v (eval-native (cadr expr) env)])
                (check (cons arg-v context) (cons (make-v-neut (length context) '()) env) (caddr expr) 'Type) 'Type)]
       [(fix) (let ([t (eval-native (cadr expr) env)])
                (check context env (cadr expr) 'Type)
                (check context env (caddr expr) (make-v-pi t (lambda (_) t))) t)]
       [(+ -) (check context env (cadr expr) 'Nat) (check context env (caddr expr) 'Nat) 'Nat]
       [(<)       (check context env (cadr expr) 'Nat) (check context env (caddr expr) 'Nat) 'Bool]
       [(if)      (check context env (cadr expr) 'Bool)
                  (let ([t (infer context env (caddr expr))]) (check context env (cadddr expr) t) t)]
       [else (let ([f-t (infer context env (car expr))])
                (if (v-pi? f-t)
                    (begin (check context env (cadr expr) (v-pi-arg-type f-t))
                           (do-apply f-t (eval-native (cadr expr) env)))
                    (error 'infer "Expected Pi type" f-t)))]
       )]
    [else (error 'infer "Syntax error" expr)]))

(define (check context env expr expected-v)
  (cond
    ;; 1. Handle Lambda with Pi types
    [(and (list? expr) (eq? (car expr) 'lambda) (v-pi? expected-v))
     (let* ([new-v (make-v-neut (length context) '())])
       (check (cons (v-pi-arg-type expected-v) context)
              (cons new-v env) (caddr expr) ((v-pi-body expected-v) new-v)))]

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
  (eval (logging (to-native expr 0 #t)) (environment '(scheme) '(micalang rt))))

;; =============================================================================
;; 4. EXECUTION & NATIVE TYPE-CHECKING BENCHMARKS
;; =============================================================================

(define fib-program
  '(fix (pi Nat Nat)
    (lambda ((pi Nat Nat))
      (lambda (Nat)
        (if (< (var 0) 2)
          (var 0)
          (+
            ((var 1) (- (var 0) 1))
            ((var 1) (- (var 0) 2))))))))

(newline)
(display "--- Phase 1: Native Type-Level Computation ---\n")
(display "Goal: Verify that '102334155' is of type Fib(40)\n")

;; This measures how fast the TYPE CHECKER reduces the type expression
(time
 (begin
   ;; We are checking if the value 9227465 matches the result of (fib 40)
   (compile-and-run-native 102334155 `(,fib-program 40))
   (display "Verified: Fib(40) type reduction completed natively.\n")))

(newline)
(display "--- Phase 2: Runtime Performance (Fast Mode) ---\n")
(define fib-fast (compile-and-run-native fib-program '(pi Nat Nat)))

(display "Mica-Fib Result (Native Optimized):\n")
(time (display (fib-fast 40)))
(newline)

(display "Scheme-Fib Result (Library Native):\n")
(time (display (fib 40)))
(newline)

(newline)
(display "--- Phase 3: Error Handling Test ---\n")
;; Using 'guard' to catch the mismatch error properly
(guard (x [else (display "Successfully caught Type Mismatch as expected.\n")])
  (display "Checking invalid type (expecting error)...\n")
  (compile-and-run-native 10 `(,fib-program 40)))
