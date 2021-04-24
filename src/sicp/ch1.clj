(ns sicp.ch1
  (:import (java.lang Math))
  (:require [clojure.tools.trace :as trace]
            [sicp.util]))

;; Exercise 1.4
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

;; Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:
(defn p [] (p))

(defn test* [x y]
  (if (= x 0) 0 y))

;; >>: clojure is applicative order: it evaluates all arguments to "test" first. This causes infinite recursion with the function "p". In normal evaluation, "p" is not evaluated till it is needed (which is never).
(comment (test* 0 (p)))

;; Exercise 1.6
(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

(new-if (= 2 3) (print "yes") (print "no"))
(if (= 2 3) (print "yes") (print "no"))
(cond (= 2 3) (print "yes") :else (print "no"))

;; Exercise 1.7.  The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. 
(defn sqrt-iter [guess x]
  (letfn [(good-enough? [guess x]
            (< (Math/abs ^float (- (* guess guess) x))
               0.001))
          (average [x y]
            (/ (+ x y) 2))
          (improve [guess x]
            (average guess (/ x guess)))]
    (if (good-enough? guess x) guess
        (recur (improve guess x) x))))

(comment
  (sqrt-iter 1 4.0)
  (sqrt-iter 10 92400000000000000))
;; >>: arithmetic exception
(sqrt-iter 1 0.0005)

;; An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?
(defn sqrt-iter-2 [first-guess x]
  (letfn [(sqrt-iter-2* [prev-guess guess]
            (if (good-enough? prev-guess guess) guess
                (recur guess (improve guess))))
          (good-enough? [prev-guess guess]
            (< (Math/abs ^float (- guess prev-guess))
               0.001))
          (average [x y]
            (/ (+ x y) 2))
          (improve [guess]
            (average guess (/ x guess)))]
    (sqrt-iter-2* x first-guess)))

(sqrt-iter-2 1 0.0005)
;; >>: gives 0.02236... which is far more accurate!

;; Exercise 1.8
;; TODO

;; Exercise 1.9.  Each of the following two procedures defines a method for adding two positive integers in terms of the procedures inc, which increments its argument by 1, and dec, which decrements its argument by 1.
;; Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5) . Are these processes iterative or recursive?

(defn plus [a b]
  (if (= a 0)
    b
    (inc (plus (dec a) b))))

;; (plus 4 5)
;; (inc (plus 3 5))
;; (inc (inc (plus 2 5)))
;; (inc (inc (inc (plus 1 5))))
;; (inc (inc (inc (inc (plus 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

(defn plus2 [a b]
  (if (= a 0)
    b
    (recur (dec a) (inc b))))

;; (plus2 4 5)
;; (plus2 3 6)
;; (plus2 2 7)
;; (plus2 1 8)
;; (plus2 0 9)

;; note from clojure docs:
;; in functional languages looping and iteration are replaced/implemented via recursive function calls. Many such languages guarantee that function calls made in tail position do not consume stack space, and thus recursive loops utilize constant space. Since Clojure uses the Java calling conventions, it cannot, and does not, make the same tail call optimization guarantees. Instead, it provides the recur special operator, which does constant-space recursive looping by rebinding and jumping to the nearest enclosing loop or function frame. While not as general as tail-call-optimization, it allows most of the same elegant constructs, and offers the advantage of checking that calls to recur can only happen in a tail position.

;; let's look at the jvm bytecode!
;; first, for no recur:
;; lein compile :all
;; cd target/classes
;; javap -c sicp.ch1\$plus2     
;; Compiled from "ch1.clj"
;; public final class sicp.ch1$plus2 extends clojure.lang.AFunction {
;;   public static final clojure.lang.Var const__2;

;;   public sicp.ch1$plus2();
;;     Code:
;;        0: aload_0
;;        1: invokespecial #9                  // Method clojure/lang/AFunction."<init>":()V
;;        4: return

;;   public static java.lang.Object invokeStatic(java.lang.Object, java.lang.Object);
;;     Code:
;;        0: aload_0
;;        1: lconst_0
;;        2: invokestatic  #17                 // Method clojure/lang/Util.equiv:(Ljava/lang/Object;J)Z
;;        5: ifeq          15
;;        8: aload_1
;;        9: aconst_null
;;       10: astore_1
;;       11: goto          41
;;       14: athrow
;;       15: getstatic     #21                 // Field const__2:Lclojure/lang/Var;
;;       18: invokevirtual #27                 // Method clojure/lang/Var.getRawRoot:()Ljava/lang/Object;
;;       21: checkcast     #29                 // class clojure/lang/IFn
;;       24: aload_0
;;       25: aconst_null
;;       26: astore_0
;;       27: invokestatic  #35                 // Method clojure/lang/Numbers.dec:(Ljava/lang/Object;)Ljava/lang/Number;
;;       30: aload_1
;;       31: aconst_null
;;       32: astore_1
;;       33: invokestatic  #38                 // Method clojure/lang/Numbers.inc:(Ljava/lang/Object;)Ljava/lang/Number;
;;       36: invokeinterface #41,  3           // InterfaceMethod clojure/lang/IFn.invoke:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;
;;       41: areturn

;;   public java.lang.Object invoke(java.lang.Object, java.lang.Object);
;;     Code:
;;        0: aload_1
;;        1: aconst_null
;;        2: astore_1
;;        3: aload_2
;;        4: aconst_null
;;        5: astore_2
;;        6: invokestatic  #50                 // Method invokeStatic:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;
;;        9: areturn

;;   public static {};
;;     Code:
;;        0: ldc           #53                 // String sicp.ch1
;;        2: ldc           #55                 // String plus2
;;        4: invokestatic  #61                 // Method clojure/lang/RT.var:(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;
;;        7: checkcast     #23                 // class clojure/lang/Var
;;       10: putstatic     #21                 // Field const__2:Lclojure/lang/Var;
;;       13: return
;; }

;; Now, with tail call optimization
;; Compiled from "ch1.clj"
;; public final class sicp.ch1$plus2 extends clojure.lang.AFunction {
;;   public sicp.ch1$plus2();
;;     Code:
;;        0: aload_0
;;        1: invokespecial #9                  // Method clojure/lang/AFunction."<init>":()V
;;        4: return

;;   public static java.lang.Object invokeStatic(java.lang.Object, java.lang.Object);
;;     Code:
;;        0: aload_0
;;        1: lconst_0
;;        2: invokestatic  #17                 // Method clojure/lang/Util.equiv:(Ljava/lang/Object;J)Z
;;        5: ifeq          15
;;        8: aload_1
;;        9: aconst_null
;;       10: astore_1
;;       11: goto          32
;;       14: athrow
;;       15: aload_0
;;       16: aconst_null
;;       17: astore_0
;;       18: invokestatic  #23                 // Method clojure/lang/Numbers.dec:(Ljava/lang/Object;)Ljava/lang/Number;
;;       21: aload_1
;;       22: aconst_null
;;       23: astore_1
;;       24: invokestatic  #26                 // Method clojure/lang/Numbers.inc:(Ljava/lang/Object;)Ljava/lang/Number;
;;       27: astore_1
;;       28: astore_0
;;       29: goto          0
;;       32: areturn

;;   public java.lang.Object invoke(java.lang.Object, java.lang.Object);
;;     Code:
;;        0: aload_1
;;        1: aconst_null
;;        2: astore_1
;;        3: aload_2
;;        4: aconst_null
;;        5: astore_2
;;        6: invokestatic  #36                 // Method invokeStatic:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;
;;        9: areturn

;;   public static {};
;;     Code:
;;        0: return
;; }
;; 
;; we see on line 29 a goto 0 (loop) rather than a function call ()line 36 invokeinterface). So indeed, there is tail call optimization going on here!
;; 


;; Exercise 1.10.  The following procedure computes a mathematical function called Ackermann's function.

(defn ackermann [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (recur (dec x)
                     (ackermann x (dec y)))))

(ackermann 1 10) ;; > 1024
(ackermann 2 4) ;; > 65536
(ackermann 3 3) ;; > 65536

;; (define (f n) (A 0 n))
;; (define (g n) (A 1 n))
;; (define (h n) (A 2 n))
;; (define (k n) (* 5 n n))
;; Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values of n.
;; f = 2n
;; 
;; g = (A 1 n) = (A 0 (A 1 (dec n))) = 2 (A 1 (dec n))
;; 2^n
;; 
;; h = (A 2 n) = (A 1 (A 2 (dec n))) = (A 1 (A 1 ... (A 1 (A 2 0))))
;;   = 2^2^... n times
;; this looks different than the definition on wikipedia?

;; 1.2.2 Tree Recursion
(defn fibonacci [n]
  (memoize ;; with this, (fib 30 takes 2.3 msecs rather than 253 msecs!)
   (fn [n]
     (cond (= n 0) 0
           (= n 1) 1
           :else (+ (fibonacci (dec n)) (fibonacci (- n 2)))))))

;; AH! The key idea here is to keep as much state variables as we have recursive calls, and use them as a queue, with the first presenting the most recent computation, and so on:

(defn fibonacci* [n]
  (letfn [(fib-iter [a b count]
            (if (zero? count) b
                (recur (+ a b) a (dec count))))]
    (fib-iter 1 0 n)))

(time (fibonacci 30))
(time (fibonacci* 30)) ;; fastest of all, 0.147 msecs (loop doesn't change runtime)

;; Exercise 1.11.  A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.
(defn ex111-r [n]
  (cond (< n 3) n
        (>= n 3) (+ (ex111-r (- n 1))
                    (* 2 (ex111-r (- n 2)))
                    (* 3 (ex111-r (- n 3))))
        :else (throw (new AssertionError "invalid n"))))

(ex111-r 10)

(defn ex111-i [n]
  (let [iter (fn [a b c count]
               (if (zero? count) c
                   (recur b c (+ (* 3 a) (* 2 b) c) (dec count))))]
    (iter 0 1 2 (- n 2))))

(ex111-i 10)

;; Exercise 1.12. Write a procedure that computes elements of Pascal's triangle by means of a recursive process.
(defn pascal [n]
  (let [pp (fn [i prev]
             (cond (< n 0) (throw (new AssertionError "invalid n"))
                   (= n 1) [1]
                   (= n 2) [1 1]
                   (= i n) prev
                   :else (let [pairs (partition 2 1 prev)
                               new-inner (map #(apply + %) pairs)]
                           (recur (inc i) (concat [1] new-inner [1])))))]
    (pp 1 [])))

(assert (= (pascal 5) [1 4 6 4 1]))

;; TODO skipped 1.14 and 1.15

;; Exercise 1.16  Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt.

(defn fast-expt [b n]
  (cond (zero? n) 1
        (even? n) (* (fast-expt b (/ n 2)) (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))
(fast-expt 2 5)

(defn fast-expt-iter [b n]
  ;; a﹒b^n is invariant
  (let [f (fn [b n a]
            (cond
              (= n 0) a
              (even? n) (recur (* b b) (/ n 2) a)
              :else (recur b (dec n) (* b a))))]
    (f b n 1)))
(fast-expt-iter 2 5)

;; Exercise 1.17 In a similar way, one can perform integer multiplication by means of repeated addition. This algorithm takes a number of steps that is linear in b. Now suppose we include, together with addition, operations double, which doubles an integer, and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.

(defn fast-mult [a b]
  (let [double (fn [a] (* a 2))
        halve (fn [a] (/ a 2))]
    (cond
      (zero? b) 0
      (even? b) (fast-mult (double a) (halve b))
      :else (+ a (fast-mult a (dec b))))))

(fast-mult 11 3)

;; Exercise 1.18.  Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.40

;; After thinking of the invariant, it comes naturally.
(defn fast-mult-iter [a b n]
  ;; a﹒b + n is invariant
  (let [double (fn [a] (* a 2))
        halve (fn [a] (/ a 2))]
    (cond
      (zero? b) n
      (even? b) (recur (double a) (halve b) n)
      :else (recur a (dec b) (+ n a)))))
(fast-mult-iter 11 3 0)

;; Exercise 1.19

(defn fib-iter [a b p q count]
  (cond (= count 0) b
        (even? count) (recur
                       a
                       b
                       (+ (* q q) (* p p))
                       (+ (* q q) (* 2 p q))
                       (/ count 2))
        :else (recur (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (dec count))))

(fib-iter 1 0 0 1 10)

;; Ex 1.20 TODO
;; (define (gcd a b)
;;  (if (= b 0)
;;    a
;;    (gcd b (remainder a b))))
;; gcd(206, 40)

(defn gcd [a b]
  (if (= b 0) a
      (gcd b (mod a b))))

(gcd 10 7)

;; Exercise 1.21.  Use the smallest-divisor procedure to find the smallest divisor of each of the following numbers: 199, 1999, 19999.

(defn smallest-divisor-slow [n]
  (first (filter #(zero? (mod n %))
                 (range 2 (inc n)))))

(defn smallest-divisor [n]
  (first (filter #(zero? (mod n %))
                 (concat '(2) (range 3 (inc n) 2)))))

(smallest-divisor 199) ;; 199
(smallest-divisor 1999) ;; 1999
(smallest-divisor 19999) ;; 7

;; Ex 1.23
(defn prime-slow? [n] (= n (smallest-divisor-slow n)))
(defn prime? [n] (= n (smallest-divisor n)))
(time (dotimes [n 10] (prime? 1000037))) ;; 555 ms
(time (dotimes [n 10] (prime-slow? 1000037))) ;; 1073 ms
;; 2x speedup, as expected!


;; Ex 1.21
(defn fermat-test [n]
  (let [expmod (fn expmod [base exp m]
                 (cond (zero? exp) 1
                       (even? exp) (let [x (expmod base (/ exp 2) m)] (mod (* x x) m))
                       :else (mod (* base (expmod base (dec exp) m)) m)))
        try-it  (fn [a] (= (expmod a n n) a))]
    (try-it (+ 1 (rand-int (dec n))))))
;; Uses the mod property 

(fermat-test 19999)

(defn fast-prime? [n times]
  (cond (zero? times) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))

;; Ex 1.22
(defn search-for-primes [lower-bound num-primes-wanted]
  (take num-primes-wanted
        (filter #(fast-prime? % 8)
                (filter odd? (range lower-bound (java.lang.Integer/MAX_VALUE))))))

(defn search-for-primes-slow [lower-bound num-primes-wanted]
  (take num-primes-wanted
        (filter prime?
                (filter odd? (range lower-bound (java.lang.Integer/MAX_VALUE))))))

(time (doall (search-for-primes-slow 1000 3))) ;;/ (1009 1013 1019) 0.6 msecs"
(time (doall (search-for-primes-slow 10000 3))) ;; (10007 10009 10037) 4 msecs
(time (doall (search-for-primes-slow 100000 3))) ;; (100003 100019 100043) 30 msecs"
(time (doall (search-for-primes-slow 1000000 3))) ;; (1000003 1000033 1000037) 224 ms

;; Ex 1.24
(trace/trace (time (doall (search-for-primes 1000 3)))) ;; (1009 1013 1019) 0.24 msecs"
(time (doall (search-for-primes 10000 3))) ;; (10007 10009 10037) 0.50 msecs
(time (doall (search-for-primes 100000 3))) ;; (100003 100019 100043) 0.57 msecs"
(time (doall (search-for-primes 1000000 3))) ;; (1000003 1000033 1000037) 0.6 msecs"

;; Exercise 1.25.  Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod. After all, she says, since we already know how to compute exponentials, we could have simply written: 
;; (define (expmod base exp m)
;;     (remainder (fast-expt base exp) m))

;; Theoretically (in terms of math), it would work. Computationally it fails since the numbers (without the intermediate mods) are too large and either overflow or take too long.

;;  Exercise 1.26
;;  Previously, the process was linear recursive and you halved the search space on each level (the /2). Now, every time you halve the seach space, you make 2 recursive calls at the same level. So the runtime is now O(N) instead of O(log N).s

;; Exercise 1.27
;; 47 Numbers that fool the Fermat test are called Carmichael numbers, and little is known about them other than that they are extremely rare. There are 255 Carmichael numbers below 100,000,000. The smallest few are 561, 1105, 1729, 2465, 2821, and 6601. Demonstrate that these fool the fermat test.
(defn exhaustive-fermat-test [n]
  (let [expmod (fn expmod [base exp m]
                 (cond (zero? exp) 1
                       (even? exp) (let [x (expmod base (/ exp 2) m)] (mod (* x x) m))
                       :else (mod (* base (expmod base (dec exp) m)) m)))
        try-it  (fn [a] (= (expmod a n n) a))]
    (every? true? (map try-it (range 2 n)))))

(exhaustive-fermat-test 561) ;; true
(exhaustive-fermat-test 1105) ;; true

;; Exercise 1.28
;; Proof that if there is nontrivial square root of n it is not prime
;; https://crypto.stanford.edu/pbc/notes/numbertheory/poly.html
(defn miller-rabin [n]
  (let [expmod (fn expmod [base exp m]
                 (cond (zero? exp) 1
                       (even? exp) (let [x (expmod base (/ exp 2) m)
                                         sq (mod (* x x) m)]
                                     (if (and (not= x 1)
                                              (not= x (dec m))
                                              (= sq 1)) 0
                                         sq))
                       :else (mod (* base (expmod base (dec exp) m)) m)))
        try-it  (fn [a] (= (expmod a (dec n) n) 1))]
    (try-it 3)
    ;; (every? true? (map try-it (range 2 n)))
    ))

(miller-rabin 561)
(miller-rabin 1105)
(miller-rabin 1729)

;; Exercise 1.29
(defn summation [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (summation term (next a) next b))))

(defn integral [f a b dx]
  (let [add-dx (fn [x] (+ x dx))]
    (* (summation f (+ a (/ dx 2.0)) add-dx b) dx)))

(defn cube [x] (* x x x))

(integral cube 0 1 0.01) ; 0.24998750...


(defn simpsons-rule-integral [f a b n]
  (:pre [(even? n)])
  (let [h (/ (- b a) n)
        f* (fn [k]
             (let [multiplier
                   (cond (zero? k) 1
                         (= n k) 1
                         (even? k) 2
                         :else 4)] (* multiplier (f (+ a (* k h))))))
        summ (summation f* 0 inc n)]
    (* (/ h 3) summ)))

(float (simpsons-rule-integral cube 0 1 100)); 0.25
(float (simpsons-rule-integral cube 0 1 1000)); 0.25

;; Exercise 1.30
(defn summation-iter [term a next b]
  (let [iter (fn [a result]
               (if (> a b) result
                   (recur (next a) (+ (term a) result))))]
    (iter a 0)))

(summation-iter cube 1 inc 3)

;; Exercise 1.31
(defn product-recur [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product-recur term (next a) next b))))

(product-recur cube 1 inc 3)

(defn product-iter [term a next b]
  (let [iter (fn [a result]
               (if (> a b) result
                   (recur (next a) (* (term a) result))))]
    (iter a 1)))

(product-iter cube 1 inc 3)

(defn factorial* [n]
  (product-iter identity 1 inc n))

(factorial* 4)

(defn wallis-pi [num-terms]
  (let [even-t (fn [k] (cond
                         (odd? k) (+ 3 k)
                         :else (+ 2 k)))
        odd-t #p (fn [k] (cond
                           (even? k) (+ 3 k)
                           :else (+ 2 k)))]
    (* 4 (product-iter #(/ (even-t %) (odd-t %)) 0 inc num-terms))))

(float (wallis-pi 1000));; 3.1410027


;; Exercise 1.32
;; Show that sum and product (exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:
;; (accumulate combiner null-value term a next b)

(defn accumulate-recur [combiner null-value term a next b]
  (if (> a b) null-value
      (combiner (term a) (accumulate-recur combiner null-value term (next a) next b))))

(defn product-acc-recur [term a next b]
  (accumulate-recur * 1 term a next b))

(product-acc-recur cube 1 inc 3)

(defn accumulate-iter [combiner null-value term a next b]
  (let [iter (fn [a result]
               (if (> a b) result
                   (recur (next a) (combiner (term a) result))))]
    (iter a null-value)))

(defn product-acc-iter [term a next b]
  (accumulate-iter * 1 term a next b))

(product-acc-iter cube 1 inc 3)

;; Exercise 1.33
(defn accumulate-iter-filtered [combiner pred null-value term a next b]
  (let [iter (fn [a result]
               (if (> a b) result
                   (recur (next a) (if (pred a) (combiner (term a) result) result))))]
    (iter a null-value)))

;; sum of squares of primes in interval a to b
(accumulate-iter-filtered + prime? 0 #(* % %) 4 inc 10)

;; the product of all the positive integers less than n that are relatively prime to n
(accumulate-iter-filtered * #(= (gcd % 10) 1) 1 identity 0 inc 10)

;; Exercise 1.34
(defn f [g] (g 2))
;; (f f) gives:
;; Execution error (ClassCastException) at sicp.ch1/f (form-init2843314859918724098.clj:581).
; class java.lang.Long cannot be cast to class clojure.lang.IFn (java.lang.Long is in module java.base of loader 'bootstrap'; clojure.lang.IFn is in unnamed module of loader 'app')

;; Exercise 1.35
;; definition of phi is phi^2 = 1 + phi, so phi = 1/phi + 1

(def tolerance 1E-5)
(defn fixed-point [f first-guess]
  (let [close-enough? (fn [v1 v2] (< (Math/abs ^float (- v1 v2)) tolerance))
        try* (fn [guess steps] (let [next (f guess)]
                                 (if (close-enough? guess next)
                                   (do (print (str "took " steps " steps, guess=" guess))
                                       next)
                                   (recur next (inc steps)))))]
    (try* first-guess 0)))

(defn phi-fp [x] (+ 1 (/ 1 x)))
(float (fixed-point phi-fp 1))

;; Exercise 1.36
;; Then find a solution to x^x = 1000 by finding a fixed point of x -> log(1000)/log(x). (Use Scheme's primitive log procedure, which computes natural logarithms.) Compare the number of steps this takes with and without average damping. (Note that you cannot start fixed-point with a guess of 1, as this would cause division by log(1) = 0.)

;; I modified fixed-point using the https://github.com/weavejester/hashp #p data reader to print out intermediate values.

(defn undampened-x-pow-x [x] (/ (Math/log 1000) (Math/log ^float x)))
(float (fixed-point undampened-x-pow-x 2))
;; took 28 steps
;; 4.5555634
(float (fixed-point #(/ (+ (undampened-x-pow-x %) %) 2) 2))
;; took 7 steps
;; 4.5555468
;; no oscillation at the start; nice!

;; phi is 1.6180
;; 1/phi = 0.61890

;; Exercise 1.37
(defn cont-frac [n d k]
  (let [n-t (n k)
        d-t (d k)
        iter* (fn iter* [i]
                (if (> i k) 0
                    (/ n-t (+ d-t (iter* (inc i))))))]
    (iter* 1)))

(float (cont-frac (fn [x] 1) (fn [x] 1) 11))
;; 11 iterations
;; Iterative version

;; TODO Skipped 1.38 and 1.39
(defn cont-frac-iter [n d k]
  (let [n-t (n k)
        d-t (d k)
        iter* (fn iter* [i res]
                (if (zero? i) res
                    (recur (dec i) (/ n-t (+ d-t res)))))]
    (iter* k 0)))

(float (cont-frac-iter (fn [x] 1) (fn [x] 1) 11))
;; This seems analogous to top-down dynamic programming vs bottom up dynamic programming.

;; Exercise 1.40
(def dx 0.00001)
(defn deriv [f]
  (fn [x] (/ (- (f (+ x dx)) (f x)) dx)))
((deriv (fn [x] (* x x))) 5)
(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))
(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))
(newtons-method (fn [x] (* (+ x 4) (- x 4))) -10)
(defn fixed-point-of-transform [g t guess]
  (fixed-point (t g) guess))
(defn cubic [a b c]
  (fn [x] (+ (sicp.util/cube x) (* a (sicp.util/square x)) (* b x) c)))
(newtons-method (cubic 1 2 3) 1)

;; Exercise 1.41
(defn double* [f]
  (fn [x] (f (f x))))

(((double* (double* double*)) inc) 5)

;; Exercise 1.42
(defn compose* [f g]
  (fn [x] (f (g x))))

;; clojure has `comp` which handles
;; multi-arity arguments: ([x y z & args] (f (apply g x y z args)))))
;; multiple-function composition:([f g & fs]
;;    (reduce1 comp (list* f g fs))))

;; Exercise 1.43
(defn repeated [f n]
  (if (zero? n) identity
      (compose* f (repeated f (dec n)))))

((repeated sicp.util/square 2) 5)

;; Exercise 1.44
(defn smooth [f]
  (fn [x] (/ (+ (f x)
                (f (+ x dx))
                (f (- x dx)))) 3))

(defn n-smooth [f n]
  ((repeated smooth n) f))

;; Exercise 1.45
(defn dampened-root [x num-root num-repeat]
  (fixed-point-of-transform
   (fn [y] (sicp.util/average y (/ x (Math/pow ^double y ^double (dec num-root)))))
   #(repeated % num-repeat)
   2))

;; (float (dampened-root 2 4 1)) don't run, does not converge.
(float (dampened-root 2 4 2)) ;; 1.2013538948532418

;; Exercise 1.46
(defn iterative-improver [test-fn improve-fn]
  (fn [guess] (let [improved-guess (improve-fn guess)
                    good-enough? (test-fn guess improved-guess)]
                (if good-enough? improved-guess
                    (recur improved-guess)))))

(defn sqrt-final [num]
  (let [guesser (iterative-improver
                 (fn [a b] (< (Math/abs ^float (- a b))
                              0.001))
                 (fn [guess] (sicp.util/average guess (/ num guess))))]
    (guesser 1)))

(float (sqrt-final 4001))