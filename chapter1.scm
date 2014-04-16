;; compute square of x
(define (square x)
  (* x x))

;; compute sum of square a and square b
(define (sum-of-squares a b)
  (+ (square a)
     (square b)))

;; compute abs of x
(define (abs1 x)
  (cond ((< x 0) (- x))
	((= x 0) 0)
	((> x 0) x)))

(define (abs2 x)
  (cond ((< x 0) (- x))
	(else x)))

(define (abs3 x)
  (if (< x 0)
    (- x)
    x))

;; define >= operator
(define (>= x y)
  (not (< x y)))


;; Exercise 1.1
10
;; => 10
(+ 5 3 4)
;; => 12
(- 9 1)
;; => 8
(/ 6 2)
;; => 3
(+ (* 2 4) (- 4 6))
;; => 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
;; => 19
(= a b)
;; => #f
(if (and (> b a) (< b (* a b)))
  b
  a)
;; => 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; => 16
(+ 2 (if (> b a) b a))
;; => 6
(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))
;; => 16

;; Exercise 1.2
(+ 5
   4
   (* 3
      (- 2 (- 3 (+ 6 45)))
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3
(define (sum-of-squares-of-two-larger-ones x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
	((and (> x y) (> z y)) (sum-of-squares x z))
	((and (> y x) (> z x)) (sum-of-squares y z))))

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -)
   a
   b))
;; 将操作符当作返回值返回，类似于返回函数对象。

;; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))
;; (test 0 (p))
;; 解释器会进入假死状态，表明解释器使用的是application order。
;; 如果解释器使用的是normal order的话输出的结果是0。
;; 解释器假死是因为我们定义了p这个无限递归的程序调用，在(test 0 (p))
;; 被执行时，解释器首先需要计算(p)的值，随后便会进入无限递归，导致假死。
;; 而normal order的解释器在完全展开后根本不需要计算(p)的值，因而会正常运行，
;; 输出结果0。

;; Square Root by Newton's Method
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt1 x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    ;(display guess)
    ;(newline)
    ;(sleep (make-time 'time-duration 100000000 0))
    (average guess (/ x guess)))

  (sqrt-iter 1.0 x))

;; Exercise 1.6
(define (new-if predicate
		then-clause
		else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;; 	  guess
;; 	  (sqrt-iter (improve guess) x)))
;; 使用这个new-if来计算sqrt-iter会导致无限递归，
;; new-if作为一个普通的函数，它在执行时需要对它的两个参数都进行求值，
;; 这会导致每次的new-if都会需要计算(sqrt-iter (improve guess x) x)，
;; 而sqrt-iter内又有new-if，这个new-if又需要计算下一个sqrt-iter的值，
;; 从而导致无限递归。而if作为special form则不需要每次都预先执行sqrt-iter
;; 语句，因而不会产生无限递归的状况。

;; Exercise 1.7
;; 对于过小的x值，在计算good-enough?时，对guess的判断则更倾向于常数而不是x。
;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))
;; 由上可知，当x小于0.001时，(square guess)更为接近0.001而不是x；同时当x越接近0.001，
;; 它首0.001的影响也越大。
;; 对于过大的值，程序在获得满足条件的值之前可能因为improve计算时的精度问题导致每次产生
;; 的guess值都是其自身。其原因也很直观，当guess越来越接近真实值时，每次improve的量必然越来越小，
;; 当x较大时，guess的小数部分的位数就会减小，从而无法承载非常小的值。
;; 运行(sqrt1 1e13)即可看到具体的情况。
;; 以下是改进算法
(define (sqrt2 x)
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- guess (improve guess x))) 0.00000001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; 使用let来避免重复调用improve，使用block语法来避免省略参数x
(define (sqrt3 x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess improved-guess)
    (< (abs (- guess improved-guess)) 0.000000000001))
  (define (sqrt-iter guess)
    (let ((improved-guess (improve guess)))
      (if (good-enough? guess improved-guess)
	guess
	(sqrt-iter improved-guess))))
  (sqrt-iter 1.0))
;; 改进后的方案对极小和极大的数都有很好的效果

;; Exercise 1.8
(define (cube-root x)
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (define (good-enough? guess improved-guess)
    (< (abs (- guess improved-guess)) 0.000000000001))
  (define (cube-root-iter guess)
    (let ((improved-guess (improve guess)))
      (if (good-enough? guess improved-guess)
	guess
	(cube-root-iter improved-guess))))
  (cube-root-iter 1.0))

;; linear recursive factorial
(define (factorial1 n)
  (if (= n 1)
    1
    (* n (factorial1 (- n 1)))))

;; linear iterative factorial
(define (factorial2 n)
  (define (iter product counter max-count)
    (if (> counter max-count)
      product
      (iter (* product counter)
		 (+ counter 1)
		 max-count)))
  (iter 1 1 n))

;; chez scheme中trace调用的方法
;; > (trace factorial1)
;; > (factorial1 6)
;; > (untrace factorial1)

;; Exercise 1.9
;; 两个辅助函数
(define (inc n)
  (+ n 1))
(define (dec n)
  (- n 1))

;; 下面使用add来替代+作为函数名
(define (add1 a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))
(define (add2 a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))
;; 展开(add1 4 5)，recursive
;; (add1 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; 展开(add2 4 5)，iterative
;; (add2 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

;; Exercise 1.10
;; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))

(A 1 10)
;; => 1024
(A 2 4)
;; => 65536
(A 3 3)
;; => 65536
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))

;; (define (f n) (A 0 n))
;; f(n) = 2n
;; (define (g n) (A 1 n))
;; g(n) = 2^n
;; (define (h n) (A 2 n))
;; h(0) = 2
;; h(n) = 2^h(n-1)

;; tree recursive fibonacci
(define (fib1 n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib1 (- n 1))
		 (fib1 (- n 2))))))

;; iterative fibonacci
(define (fib2 n)
  (define (iter a b count)
    (if (= count 0)
      b
      (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

;; count change
(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
	  ((or (< amount 0) (= kinds-of-coins 0)) 0)
	  (else (+ (cc amount
		       (- kinds-of-coins 1))
		   (cc (- amount
			  (first-denomination
			    kinds-of-coins))
		       kinds-of-coins)))))
  (cc amount 5))

;; Exercise 1.11
;; recursive version
(define (f-recursive n)
  (if (< n 3)
    n
    (+ (f-recursive (- n 1))
       (* 2 (f-recursive (- n 2)))
       (* 3 (f-recursive (- n 3))))))
;; iterative version
(define (f-iterative n)
  (define (iter a b c count)
    (if (< count 3)
      a
      (iter (+ a (* 2 b) (* 3 c))
	    a
	    b
	    (- count 1))))
  (define (first-less-than-boundary boundary value)
    (if (< value boundary)
      value
      (first-less-than-boundary boundary (- value 1))))
  (if (< n 3)
    n
    (iter (first-less-than-boundary 3 n)
	  (first-less-than-boundary 2 n)
	  (first-less-than-boundary 1 n)
	  n)))

;; Exercise 1.12
;; 行和列都是从1开始计数
(define (pascal-triangle row col)
  (if (or (= col 1) (= row col))
    1
    (+ (pascal-triangle (- row 1) (- col 1))
       (pascal-triangle (- row 1) col))))

;; Exercise 1.14
;; (count-change 11)
;; > 4
;; Order of growth

;; Exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))
;; (a) p一共会被调用5次
;; (sine 12.15)
;; (p (sine 4.05))
;; (p (p (sine 1.35)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))
;; 到这里有0.05<0.1，延迟的p可以逐步执行了，可以看到p会被调用5次。
;; (b) Order of growth
;; 假设sine一共需要被调用n次，则我没有：
;; 	a <= 0.1*3^n
;;	=> n >= log_3(10*a)
;; 也就是说当输入每次增长3倍时，执行步骤会按线性增长。
;; 同理，所需要的空间也是一样，因为执行步骤每涨一步，都有一个p会被延迟计算。
;; 因而其复杂度为O(log n)。

;; 计算指数
(define (expt-recursive b n)
  (if (= n 0)
    1
    (* b (expt-recursive b (- n 1)))))
(define (expt-iterative b n)
  (define (iter product base count)
    (if (= count 0)
      product
      (iter (* product base) base (- count 1))))
  (iter 1 b n))

(define (my-even? n)
  (= (remainder n 2) 0))
(define (fast-expt-recursive-1 b n)
  (cond ((= n 0) 1)
	((even? n) (fast-expt-recursive-1 (square b) (/ n 2)))
	(else (* b
		 (fast-expt-recursive-1 b (- n 1))))))
(define (fast-expt-recursive-2 b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt-recursive-2 b (/ n 2))))
	(else (* b
		 (fast-expt-recursive-2 b (- n 1))))))

;; Exercise 1.16
(define (fast-expt-iterative b n)
  (define (iter product base counter)
    (cond ((= counter 0) product)
	  ((even? counter) (iter product
				 (square base)
				 (/ counter 2)))
	  (else (iter (* product base)
		      base
		      (- counter 1)))))
  (iter 1 b n))

;; Exercise 1.17
(define (double n)
  (+ n n))
(define (halve n)
  (/ n 2))
(define (mul a b)
  (if (= b 0)
    0
    (+ a (mul a (- b 1)))))
(define (fast-mul-recursive a b)
  (cond ((= b 0) 0)
	((even? b) (fast-mul (double a) (halve b)))
	(else
	  (+ a (fast-mul a (- b 1))))))

;; Exercise 1.18
(define (fast-mul-iterative a b)
  (define (iter product base counter)
    (cond ((= counter 0) product)
	  ((even? counter)
	   (iter product (double base) (halve counter)))
	  (else (iter (+ product base)
		      base
		      (- counter 1)))))
  (iter 0 a b))

;; Exercise 1.19
;; 将变换
;;   a <- bq + aq + ap
;;   b <- bp + aq
;; 连续带入两次并简化即可得到新的变换序列
;;   p' = p^2 + q^2
;;   q' = q^2 + 2pq
(define (fast-fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
	  ((even? count)
	   (iter a
		 b
		 (+ (square p) (square q))
		 (+ (square q) (* 2 p q))
		 (halve count)))
	  (else (iter (+ (* b q) (* a q) (* a p))
		      (+ (* b p) (* a q))
		      p
		      q
		      (- count 1)))))
  (iter 1 0 0 1 n))

;; 最大公约数, GCD
;; 复杂度theta(log n), lame's theorem
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;; Exercise 1.20
;; normal order 需要计算 1+2+4+7+4 = 18次
;(gcd 206 40)
;(if (= 40 0)
  ;206
  ;(gcd 40 (remainder 206 40)))

;(gcd 40 (remainder 206 40))
;(if (= (remainder 206 40) 0) ;; 6
  ;40
  ;(gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

;(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;(if (= (remainder 40 (remainder 206 40))) ;; 4
  ;(remainder 206 40)
  ;(gcd (remainder 40 (remainder 206 40))
       ;(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

;(gcd (remainder 40 (remainder 206 40))
     ;(remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ;; 2
  ;(remainder 40 (remainder 206 40))
  ;(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
       ;(remainder (remainder 40 (remainder 206 40))
		  ;(remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

;; applicative order需要计算 4次。
;(gcd 206 40)
;(if (= 40 0)
  ;206
  ;(gcd 40 (remainder 206 40)))

;(gcd 40 6)
;(if (= 6 0)
  ;40
  ;(gcd 6 (remainder 40 6)))

;(gcd 6 4)
;(if (= 4 0)
  ;6
  ;(gcd 4 (remainder 6 4)))

;(gcd 4 2)
;(if (= 2 0)
  ;4
  ;(gcd 2 (remainder 4 2)))

;; 质数检测
(define (divide? a b)
  (= (remainder b a) 0))
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divide? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))

;; 使用费马小定理来做质数检测
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (expmod (square base) (halve exp) m)
		    m))
	(else
	  (remainder (* base
			(expmod base (- exp 1) m))
				     m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))

;; Exercise 1.21
(smallest-divisor 199)
;; => 199
(smallest-divisor 1999)
;; => 1999
(smallest-divisor 19999)
;; => 7
