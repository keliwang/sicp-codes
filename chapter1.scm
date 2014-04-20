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
(define (Ackermann x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (Ackermann (- x 1) (Ackermann x (- y 1))))))

(Ackermann 1 10)
;; => 1024
(Ackermann 2 4)
;; => 65536
(Ackermann 3 3)
;; => 65536
;; (Ackermann 2 (Ackermann 3 2))
;; (Ackermann 2 (Ackermann 2 (Ackermann 3 1)))
;; (Ackermann 2 (Ackermann 2 2))

;; (define (f n) (Ackermann 0 n))
;; f(n) = 2n
;; (define (g n) (Ackermann 1 n))
;; g(n) = 2^n
;; (define (h n) (Ackermann 2 n))
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
(define (sine angle)
  (define (p x) (- (* 3 x) (* 4 (cube x))))
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
	((even? b) (fast-mul-recursive (double a) (halve b)))
	(else
	  (+ a (fast-mul-recursive a (- b 1))))))

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
(define true #t)
(define false #f)

;; 不要使用(square base)来递进，这会导致
;; base的值越来越大，导致计算速度减慢，
;; 将square放在expmod的外围，可以充分利用
;; remainder来减小计算量，因为expmod计算结果
;; 都只是值相对较小的余数。
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	   (square (expmod base (/ exp 2) m))
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
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

;; Exercise 1.21
(smallest-divisor 199)
;; => 199
(smallest-divisor 1999)
;; => 1999
(smallest-divisor 19999)
;; => 7

;; Exercise 1.22
(define (runtime-helper now)
  (+ (* (time-second now) 1e9) (time-nanosecond now)))
(define (runtime)
  (runtime-helper (current-time)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start count)
  (define (search start count)
    (if (not (= count 0))
      (cond ((prime? start)
	     (timed-prime-test start)
	     (search (+ start 2) (- count 1)))
	    (else
	      (search (+ start 2) count)))))
  (if (even? start)
    (search (+ start 1) count)
    (search start count)))

;; > (search-for-primes 1000 3)
;; 1009 *** 11776.0
;; 1013 *** 12544.0
;; 1019 *** 7936.0
;; > (search-for-primes 10000 3)
;; 10007 *** 33536.0
;; 10009 *** 20992.0
;; 10037 *** 20992.0
;; > (search-for-primes 100000 3)
;; 100003 *** 65536.0
;; 100019 *** 61184.0
;; 100043 *** 61440.0
;; > (search-for-primes 1000000 3)
;; 1000003 *** 192000.0
;; 1000033 *** 188416.0
;; 1000037 *** 188160.0

;; 由上面的结果可以看到prime?是符合
;; 预期的复杂度的，即√n。√10 = 3.1622。

;; Exercise 1.23
(define (smallest-divisor-using-next n)
  (define (next test-divisor)
    (if (= test-divisor 2)
      3
      (+ test-divisor 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divide? test-divisor n) test-divisor)
	  (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))
(define (prime?-with-next n)
  (= n (smallest-divisor-using-next n)))
(define (timed-prime-test-with-next n)
  (define (start-prime-test n start-time)
    (if (prime?-with-next n)
      (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  (newline)
  (display n)
  (start-prime-test n (runtime)))
;; --------------------------------------------------
;; |number  | old time (ns) | new time (ns) | ratio |
;; --------------------------------------------------
;; |1009    |    19200      |  16640        | 1.15  |
;; |1013    |    21248      |  18176        | 1.17  |
;; |1019    |    19456      |  17152        | 1.13  |
;; |10007   |    33536      |  30464        | 1.10  |
;; |10009   |    31744      |  25856        | 1.23  |
;; |10037   |    34304      |  24320        | 1.41  |
;; |100003  |    72448      |  50688        | 1.43  |
;; |100019  |    73472      |  51456        | 1.43  |
;; |100043  |    72448      |  51456        | 1.41  |
;; |1000003 |    200192     |  135424       | 1.48  |
;; |1000033 |    200704     |  136192       | 1.47  |
;; |1000037 |    200448     |  137216       | 1.46  |
;; |10000019|    641024     |  398848       | 1.61  |
;; |10000079|    601600     |  398848       | 1.51  |
;; |10000103|    639232     |  401664       | 1.59  |
;; --------------------------------------------------
;; 由上面的结果我们可以看到，效率的提升并没有达到期望中
;; 的2倍，而是随着数字的增大而逐渐接近2倍。这其中的时间损耗
;; 主要是我们使用next函数来替换+这个基本操作符所引入的时间损耗。
;; 随着数字的增大，引入这个函数带来的影响逐步被弱化，从而结果
;; 越来越接近2倍。

;; Exercise 1.24
(define (timed-fast-prime-test n)
  (define (start-prime-test n start-time)
    (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  (newline)
  (display n)
  (start-prime-test n (runtime)))
;; -------------------------
;; |number  |  time (ns)   |
;; -------------------------
;; |1009    |   522752     |
;; |1013    |   545024     |
;; |1019    |   565248     |
;; |10007   |   711936     |
;; |10009   |   654080     |
;; |10037   |   711936     |
;; |100003  |   778496     |
;; |100019  |   809216     |
;; |100043  |   812544     |
;; |1000003 |   902912     |
;; |1000033 |   997376     |
;; |1000037 |   961536     |
;; |10000019|   1090304    |
;; |10000079|   1136640    |
;; |10000103|   1134848    |
;; -------------------------
;; 从上面的结果我们可以看出在目前的输入数量级下
;; 获得的结果并不能满足我们的期望，这个原因应该
;; 是跟我们选择的随机数相关的，当数量级较小时，
;; 我们多次选中较大测试数的概率比较大，所以需要耗费
;; 相对多的时间，而数量级变大后，我们取同样多的随机数，
;; 此时我们连续多次取到较大值的概率降低了，从而耗费的
;; 时间就相对少。
;; 但是我们随着数量级的增大，程序所花费的
;; 时间并没有太多的增长，由此我们可以判断随着数量级
;; 的增大，fast-prime?的优势会越来越大。我们下面便
;; 测试一组大数量级的测试数据来看看效果。
;; 我们选择质数1000000000000159，结果如下：
;; ------------------------------------------------
;; | timed-prime-test		| 6010461440.0    |
;; | timed-prime-test-with-next	| 3996225536.0    |
;; | timed-fast-prime-test	| 5466624.0       |
;; ------------------------------------------------
;; 由上面的表格我们可以看到fast-prime?在一定数量级后
;; 取得了完全的优势。

;; Exercise 1.25
;; 这个想法是完全错误的，看似使用了fast-expt的expmod与
;; 我们自己实现的是一样的，其实它们的效率是天差地别。
;; 只要自己尝试一下使用fast-expt计算大数量级的输入就可以
;; 了解到为何不能使用fast-expt来实现expmod了，因为它的确
;; 很慢！
;; 1. 直接使用fast-expt来实现expmod会很慢的原因很简单：
;; 直接使用fast-expt的expmod需要先计算(fast-expt base exp)，
;; 这个计算在输入变大后会变得非常慢，主要原因就是需要计算的值太大了。
;; 这会严重拖慢整个程序。同样对于超大的数字，remainder函数的计算也会被
;; 严重拖慢。
;; 2. 不使用fast-expt的expmod为什么要更快呢？因为它将整个计算过程分解了，
;; 它将求余和求指数的过程穿插起来，使用remainder将expmod的值始终限定在较小
;; 的范围内，从而避免了对超大数的计算，加快了运算速度。


;; Exercise 1.26
;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;; 	((even? exp)
;; 	 (remainder (* (expmod base (/ exp 2) m)
;; 		       (expmod base (/ exp 2) m))
;; 		    m))
;; 	(else
;; 	  (remainder (* base
;; 			(expmod base (- exp 1) m))
;; 		     m))))
;; 上面的expmod过程将square展开为了显式的乘法，使得
;; expmod的计算量完全没有降低。使用square时，我们只需要
;; 计算(expmod base (/ exp 2) m)一次，从而降低了计算量，
;; 达到了提高计算速度的效果。当我们用显式的乘法来代替square
;; 后，明显可以看出，我们不得不将(expmod base (/ exp 2) m)计算两次。
;; 这样expmod的计算量大大增加了，其复杂度从使用square时的theta(log n)
;; 升到使用显式乘法时的theta(n)。

;; Exercise 1.27
;; 这个过程对于真正的质数也会返回#t
(define (test-carmichael-number n)
  (define (test-iter a n)
    (if (= a 0)
      true
      (if (= (expmod a n n) a)
	(test-iter (- a 1) n)
	false)))
  (test-iter (- n 1) n))
;; (test-carmichael-number 561)
;; => #t
;; (test-carmichael-number 1105)
;; => #t
;; (test-carmichael-number 1729)
;; => #t
;; (test-carmichael-number 2465)
;; => #t
;; (test-carmichael-number 2821)
;; => #t
;; (test-carmichael-number 6601)
;; => #t

;; Exercise 1.28
(define (expmod-with-check base exp m)
  (define (square-with-check x m)
    (if (and (not (or (= x 1) (= x (- m 1))))
	     (= (remainder (square x) m) 1))
      0
      (remainder (square x) m)))
  (cond ((= exp 0) 1)
	((even? exp)
	 (square-with-check
	   (expmod-with-check base (/ exp 2) m) m))
	(else
	  (remainder (* base
			(expmod-with-check base (- exp 1) m))
				     m))))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-with-check a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

;; (miller-rabin-test 561)
;; => #f
;; (miller-rabin-test 562)
;; => #f
;; (miller-rabin-test 563)
;; => #t
;; (miller-rabin-test 1105)
;; => #f
;; (miller-rabin-test 1729)
;; => #f
;; (miller-rabin-test 2465)
;; => #f
;; (miller-rabin-test 2821)
;; => #f
;; (miller-rabin-test 6601)
;; => #f
;; (miller-rabin-test 6601)
;; => #f

;; 将Procedure作为参数
;; (define (sum-integers a b)
;;   (if (> a b)
;;     0
;;     (+ a (sum-integers (+ a 1) b))))
;; (define (sum-cubes a b)
;;   (if (> a b)
;;     0
;;     (+ (cube a) (sum-cubes (+ a 1) b))))
;; (define (pi-sum a b)
;;   (if (> a b)
;;     0
;;     (+ (/ 1.0 (* a (+ a 2)))
;;        (pi-sum (+ a 4) b))))
;; 上面的三个函数有着明显相同的模式，下面我们将这种模式抽象出来
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; 下面的integral可以用来计算积分
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;; 下面的语句就是计算x^3在0-1上的积分，取dx=0.001
(integral cube 0 1 0.001)

;; Exercise 1.29
(define (simpson-integral f a b n)
  (define h (/ (- b a) (* n 1.0))) ;; 确保h是一个浮点数
  (define (y x)
    (f (+ a (* x h))))
  (define (simpson-term k)
    (cond ((or (= k 0) (= k n)) (y k))
	  ((even? k) (* 2 (y k)))
	  (else (* 4 (y k)))))
  (* (/ h 3.0) (sum simpson-term 0 inc n)))

;; (simpson-integral cube 0 1 100)
;; => 0.24999999999999992
;; (integral cube 0 1 0.01)
;; => 0.24998750000000042
;; (simpson-integral cube 0 1 1000)
;; => 0.2500000000000003
;; (integral cube 0 1 0.001)
;; => 0.249999875000001
;; 有上面结果可以看出simpson-integral的计算结果要比
;; integral的结果要好。

;; Exercise 1.30
(define (sum-iterative term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))
(define (factorial-with-product n)
  (product identity 1 inc n))
(define (pi-product n)
  (define (pi-term x)
    (if (even? x)
      (/ (+ x 2.0) (+ x 1.0))
      (/ (+ x 1.0) (+ x 2.0))))
  (* 4 (product pi-term 1 inc n)))

(define (product-iterative term a next b)
  (define (iter n result)
    (if (> n b)
      result
      (iter (next n) (* result (term n)))))
  (iter a 1))
(define (pi-product-iterative n)
  (define (pi-term x)
    (if (even? x)
      (/ (+ x 2.0) (+ x 1.0))
      (/ (+ x 1.0) (+ x 2.0))))
  (* 4 (product-iterative pi-term 1 inc n)))

;; Exercise 1.32
;; 递归实现的accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
	      (accumulate combiner null-value term (next a) next b))))
(define (sum-with-accumulate term a next b)
  (accumulate + 0 term a next b))
(define (product-with-accumulate term a next b)
  (accumulate * 1 term a next b))
;; 用于测试accumulate的正确性，sum和product几乎一致，
;; 测试一个就可以确定accumulate的正确性了。
(define (pi-product-with-accumulate n)
  (define (pi-term x)
    (if (even? x)
      (/ (+ x 2.0) (+ x 1.0))
      (/ (+ x 1.0) (+ x 2.0))))
  (* 4 (product-with-accumulate pi-term 1 inc n)))

;; 迭代实现的accumulate
(define (accumulate-iterative combiner null-value term a next b)
  (define (iter n result)
    (if (> n b)
      result
      (iter (next n) (combiner result (term n)))))
  (iter a null-value))
(define (sum-with-accumulate-iterative term a next b)
  (accumulate-iterative + 0 term a next b))
(define (product-with-accumulate-iterative term a next b)
  (accumulate-iterative * 1 term a next b))
(define (pi-product-with-accumulate-iterative n)
  (define (pi-term x)
    (if (even? x)
      (/ (+ x 2.0) (+ x 1.0))
      (/ (+ x 1.0) (+ x 2.0))))
  (* 4 (product-with-accumulate-iterative pi-term 1 inc n)))

;; Exercise 1.33
(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter n result)
    (if (> n b)
      result
      (if (predicate n)
	(iter (next n) (combiner result (term n)))
	(iter (next n) result))))
  (iter a null-value))
(define (sum-of-primes a b)
  (filtered-accumulate + 0 identity a inc b prime?))
(define (product-of-relative-prime-of n)
  (define (relative-prime-of-n i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 identity 1 inc (- n 1) relative-prime-of-n))

;; 使用let创建局部变量
;; f(x, y) = x(1+xy)^2+y(1-y)+(1+xy)(1-y)
;; 不使用let如何达到构建局部变量的效果：
;;   即使用一个辅助函数，将局部变量转化为辅助函数的参数，
;;   这样就可以达到等同与创建局部变量的效果。
;; (define (f x y)
;;   (define (f-helper a b)
;;     (+ (* x (square a))
;;        (* y b)
;;        (* a b)))
;;   (f-helper (+ 1 (* x y))
;; 	    (- 1 y)))
;; (define (f x y)
;;   ((lambda (a b)
;;      (+ (* x (square a))
;; 	(* y b)
;; 	(* a b)))
;;    (+ 1 (* x y))
;;    (- 1 y)))
;; 因为创建局部变量的需求的确非常常见，所以有了let这个
;; special form来专门用于创建局部变量。
;; (define (f x y)
;;   (let ((a (+ 1 (* x y)))
;; 	(b (- 1 y)))
;;     (+ (* x (square a))
;;        (* y b)
;;        (* a b))))

;; Exercise 1.34
;; (define (f g) (g 2))
;; 执行(f f)会发生什么？
;; (f f)会运行出错，因为我们最终把2当成了操作符。
;; (f f) -> (f 2) -> (2 2)从而产生错误。

;; 使用half-interval(区间分半)方法来求f(x)=0的根。
(define (search f neg-point pos-point)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
	(cond ((positive? test-value)
	       (search f neg-point midpoint))
	      ((negative? test-value)
	       (search f midpoint pos-point))
	      (else midpoint))))))
(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond
      ((and (positive? a-value) (negative? b-value))
       (search f b a))
      ((and (negative? a-value) (positive? b-value))
       (search f a b))
      (else
	(error "Values are not of opposite sign" a b)))))
(half-interval-method sin 2.0 4.0)

;; 求f(x)=x的根的方法，也叫求函数的fixed point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((new-guess (f guess)))
      (if (close-enough? guess new-guess)
	new-guess
	(try new-guess))))
  (try first-guess))

;; (define (sqrt-with-fixed-point x)
;;   (fixed-point (lambda (y) (/ x y))
;; 	       1.0))
;; 上面的函数无法正常工作，因为它会陷入无限循环
(define (sqrt-with-fixed-point x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

;; Exercise 1.35
;; f(x) = 1+1/x and f(x) = x
;; => x^2-x-1=0
;; => x = 1/2(1±√5)
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))

;; Exercise 1.36
(define (fixed-point-with-output f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((new-guess (f guess)))
      (display new-guess)
      (newline)
      (if (close-enough? guess new-guess)
	new-guess
	(try new-guess))))
  (try first-guess))
;; (define (exercise-136-solution)
;;   (fixed-point-with-output (lambda (x) (/ (log 1000) (log x)))
;; 			   2.0))
;; (define (exercise-136-solution-with-avg-damping)
;;   (fixed-point-with-output (lambda (x) (average x (/ (log 1000) (log x))))
;; 			   2.0))
;; 不使用average damping需要花费36步，使用了average damping只需要花费10步。

;; Exercise 1.37
(define (cont-frac n d k)
  (define (iter count result)
    (if (= count 0)
      result
      (iter (- count 1)
	    (/ (n count) (+ (d count) result)))))
  (iter k 0))

;; 使用递归模式时还是需要使用一个辅助函数
(define (cont-frac-recursive n d k)
  (define (helper x)
    (if (= x k)
      (/ (n x) (d x))
      (/ (n x) (+ (d x) (helper (inc x))))))
  (helper 1))

;; Exercise 1.38
(define (e)
  (+ (cont-frac (lambda (x) 1.0)
		(lambda (x)
		  (if (= (remainder x 3) 2)
		    1
		    (* 2 (/ (+ x 1) 3))))
		10000)
     2))
