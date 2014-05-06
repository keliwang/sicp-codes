;; 加入分数的相关操作，假设已经有了构造分数的工具
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer x))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;; 实现表示分数的符合数据
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

;; Exercise 2.1
;; 更好的处理正负数
(define (make-rat n d)
  (if (negative? d)
    (cons (- n) (- d))
    (cons n d)))

;; Exercise 2.2
(define (average x y)
  (/ (+ x y) 2))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (midpoint-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point (average (x-point start) (x-point end))
		(average (y-point start) (y-point end)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Exercise 2.3
(define (rect-perimeter rect)
  (* (+ (rect-width rect) (rect-height rect)) 2))
(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))
;; 使用相对的两个点，且假设这个长方形边与对于坐标轴平行
(define (make-rect a b)
  (cons a b))
(define (rect-width rect)
  (abs (- (x-point (car rect)) (x-point (cdr rect)))))
(define (rect-height rect)
  (abs (- (y-point (car rect)) (y-point (cdr rect)))))
;; 使用一个顶点和长宽值
(define (make-rect p w h)
  (cons p (cons w h)))
(define (rect-width rect)
  (car (cdr rect)))
(define (rect-height rect)
  (cdr (cdr rect)))

;; 实现cons, car, cdr
(define (my-cons1 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "mycons1" "Argument not 0 or 1: CONS" m))))
  dispatch)
(define (my-car1 z)
  (z 0))
(define (my-cdr1 z)
  (z 1))

;; Exercise 2.4
(define (my-cons2 x y)
  (lambda (m) (m x y)))
(define (my-car2 z)
  (z (lambda (p q) p)))
(define (my-cdr2 z)
  (z (lambda (p q) q)))

;; Exercise 2.5
;; 将(cons x y)用(2^x)*(3^y)来表示，
;; 求car时，我们可以将cons算得的值不断除去2，
;; 直到无法整除为止，统计一共进行了多少次除法
;; 我们就可以获得x的值，同理可以获得y的值。
(define (num-divs n d)
  (define (iter count num)
    (if (= (remainder num d) 0)
      (iter (+ count 1) (/ num d))
      count))
  (iter 0 n))
(define (my-cons3 x y)
  (* (expt 2 x) (expt 3 y)))
(define (my-car3 z)
  (num-divs z 2))
(define (my-cdr3 z)
  (num-divs z 3))

;; Exercise 2.6
;; zero会执行f 0次
;; one会执行f 1次
;; two会执行f 2次
;; 依此类推即可直到这里的用f的可以执行的次数来表示数量
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; interval arithematic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
		   (/ 1.0 (lower-bound y)))))

;; Exercise 2.7
(define (make-interval a b)
  (cons a b))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

;; Exercise 2.8
;; [a, b] - [c, d] = [a-d, b-c]
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9
;; 首先我们看看interval arithematic的加法：
;; [a, b] + [c, d] = [a+c, b+d]
;; 和的宽度为((b+d) - (a+c))/2 = (b-a)/2 + (d-c)/2
;; 由上可知和的宽度等于宽度的和。
;; 乘法和除法不满足的示例：
;; (define a (make-interval 3 5))
;; (define a (make-interval 2 9))
;; (mul-interval a b)
;; => (6 . 45) 其宽度为39/2，很明显不满足
;; (div-interval a b)
;; => (0.33333333333 . 2.5) 其宽度2.166667/2，也不满足
;; 由上知，乘法和除法并不满足。

;; Exercise 2.10
(define (div-interval-with-zero-check x y)
  (let ((upper (upper-bound y))
	(lower (lower-bound y)))
    (if (or (= upper 0) (= lower 0))
      (error "div-interval-with-zero-check" "lower bound and upper bound both cannot be 0" lower upper)
      (mul-interval
	x
	(make-interval (/ 1.0 (upper-bound y))
		       (/ 1.0 (lower-bound y)))))))

;; Exercise 2.11
;; 一个interval的符号只有以下三种情况：[+, +], [-, +], [-, -]
;; 所以两个interval的符号组合至多有3*3=9种。
;; 经过分析，其中只有[-, +], [-, +]这一对必须进行4次乘法运算。
;; 下面的函数里会将0分到+类中
(define (mul-interval x y)
  (define (classify-interval interval)
    (cond ((and (negative? (lower-bound interval))
		(negative? (upper-bound interval)))
	   1) ; [-, -]
	  ((negative? (lower-bound interval))
	   2) ; [-, +]
	  (else 3))) ; [+, +]
  (cond ((and (= (classify-interval x) 1)
	      (= (classify-interval y) 1))
	 (make-interval (* (upper-bound x) (upper-bound y))
			(* (lower-bound x) (lower-bound y))))
	((and (= (classify-interval x) 1)
	      (= (classify-interval y) 2))
	 (make-interval (* (lower-bound x) (upper-bound y))
			(* (lower-bound x) (lower-bound y))))
	((and (= (classify-interval x) 1)
	      (= (classify-interval y) 3))
	 (make-interval (* (lower-bound x) (upper-bound y))
			(* (upper-bound x) (lower-bound y))))
	((and (= (classify-interval x) 2)
	      (= (classify-interval y) 1))
	 (make-interval (* (upper-bound x) (lower-bound y))
			(* (lower-bound x) (lower-bound y))))
	((and (= (classify-interval x) 2)
	      (= (classify-interval y) 2))
	 (let ((p1 (* (lower-bound x) (lower-bound y)))
	       (p2 (* (lower-bound x) (upper-bound y)))
	       (p3 (* (upper-bound x) (lower-bound y)))
	       (p4 (* (upper-bound x) (upper-bound y))))
	   (make-interval (min p1 p2 p3 p4)
			  (max p1 p2 p3 p4))))
	((and (= (classify-interval x) 2)
	      (= (classify-interval y) 3))
	 (make-interval (* (lower-bound x) (upper-bound y))
			(* (upper-bound x) (upper-bound y))))
	((and (= (classify-interval x) 3)
	      (= (classify-interval y) 1))
	 (make-interval (* (upper-bound x) (lower-bound y))
			(* (lower-bound x) (upper-bound y))))
	((and (= (classify-interval x) 3)
	      (= (classify-interval y) 2))
	 (make-interval (* (upper-bound x) (lower-bound y))
			(* (upper-bound x) (upper-bound y))))
	((and (= (classify-interval x) 3)
	      (= (classify-interval y) 3))
	 (make-interval (* (lower-bound x) (lower-bound y))
			(* (upper-bound x) (upper-bound y))))))

;; 使用中间值和容忍度来表示interval
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (average (lower-bound i) (upper-bound i)))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

;; Exercise 2.12
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))
(define (percent i)
  (* 100.0 (/ (width i) (center i))))

;; Exercise 2.13
;; 假设使用中间值和容忍度表示的interval的lower bound都是正值
;; 假设现有两组interval: [a-ap, a+ap], [b-bq, b+bq]
;; [a-ap, a+ap]*[b-bq, b+bq] = [ab-abq-abp+abpq, ab+abq+abp+abpq]
;; 因而其宽度为[(ab+abq+abp+abpq)-(ab-abq-abp+abqp)]/2 = ab(p+q)
;; 其center为：ab(1+pq)
;; 我们可以得出乘积的容忍度为：(p+q)/(1+pq)。
;; 因为p和q都是非常小的值，所以1+pq可以近似看作1，
;; 所以两个interval乘积的容忍度可以用p+q近似表示。

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one
      (add-interval (div-interval one r1)
		    (div-interval one r2)))))

;; Exercise 2.14
;; Lem是对的，我们可以随意测试一下，结果如下
;; (define a (make-center-percent 5 0.8))
;; (par1 a a)
;; => (2.4406349206349205 . 2.560645161290323)
;; (par2 a a)
;; => (2.48 . 2.52)

;; 我们继续测试两组数据
;; (define a (make-center-percent 100 0.001))
;; (define b (make-center-percent 200 0.002))
;; (define aa (div-interval a a))
;; (define ab (div-interval a b))
;; (center aa)
;; => 1.0000000002
;; (percent aa)
;; => 0.001999999999812942
;; (center ab)
;; => 0.5000000003
;; (percent ab)
;; => 0.002999999999402041

;; (define a (make-center-percent 100 40))
;; (define b (make-center-percent 200 50))
;; (define aa (div-interval a a))
;; (define ab (div-interval a b))
;; (center aa)
;; => 1.380952380952381
;; (percent aa)
;; => 68.96551724137932
;; (center ab)
;; => 0.8
;; (percent ab)
;; => 75.00000000000001
;; 从上面两组数据我们可以看到(center a/a)的结果并不是1，
;; 这就明显的说明了我们的计算存在误差，而且percent越大，
;; 误差也越大。

;; Exercise 2.15
;; 从数学上来说R1R2/(R1+R2) = 1/(1/R1+1/R2)这个等式是完全没有问题的。
;; 但是我们是怎么从原始的1/(1/R1+1/R2)这个公式推导出R1R2/(R1+R2)的呢？
;; 下面我们来推导一遍：
;; 1/(1/R1+1/R2) = (R1/R1)(R2/R2)(1/(1/R1+1/R2)) = R1R2/(R1+R2)
;; 上面的推导从数学上看完全没有问题，它基于一个事实：R1/R1=1, R2/R2=1
;; 这点在数学上当然成立，可惜这点在我们的程序里并不成立，通过
;; exercise 2.14我们知道(center a/a)的值并不是1，也就是我们程序
;; 计算的R1/R1的结果是有偏差的，所以par1和par2的计算结果才会出现差别。
;; 从上面的分析可以知道，我们应该使用par2，因为它避免了对R1/R1，R2/R2的隐式计算，
;; 它的结果应当更为准确。

;; Exercise 2.16
;; 简而言之，两个数学上相等的公式在计算机上计算出的结果不同
;; 是因为它们在计算机上的计算过程并不是完全等价的，其中存在着
;; 一定的误差。
;; 关于Interval Arithematic的这个问题，可以阅读：
;; http://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem

;; 一些对list的操作
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))
(define (length lst)
  (if (null? lst)
    0
    (+ 1 (length (cdr lst)))))
(define (length lst)
  (define (iter count l)
    (if (null? l)
      count
      (iter (+ count 1) (cdr l))))
  (iter 0 lst))
(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

;; Exercise 2.17
(define (last-pair items)
  (if (= (length items) 1)
    items
    (last-pair (cdr items))))
(define (last-pair items)
  (define (helper a d)
    (if (null? d)
      (cons a d)
      (helper (car d) (cdr d))))
  (helper (car items) (cdr items)))

;; Exercise 2.18
(define (reverse-list items)
  (if (null? items)
    items
    (append (reverse-list (cdr items)) (list (car items)))))

;; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (define (first-denomination items)
    (car items))
  (define (except-first-denomination items)
    (cdr items))
  (define (no-more? items)
    (null? items))
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	  (+ (cc amount
		 (except-first-denomination
		   coin-values))
	     (cc (- amount
		    (first-denomination coin-values))
		 coin-values)))))
;; 硬币面值的顺序并不会影响cc的结果。
;; cc将兑换方法分为两类：一定使用第一种的和一定不使用第一种的，
;; 然后不停地递归计算。这种方法会将硬币的每一种面值都按照这种方法
;; 来考虑一遍，不会有任何遗漏。所以硬币的顺序并不会造成影响。

;; Exercise 2.20
(define (same-parity first . subs)
  (define (helper head tails pred)
    (cond ((and (null? tails) (pred head))
	   (cons head tails))
	  ((null? tails)
	   tails)
	  ((pred head)
	   (cons head (helper (car tails) (cdr tails) pred)))
	  (else
	    (helper (car tails) (cdr tails) pred))))
  (let ((pred (if (even? first) even? odd?)))
    (cons first (helper (car subs) (cdr subs) pred))))

;; map操作
(define nil '())
(define (scale-list items factor)
  (if (null? items)
    nil
    (cons (* (car items) factor)
	  (scale-list (cdr items)
		      factor))))
;; map的重要意义在于它可以帮助我们隐藏list
;; 中元素层面的相关操作，为我们提供了直接
;; 对整个list的操作。
(define (my-map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
	  (my-map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* factor x))
       items))

;; Exercise 2.21
(define (square x)
  (* x x))
(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items))
	  (square-list (cdr items)))))
(define (square-list items)
  (my-map (lambda (x) (square x))
	items))

;; Exercise 2.22
;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;       answer
;;       (iter (cdr things)
;; 	    (cons (square (car things))
;; 		  answer))))
;;   (iter items nil))
;; 上面的程序会逆序输出结果是因为
;; (cons (square (car things)) answer)每次都会把当前计算出的值加入到
;; 列表的最前方。如果我们的输入是(1 2 3 4)，那么我们得到的
;; 结果将是(16 9 4 1)，最先处理的1其后在list的最好，而最后处理的4
;; 则放到了list的最前面。
;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;       answer
;;       (iter (cdr things)
;; 	    (cons answer
;; 		  (square (car things))))))
;;   (iter items nil))
;; 上面的程序无法正常工作是因为
;; (cons answer (square (car things)))将单个的元素放到了后面，这样
;; 我们得到的就不是合法的list了，而是层层包裹住的cons。

;; Exercise 2.23
(define (my-for-each proc items)
  (cond ((null? items)
	 #t)
	(else
	  (proc (car items))
	  (my-for-each proc (cdr items)))))

;; count-leaves的实现
(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

;; Exercise 2.24
;; (list 1 (list 2 (list 3 4)))
;; 解释器的输出：(1 (2 (3 4)))
;; box-and-pointer图及树形图省略。

;; Exercise 2.25
;; (1 3 (5 7) 9)
;; => (car (cdr (car (cdr (cdr lst)))))
;; ((7))
;; => (car (car lst))
;; (1 (2 (3 (4 (5 (6 7))))))
;; => (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst))))))))))))

;; Exercise 2.26
;; (define x (list 1 2 3))
;; (define y (list 4 5 6))
;; (append x y)
;; => (1 2 3 4 5 6)
;; (cons x y)
;; => ((1 2 3) 4 5 6)
;; (list x y)
;; => ((1 2 3) (4 5 6))

;; Exercise 2.27
(define (list-without-sublists? lst)
  (cond ((null? lst) #t)
	((pair? (car lst)) #f)
	(else
	  (list-without-sublists? (cdr lst)))))
(define (deep-reverse lst)
  (cond ((list-without-sublists? lst) (reverse lst))
	((not (pair? lst)) lst)
	(else
	  (reverse (map deep-reverse lst)))))

;; Exercise 2.28
(define (fringe tree)
  (define (helper head tails)
    (cond ((and (null? tails) (not (pair? head)))
	   (cons head nil))
	  ((null? tails)
	   (helper (car head) (cdr head)))
	  ((not (pair? head))
	   (append (cons head nil)
		   (helper (car tails) (cdr tails))))
	  (else
	    (append (helper (car head) (cdr head))
		    (helper (car tails) (cdr tails))))))
  (helper (car tree) (cdr tree)))

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
(define (total-weight mobile)
  (let ((left-struct (branch-structure (left-branch mobile)))
	(right-struct (branch-structure (right-branch mobile))))
    (cond ((and (not (pair? left-struct)) (not (pair? right-struct)))
	   (+ left-struct right-struct))
	  ((and (pair? left-struct) (pair? right-struct))
	   (+ (total-weight left-struct) (total-weight right-struct)))
	  ((pair? left-struct)
	   (+ right-struct (total-weight left-struct)))
	  (else
	    (+ left-struct (total-weight right-struct))))))
(define (torque branch)
  (let ((struct (branch-structure branch))
	(len (branch-length branch)))
    (if (pair? struct)
      (* len (total-weight struct))
      (* len struct))))
(define (mobile-balanced? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile))
	(left-struct (branch-structure (left-branch mobile)))
	(right-struct (branch-structure (right-branch mobile))))
    (cond ((not (= (torque left) (torque right)))
	   #f)
	  ((and (pair? left-struct)
		(pair? right-struct))
	   (and (mobile-balanced? left-struct)
		(mobile-balanced? right-struct)))
	  ((pair? left-branch)
	   (mobile-balanced? left-struct))
	  ((pair? right-branch)
	   (mobile-balanced? right-struct))
	  (else
	    #t))))

;; (define (make-mobile left right) (cons left right))
;; (define (make-branch len structure) (cons len structure))
;; 如果我们将make-mobile和make-branch改为上面的形式，我们只要
;; 将mobile和branch的相关selectors也做出相应修改就可以保证
;; 其它的部分都可以正常工作。

;; 遍历一棵树
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (scale-tree sub-tree factor)
	   (* sub-tree factor)))
       tree))

;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (square tree))
	(else
	  (cons (square-tree (car tree))
		(square-tree (cdr tree))))))
(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (square-tree sub-tree)
	   (square sub-tree)))
       tree))

;; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (tree-map proc sub-tree)
	   (proc sub-tree)))
       tree))
(define (square-tree tree)
  (tree-map square tree))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest
	      (map (lambda (x)
		     (cons (car s) x)) rest)))))
;; From Wikipedia, http://en.wikipedia.org/wiki/Powerset#Algorithms
;; the power set of the empty set is the set containing the empty set
;; and the power set of any other set is all the subsets of the set
;; containing some specific element and all the subsets of the set not
;; containing that specific element.

;; 将序列作为接口
(define (fib n)
  (define (iter a b count)
    (if (= count 0)
      b
      (iter (+ a b) a (- count 1))))
  (iter 1 0 n))
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (if (odd? tree) (square tree) 0))
	(else (+  (sum-odd-squares (car tree))
		  (sum-odd-squares (cdr tree))))))
(define (even-fibs n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
	(if (even? f)
	  (cons f (next (+ k 1)))
	  (next (+ k 1))))))
  (next 0))

(define (my-filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (my-filter predicate (cdr sequence))))
	(else (my-filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))
(define (sum-odd-squares tree)
  (accumulate
    + 0 (map square (my-filter odd? (enumerate-tree tree)))))
(define (even-fibs n)
  (accumulate
    cons
    nil
    (my-filter even? (map fib (enumerate-interval 0 n)))))
(define (list-fib-squares n)
  (accumulate
    cons
    nil
    (map square (map fib (enumerate-interval 0 n)))))
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square (my-filter odd? sequence))))

;; Exercise 2.33
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
	      0
	      coefficient-sequence))

;; Exercise 2.35
;; 用map将每个叶子都当成1，然后使用accumulate累加
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
	  (accumulate-n op init (map cdr seqs)))))

;; Exercise 2.37
;; 矩阵相关操作
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))
(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;; Exercise 2.38
;; accumulate也叫fold-right
(define (my-fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
	    (cdr rest))))
  (iter initial sequence))
;; 如果op满足交换律，那么fold-left和fold-right的值一定一致。
;; A op B = B op A

;; Exercise 2.39
(define (my-reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (my-reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; Nested Mapping
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

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
			    (lambda (i)
			      (map (lambda (j) (list i j))
				   (enumerate-interval 1 (- i 1))))
			    (enumerate-interval 1 n)))))

(define (my-remove item seq)
  (filter (lambda (x) (not (= x item)))
	  seq))
(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
	       (map (lambda (p) (cons x p))
		    (permutations (my-remove x s))))
	     s)))

;; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j)
		    (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

;; Exercise 2.41
(define (unique-triples n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(map (lambda (k)
			       (list i j k))
			     (enumerate-interval 1 (- j 1))))
		      (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))
(define (sum-is-s-triples n s)
  (define (sum-is-s? pair)
    (= (accumulate + 0 pair) s))
  (filter sum-is-s?
	  (unique-triples n)))

;; Exercise 2.42
(define (make-position row col)
  (cons row col))
(define (position-row p)
  (car p))
(define (position-col p)
  (cdr p))
(define empty-board nil)
(define (adjoin-position new-row col rest)
  (append rest (list (make-position new-row col))))
(define (safe? col positions)
  (define (attack? exist-pos new-pos)
    (cond ((= (position-row exist-pos) (position-row new-pos)) #t)
	  ((= (abs (- (position-row exist-pos) (position-row new-pos)))
	      (abs (- (position-col exist-pos) (position-col new-pos))))
	   #t)
	  (else #f)))
  (define (check-new-pos exist-pos-count new-pos)
    (cond ((= exist-pos-count 0) #t)
	  ((attack? (list-ref positions (- exist-pos-count 1)) new-pos) #f)
	  (else
	    (check-new-pos (- exist-pos-count 1) new-pos))))
  (let ((nth-queen (list-ref positions (- col 1))))
    (check-new-pos (- col 1) nth-queen)))
(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
	(lambda (positions) (safe? k positions))
	(flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position
		     new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queens-cols (- k 1))))))
  (queens-cols board-size))

;; Exercise 2.43
;; (flatmap (lambda (new-row)
;; 	   (map (lambda (rest-of-queens)
;; 		  (adjoin-position new-row k rest-of-queens))
;; 		(queens-cols (- k 1))))
;; 	 (enumerate-interval 1 board-size))
;; 改成上面的写法我们就要重复很多次queens-cols计算了。
;; 当queens-cols位于外围时，执行(queens n)时一共只需要计算queens-cols
;; (n+1)次，它们分别是(queens-cols n) (queens-cols (- n 1)) ... (queens-cols 0)。
;; 但是当queens-cols被放在内部时，我们每次都要重复对同一个参数计算多次queens-cols，
;; 这时我们执行(queens n)是需要计算queens-cols n^n次，因为我们首先需要计算(queens-cols (- n 1))
;; n次，每次计算(queens-cols (- n 1))又需要计算(queens-cols (- n 2))n次，依此类推，我们需要计算
;; n^n次。
;; 也就是说这种调换将原先的线性递归变成了现在的树形递归。所以现在的时间复杂度变为了T^board-size。

;; The Picture Language
;; 仅作为占位符
(define (flip-vert painter)
  painter)
(define (flip-horiz painter)
  painter)
(define (beside p1 p2)
  p1)
(define (below p1 p2)
  p1)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
	    (bottom-right (below right right))
	    (corner (corner-split painter (- n 1))))
	(beside (below painter top-left)
		(below bottom-right corner))))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; Exercise 2.44
(define (up-split painter n)
  (let ((smaller (up-split (- n 1))))
    (if (= n 0)
      painter
      (below painter (beside smaller smaller)))))

;; 使用高阶函数
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180  flip-vert)))
    (combine4 (corner-split painter n))))
(define (compose f g)
  (lambda (x)
    (f (g x))))
(define rotate180
  (compose flip-vert flip-horiz))

;; Exercise 2.45
(define (split combine-main combine-branchs)
  (define (split-helper painter n)
    (let ((smaller (split-helper painter (- n 1))))
      (if (= n 0)
	painter
	(combine-main painter (combine-branchs smaller smaller)))))
  split-helper)
(define right-split (split beside below))
(define up-split (split below beside))

;; frame的一些操作
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
		(scale-vect (ycor-vect v) (edge2-frame frame))))))

;; Exercise 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect n v)
  (make-vect (* n (xcor-vect v))
	     (* n (ycor-vect v))))

;; Exercise 2.47
;; 实现1
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (list-ref frame 0))
(define (edge1-frame frame)
  (list-ref frame 1))
(define (edge2-frame frame)
  (list-ref frame 2))
;; 实现2
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cddr frame))

;; painters简单实现
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
	(draw-line
	  ((frame-coord-map frame)
	   (start-segment segment))
	  ((frame-coord-map frame)
	   (end-segment segment))))
      segment-list)))

;; Exercise 2.48
(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

;; Exercise 2.49
;; 绘制frame的轮廓
(define (frame-outline-painter frame)
  ((segments->painter
     (list (make-segment (make-vect 0 0) (make-vect 0 1))
	   (make-segment (make-vect 0 0) (make-vect 1 0))
	   (make-segment (make-vect 1 1) (make-vect 0 1))
	   (make-segment (make-vect 1 1) (make-vect 1 0))))
   frame))
;; 连接frame的对角线
(define (frame-diagonal-line-painter frame)
  ((segments->painter
     (list (make-segment (make-vect 0 0) (make-vect 1 1))
	   (make-segment (make-vect 0 1) (make-vect 1 0))))
   frame))
;; 绘制一个钻石形
(define (diamond-painter frame)
  ((segments->painter
     (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
	   (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
	   (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
	   (make-segment (make-vect 0.5 0) (make-vect 0 0.5))))
   frame))
;; 绘制wave
;; 略

;; painter的基本操作
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter (make-frame
		   new-origin
		   (sub-vect (m corner1) new-origin)
		   (sub-vect (m corner2) new-origin)))))))
(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
(define (shrink-to-upper-right painter)
  (transform-painter
    painter
    (make-vect 0.5 0.5)
    (make-vect 1.0 0.5)
    (make-vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	    (transform-painter painter1
			       (make-vect 0.0 0.0)
			       split-point
			       (make-vect 0.0 1.0)))
	  (paint-right
	    (transform-painter painter2
			       split-point
			       (make-vect 1.0 0.0)
			       (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 0.0 1.0)))
(define (rotate180 painter)
  (rotate90 (rotate90 painter)))
(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

;; Exercise 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
	    (transform-painter painter2
			       split-point
			       (make-vect 1.0 0.5)
			       (make-vect 0.0 1.0)))
	  (paint-bottom
	    (transform-painter painter1
			       (make-vect 0.0 0.0)
			       (make-vect 1.0 0)
			       split-point)))
      (lambda (frame)
	(paint-top frame)
	(paint-bottom frame)))))
(define (below painter1 painter2)
  (rotate90 (beside painter1 painter2)))

;; Exercise 2.52
;; 暂略

;; symbols
(define (my-memq item x)
  (cond ((null? x) #f)
	((eq? item (car x)) x)
	(else (my-memq item (cdr x)))))

;; Exercise 2.53
;; (list 'a 'b 'c)
;; => (a b c)
;; (list (list 'george))
;; => ((george))
;; (cdr '((x1 x2) (y1 y2)))
;; => ((y1 y2))
;; (pair? (car '(a short list)))
;; => #f
;; (memq 'red '((red shoes) (blue socks)))
;; => #f
;; (memq 'red '(red shoes blue socks))
;; => (red shoes blue socks)

;; Exercise 2.54
(define (my-equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
	((or (null? list1) (null? list2)) #f)
	((eq? (car list1) (car list2))
	 (my-equal? (cdr list1) (cdr list2)))
	(else
	  #f)))

;; Exercise 2.55
(car ''abracadabra)
;; (car ''abracadabra)
;; 在解释器中会被表示为
;; (car (quote (quote abracadabra)))
;; 即(car '(quote abracadabra))，
;; 因而它的结果是quote

;; 简单的符号求导
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	   (make-product (multiplier exp)
			 (deriv (multiplicand exp) var))
	   (make-product (deriv (multiplier exp) var)
			 (multiplicand exp))))
	(else
	  (error "deriv" "unknown expression type" exp))))

;; 如何表示代数表达式
(define (variable? x)
  (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (list '+ a1 a2))
(define (make-product m1 m2)
  (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s)
  (cadr s))
(define (augend s)
  (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (caddr p))
;; 允许简化表达式
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;; Exercise 2.56
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	(else (list '** b e))))
(define (base s)
  (cadr s))
(define (exponent s)
  (caddr s))
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product
			(make-exponentiation (base exp) (- (exponent exp) 1))
			(deriv (base exp) var))))
	(else
	 (error "deriv" "unknown expression type" exp))))

;; Exercise 2.57
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;; Exercise 2.58
;; a部分
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else
	 (list a1 '+ a2))))
(define (sum? s)
  (and (pair? s) (eq? (cadr s) '+)))
(define (addend s)
  (car s))
(define (augend s)
  (caddr s))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else
	 (list m1 '* m2))))
(define (product? p)
  (and (pair? p) (eq? (cadr p) '*)))
(define (multiplier p)
  (car p))
(define (multiplicand p)
  (caddr p))
;; b部分
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cddr p)))

;; sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else
	 (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else
	 (intersection-set (cdr set1) set2))))

;; Exercise 2.59
(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) set2)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else
	 (cons (car set1) (union-set (cdr set1) set2)))))

;; Exercise 2.60
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else
	 (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (cons x set))
(define (union-set set1 set2)
  (append set1 set2))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else
	 (intersection-set (cdr set1) set2))))
;; union-set的复杂度由O(n^2)变为O(n)
;; adjoin-set的复杂度由O(n)变为O(1)
;; 由于set里允许有重复的元素，所以它的空间消耗会变大。
;; 这种表示方法主要应该用在我们不关心空间消耗，同时
;; 我们执行union-set和adjoin-set操作比较频繁时采用。

;; sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) #f)
	(else
	 (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set (cdr set1)
					  (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))))))

;; Exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else
	 (cons (car set) (adjoin-set x (cdr set))))))

;; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((= x1 x2)
		  (union-set (cdr set1) set2))
		 ((< x1 x2)
		  (cons x1 (union-set (cdr set1) set2)))
		 ((> x1 x2)
		  (cons x2 (union-set set1 (cdr set2)))))))))

;; sets as binary trees
(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))


;; Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1
		     (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list
			     (right-branch tree)
			     result-list)))))
  (copy-to-list tree '()))
;; 上面的两个函数对每棵树产生的结果都相同，因为
;; 上面两个函数是同一个算法（中序遍历）的两种不同表达，
;; 递归形式表达和迭代形式表达。
;; (tree->list-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
;; => (1 3 5 7 9 11)
;; (tree->list-2 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
;; => (1 3 5 7 9 11)
;; (tree->list-1 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))))
;; => (1 3 5 7 9 11)
;; (tree->list-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))))
;; => (1 3 5 7 9 11)
;; (tree->list-1 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))))
;; => (1 3 5 7 9 11)
;; (tree->list-2 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))))
;; => (1 3 5 7 9 11)

;; 这两个算法都会访问树的每一个节点，但是其中也会有些不同。
;; 第二个算法每一步都会调用cons, 它是一个复杂度O(1)
;; 的操作，所以第二个算法的复杂度为O(n)。
;; 第一个算法则有所不同，它的每一步都需要调用append，
;; 从以前的实现我们知道append的复杂度依赖于它的第一个参数，
;; 这里它依赖的是树的左子树。对append的n个操作，每次操作
;; 的数目都会减半，所以第一个算法的复杂度为O(n*log n)

;; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result
	       (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result
		   (partial-tree
		    (cdr non-left-elts)
		    right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts
		     (cdr right-result)))
		(cons (make-tree this-entry
				 left-tree
				 right-tree)
		      remaining-elts))))))))
;; partial-tree会将输入的元素分为三个部分，它们分别是
;; 中间点，中间点左边部分和中间点右边部分。
;; 然后对左右两个部分再分别使用partial-tree构建子树，
;; 这是一个递归的过程。
;; (list->tree '(1 3 5 7 9 11))
;; => (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;; 上面的算法会访问列表中的所有节点，每次访问都只是
;; 做了一个cons操作，所以它的复杂度为O(n)。

;; Exercise 2.65
(define (union-set set1 set2)
  (define (union-list list1 list2)
    (cond ((null? list1) list2)
	  ((null? list2) list1)
	  (else
	   (let ((x1 (car list1))
		 (x2 (car list2)))
	     (cond ((= x1 x2)
		    (union-list (cdr list1) list2))
		   ((< x1 x2)
		    (cons x1 (union-list (cdr list1) list2)))
		   ((> x1 x2)
		    (cons x2 (union-list list1 (cdr list2)))))))))
  (list->tree
   (union-list
    (tree->list-2 set1)
    (tree->list-2 set2))))
(define (intersection-set set1 set2)
  (define (common-elements list1 list2)
    (if (or (null? list1) (null? list2))
	'()
	(let ((x1 (car list1))
	      (x2 (car list2)))
	  (cond ((= x1 x2)
		 (cons x1 (common-elements (cdr list1) (cdr list2))))
		((< x1 x2)
		 (common-elements (cdr list1) list2))
		((> x1 x2)
		 (common-elements list1 (cdr list2)))))))
  (list->tree
   (common-elements
    (tree->list-2 set1)
    (tree->list-2 set2))))

;; sets and information retrieval
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((equal? given-key (key (car set-of-records)))
	 (car set-of-records))
	(else
	 (lookup given-key (cdr set-of-records)))))

;; Exercise 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((= given-key (key (entry set-of-records)))
	 (car set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (right-branch set-of-records)))
	(else
	 (lookup given-key (left-branch set-of-records)))))


;; Huffman encoding procedure
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; Huffman decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else
	 (error "choose-branch" "bad bit" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set)))
	 (cons x set))
	(else
	 (cons (car set)
	       (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) ; symbol
			       (cadr pair)) ; frequency
		    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree
		    (make-leaf 'D 1)
		    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; (decode sample-message sample-tree)
;; => (A D A B B C A)

;; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (symbol-in-set? symbol set)
    (cond ((null? set) #f)
	  ((eq? (car set) symbol) #t)
	  (else
	   (symbol-in-set? symbol (cdr set)))))
  (define (symbol-in-branch? symbol branch)    
    (symbol-in-set? symbol (symbols branch)))
  (cond ((leaf? tree) '())
	((symbol-in-branch? symbol (left-branch tree))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((symbol-in-branch? symbol (right-branch tree))
	 (cons 1 (encode-symbol symbol (right-branch tree))))
	(else
	 (error "encode-symbol" "symbol not in tree" symbol tree))))
;; 测试
;; (encode '(A D A B B C A) sample-tree)
;; => (0 1 1 0 0 1 0 1 0 1 1 1 0)
;; 与sample-message一致

;; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (let ((current-tree (make-code-tree
			   (car leaf-set)
			   (cadr leaf-set))))
	(successive-merge
	 (adjoin-set current-tree
		     (cddr leaf-set))))))

;; Exercise 2.70
(define lyric-tree (generate-huffman-tree
		    '((a 2) (get 2) (sha 3) (wah 1)
		      (boom 1) (job 2) (na 16) (yip 9))))
(define lyric '(get a job sha na na na na na na na na
		get a job sha na na na na na na na na
		wah yip yip yip yip yip yip yip yip yip
		sha boom))
(define lyric-codes (encode lyric lyric-tree))
;; (length lyric-codes) ;; 编码这首歌所需要的bit数
;; => 84
;; 如果使用定长编码方式来编码的话，8种不同的”字符“
;; 最少需要三个bit来编码，所以编码上面的歌曲最少需要
;; 36*3 = 108bit。

;; Exercise 2.71
;; n = 5 和n = 10时的huffman tree的绘制省略。
;; 在这种频率的分布下，出现最频繁的符号需要1bit
;; 来编码，出现最少的符号需要n-1bit来编码。
;; 通过绘制几组huffman tree我们很容易得出上面的结论。

;; Exercise 2.72
;; 编码最频繁出现的符号只需要几步就可以了，它的复杂度为O(1)。
;; 编码最少出现的符号则需要沿着树一直向下，一共要向下n次，
;; 一共需要进行n次查找，其查找次数为n+(n-1)+...+1=n^2，
;; 所以其复杂度为O(n^2)。

;; complex number
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

;; 第一种实现方法
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
	   (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y)
  (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))
;; 第二种实现方法
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z)
  (car z))
(define (angle z)
  (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
	(atan y x)))
(define (make-from-mag-ang r a)
  (cons r a))

;; Tagged Data
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "type-tag" "Bad tagged datum" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "contents" "Bad tagged datum" datum)))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z)
  (car z))
(define (imag-part-rectangular z)
  (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
	      (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z)
  (car z))
(define (angle-polar z)
  (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
	 (real-part-rectangular (contents z)))
	((polar? z)
	 (real-part-polar (contents z)))
	(else
	 (error "real-part" "Unknown type" z))))
(define (imag-part z)
  (cond ((rectangular? z)
	 (imag-part-rectangular (contents z)))
	((polar? z)
	 (imag-part-polar (contents z)))
	(else
	 (error "imag-part" "Unknown type" z))))
(define (magnitude z)
  (cond ((rectangular? z)
	 (magnitude-rectangular (contents z)))
	((polar? z)
	 (magnitude-polar (contents z)))
	(else
	 (error "magnitude" "Unknown type" z))))
(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular (contents z)))
	((polar? z)
	 (angle-polar (contents z)))
	(else
	 (error "angle" "Unknown type"))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; use package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "apply-generic" "No method for these types" (list op type-tags))))))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Exercise 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp))
	       (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;; a) 上面的函数主要是先对exp分类，如果
;;    exp为数字或变量则直接处理，如果
;;    exp为表达式则查表找到相应的处理函数来处理。
;;    不将number?和variable?也变为data-directed
;;    dispatch是因为这两种情况下exp并没有操作符和
;;    操作数的组成，它不是复合的语句。
;; b)
(define (install-sum-and-product-deriv-package)
  (define (sum-deriv operands var)
    (make-sum (deriv (addend operands) var)
	      (deriv (augend operands) var)))
  (define (addend operands)
    (car operands))
  (define (augend operands)
    (cadr operands))

  (define (product-deriv operands var)
    (make-sum (make-product (deriv (multiplier operands) var)
			    (multiplicand operands))
	      (make-product (multiplier operands)
			    (deriv (multiplicand operands) var))))  
  (define (multiplicand operands)
    (cadr operands))
  (define (multiplier operands)
    (car operands))

  (put 'deriv '+ sum-deriv)
  (put 'deriv '* product-deriv)
  'done)
;; c)
(define (install-exponent-deriv-package)
  (define (exponent-deriv operands var)
    (make-product (exponent operands)
		  (make-product (make-exponentiation (base operands)
						     (- (exponent operands) 1))
				(deriv (base operands) var))))
  (define (base operands)
    (car operands))
  (define (exponent operands)
    (cadr operands))

  (put 'deriv '** exponent-deriv)
  'done)
;; d) 当我们将调用语句改为((get (operator exp) 'deriv) (operands exp) var)
;;    时，我们只要将package的install阶段改为与(put '+ 'derive sum-deriv)类似
;;    的方式就可以了。

;; Exercise 2.74
;; a)
(define (get-record employee file)
  ((get 'get-record (division file))
   employee (contents file)))
(define (make-file-with-division division file)
  (cons division file))
(define (division file)
  (car division))
(define (contents file)
  (cdr file))

;; b)
(define (get-salary generic-record)
  ((get 'get-salary (division generic-record))
   (contents generic-record)))
(define (make-generic-record record division)
  (cons record division))
(define (division record)
  (car record))
(define (contents record)
  (cdr record))

;; c)
(define (find-employee-record employee files)
  (cond ((null? files) (error "find-employee-record" "cannot file specific record" employee))
	((in-division? employee (division (car files)))
	 (get-record employee (car files)))
	(else
	 (find-employee-record
	  employee
	  (cdr files)))))
(define (in-division? employee division)
  ((get 'in-division? division) employee))

;; d)
;; 实现in-division?即可

;; message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
	  ((eq? op 'imag-part) y)
	  ((eq? op 'magnitude) (sqrt (+ square x) (square y)))
	  ((eq? op 'angle) (atan y x))
	  (else
	   (error "make-from-real-imag" "Unknown op" op))))
  dispatch)
(define (apply-generic op arg) (arg op))

;; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  (else
	   (error "make-from-mag-ang" "Unknown op" op)))))

;; Exercise 2.76
;; 要为显式分派的泛型操作添加新的类型，我们需要更新所有的泛型接口，使其可以识别
;; 新的类型。要为显式分派的泛型操作添加新操作，我们需要对所有的已有类型实现这个
;; 新操作，同时也要添加泛型接口。
;; 为data-directed系统添加新类型和操作都只需要更新操作表
;; 为message-passing系统添加新类型只需要实现这个类型就可以了，如果要添加一个新操作
;; 的话，我们需要为每个类型都添加一条操作。
;; 现有系统经常需要增加新类型或操作时，使用data-directed或message-passing系统都可以，
;; 但是message-passing系统只允许有一个参数的操作。


;; Generic Arithmetic Operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)
(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

(define (install-rational-package)
  ;; internal procedure
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (numer y))
		 (* (numer y) (numer x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Exercise 2.77
;; (put 'real-part '(complex) real-part)
;; (put 'imag-part '(complex) imag-part)
;; (put 'magnitude '(complex) magnitude)
;; (put 'angle '(complex) angle)
;; 加上上面的一组导出之后，apply-generic
;; 就能识别参数为(complex)的real-part调用了。
;; 而直接使用real-part是因为原先的real-part
;; 都是可以识别(rectangular)等参数的。
;; 调用(magitude z)的大致过程如下：
;; (apply-generic 'magnitude '(complex rectangular 3 4))
;; ((get 'magnitude '(complex)) '(rectangular 3 4))
;; (apply-generic 'magnitude '(rectangular 3 4))
;; ((get 'magnitude '(rectangular)) '(3 4))
;; (magnitude (3 4))
;; 由上可知，apply-generic 被调用了2次，第一次会调用可识别
;; (complex)参数的magnitude函数，第二次会调用可识别(rectangular)
;; 参数的magnitude函数。

;; Exercise 2.78
(define (attach-tag tag contents)
  (if (and (eq? tag 'scheme-number) (number? contents))
      contents
      (cons tag contents)))
(define (type-tag datum)
  (cond ((number? datum)
	 'scheme-number)
	((pair? datum)
	 (car datum))
	(else
	 (error "type-tag" "datum is not valid" datum))))
(define (contents datum)
  (cond ((number? datum)
	 datum)
	((pair? datum)
	 (cdr datum))
	(else
	 (error "contents" "datum is not valid" datum))))

;; Exercise 2.79
(define (equ? x y)
  (apply-generic 'equ? x y))
(define (install-equ?-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'equ? '(rational rational)
       (lambda (x y)
	 (= (* (numer x) (denom y))
	    (* (numer y) (denom x)))))
  (put 'equ? '(complex complex)
       (lambda (x y)
	 (and (= (real-part x) (real-part y))
	      (= (imag-part x) (imag-part y)))))
  'done)

;; Exercise 2.80
(define (=zero? x)
  (apply-generic '=zero? x))
(define (install-=zero?-package)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0)
			(= (imag-part x) 0))))
  'done)

;; Coercion，即类型转换
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number
	      'complex
	      scheme-number->complex)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (get-coercion type1 type2))
		      ((t2->t1 (get-coercion type2 type1))))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else
			 (error "apply-generic" "No method for these types" (list op type-tags))))))
	      (error "apply-generic" "No method for these types" (list op type-tags)))))))

;; Exercise 2.81
;; a) 用两个complex数来调用exp会导致无限递归，从而引起程序假死。因为一旦
;;    定义了相同类型的互转之后，即使exp无法处理complex参数，get-coerion阶段
;;    也总是能找到complex->complex转换函数，导致(apply-generic 'exp (t1->t2 a1) a2)
;;    被执行，而它又会导致下一个(apply-generic 'exp (t1->t2 a1) a2)的执行，
;;    从而导致程序形成无限递归，无法退出。
;; b) Louis是错的，因为apply-generic在尝试将相同类型互转时就会发现t1->t2和t2->t1的值都是
;;    false，这样错误信息就可以正常的显示，并不会引起其它的问题。
;; c) 如下：
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length type-tags) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags)))
		(if (eq? type1 type2)
		    (error "apply-generic" "No method for these types" (list op type-tags))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1))
			  (a1 (car args))
			  (a2 (cadr args)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (error "apply-generic" "No method for these types" (list op type-tags)))))
		    (error "apply-generic" "No method for these types" (list op type-tags)))))))))

;; Exercise 2.82
(define (apply-generic op . args)
  (define (can-coercion-to-type? type types)
    (cond ((null? types) #t)
	  ((eq? type (car types))
	   (can-coercion-to-type? type (cdr types)))
	  ((get-coercion (car types) type)
	   (can-coercion-to-type? type (cdr types)))
	  (else #f)))
  (define (get-valid-coercion-type-helper type-pos types)
    (cond ((= type-pos (length types)) #f)
	  ((can-coercion-to-type? (list-ref types type-pos) types)
	   (list-ref types type-pos))
	  (else
	   (get-valid-coerion-type (+ type-pos 1) types))))
  (define (get-valid-coercion-type types)
    (get-valid-coercion-type-helper 0 types))
  (define (coercion-all-args valid-type args)
    (map (lambda (type)
	   ((get-coercion type valid-type) type))
	 args))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (let ((valid-coercion-type (get-valid-coercion-type type-tags)))
	    (if valid-coercion-type
		(apply-generic op (coercion-all-args valid-coerion-type args))
		(error "apply-generic" "No method for these types" (list op type-tags))))))))
;; 假设有三个类型A, B, C，其中A可以方便的转化为B。假设一个函数(func B B C)，我们给的参数为A B C，
;; 这样的话使用上面的方案就无法正常执行func了。
