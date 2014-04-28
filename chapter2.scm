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
