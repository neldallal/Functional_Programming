;Nadine and Claudia's CSC 173 Project 3
;LISP PROJECT 3 CSC 173
;FUNCTIONAL PROGRAMMING

;List Functions:

;1. Append two lists

;(.append '(1 3 x a) '(4 2 b)) => (1 3 x a 4 2 b)

(defun .append (seq1 seq2)
 	(if (null seq1) 
		seq2
 		(cons (car seq1) (.append (cdr seq1) seq2))))



;2. Reverse a list

;(.reverse '(a b c d)) => ( d c b a)

(defun .reverse (n1)
	(.reversehelper n1 nil))

(defun .reversehelper (u1 u2)
	(if (null u1) 
		u2
		(.reversehelper (cdr u1) (cons (car u1) u2))))



;3. Map a function over every element in a list (mapcar in Lisp)

;(defun add3 (x) (+ 3 x))
;(.map 'add3 '(1 2 3 4)) => (4 5 6 7)

(defun .map (fn s)
	 (if (null s) 
		nil
		(cons (funcall fn (car s)) (.map fn (cdr s)))))


;4. Index of an element in a list (test using equalp, return -1 if element is not in the list)
;(.indexof 'a '(b c a d)) => 2
;(.indexof 'a '(b c d f)) => -1

(defun .indexof (x seq) 
	(.countCompare x seq 0))


(defun .countCompare (x1 sequence c)
	(if (null sequence)
		-1
		(if (equalp x1 (car sequence)) 
			c
			(.countCompare x1 (cdr sequence) (+ c 1)))))


;Set Functions:

;1. Set membership (test using equalp) 

;(.member 'a '(b c a d)) => t 
;(.member 'z '(b c a d)) => nil

(defun .member (r set)
	(if (null set)
		nil
		(if (equalp r (car set)) 
			t
			(.member r (cdr set)))))



;2. Set Union

;(.union '(a b c) '(a c d)) => (a b c d)


(defun .union (set1 set2)
	(cond 
		((null set2) set1)
		((not (.member (car set2) set1))
			(eliminate-duplicates (.append set1 set2)))
		(t (eliminate-duplicates (.union set1 (cdr set2))))))

(defun eliminate-duplicates (s)
	(cond ((null s) s)
		((.member (car s) (cdr s))
			(eliminate-duplicates (cdr s)))
		(t (cons (car s) (eliminate-duplicates (cdr s))))))


;3. Check if subset or equal (⊆; the p in the function name stands for "predicate", meaning something that is true or false")
;(.subsetp '(a b) '(a b c d)) => t 
;(.subsetp '(a b c d) '(a b c d)) => t
;(.subsetp '() '(a b c d)) => t
;(.subsetp '(a b x) '(a b c d)) => nil 

(defun .subsetp (s1 s2)
	(cond ((null s1) t)
		((null s2) nil)
		((.member (car s1) s2) 
			(.subsetp (cdr s1) s2))
		(t nil)))

;4. Cardinality

;(.cardinality '()) => 0
;(.cardinality '(a b c)) => 3

(defun .cardinality (y1) 
	(.countElements y1 0))

(defun .countElements (x1 n)
	(if (not (null x1)) 
		(.countElements (cdr x1) (+ n 1))
		(+ n 0)))




;Math Functions:

;1. Absolute value (of any number)
;(.abs 7) => 7
;(.abs -7) => 7

(defun .abs (x)
	(if (> x 0)
		(* x 1) (* x -1)))



;2. Factorial (of a non-negative integer)
;(.factorial 5) => 120

;recursive function for factorial calculation
(defun .factorial (n)
	(if (<= n 1) 1
		(* n (.factorial (- n 1)))))



;3. Least common multiple of two positive numbers
;(.lcm 4 6) => 12

;The LCM of two integers n1 and n2 is 
;their product divided by their greatest common divisor 

;.lcm works!
(defun .lcm (n1 n2)
	(if (and (> n1 0) (> n2 0))
	(* (/ n1 (.gcd n1 n2)) n2) 
	nil))

(defun .gcd (a b)
	(cond ((= a 0) b)
		(t (.gcd (.mod b a) a))
		))

;recursive function to calculate the modulo of two numbers
(defun .mod (n b)
	(if (< n b)
		n
		(.mod (- n b) b)))



;4. Test if an integer greater than 1 is prime
;(.primep 5) => t
;(.primep 6) => nil 

;.primep works!

;n is prime if and only if n is its own smallest divisor.
(defun .primep (n)
	(if (> n 1) 
		(= n (.lcd n)) nil))

(defun .lcd (n)
	(.calclcd n 2))

(defun .div (x y) 
	(= (.mod y x) 0))

;calculates the square of n
(defun .sq (n)
  (* n n))

;recursive function to find lowest common divisor for n
(defun .calclcd (n temp)
	(cond ((> (.sq temp) n) n) 
		((.div temp n) temp)
		(t (.calclcd n (+ temp 1)))))
; t means else here in final condition