;; HW 4

;; PART 1: Simple Function Definitions

;; 1. cycle()
(defun cycle (n lst)
	(cond
		((null lst) lst)
		(t
			(dotimes (i n lst) ; iterate n times
				(setq front (list(car lst)))       ; get first element of list
				(setq lst (append (cdr lst) front)) ; append first element to end of rest of list
			)
		)
	)
)

;; 2. split-list()
(defun split-list (delim lst)
	(setq final nil)   ; final list
	(setq current nil) ; current list
	(dolist (e lst     ; each element e in lst
		(cond          ; return statement conditional
			((null current) final)
			(t 
				(setq final (append final (list current))) ; end return statement conditional
			)
		))
		
		(cond          ; for loop conditional
			((and (equal e delim) (not(null current)))        ; element e is equal to delimeter
				(setq final (append final (list current))) ; append current list to final list	
				(setq current nil))                        ; clear current list
			((not(equal e delim))
				(setq current (append current (list e))) ; append element to current list
			)
		)
	)
)

;; 3. range()
(defun range (lst)
	(cond 
		((null lst) (setq final lst)))		; if lst is nil, return empty list
	
	(setq min (car lst))   					; set minimum to first element of list
	(setq max (car lst))					; set maximum to first element of list
	(setq sum 0)							; initialize sum  
	(setq count 0)							; initialize length of list
	(setq final lst)						; initialize final to NIL (holds min and max)
	
	(dolist(i lst final)		
		; find minimum value in list
		(cond
			((null min) lst)				; indicates that the list contains NIL
			((> min i) (setq min i)) 		; if minimum > i, new minimum = i in list
			((< min i) min)					; if minimum < i, minimum remains the same
			((min min) min)					; maximum remains the same
		)
		
		; find maximum value in list
		(cond
			((null max) lst)				; indicates that the list contains NIL
			((> max i) max) 				; if maximum > i, maximum remains the same
			((< max i) (setq max i))		; if maximum < i, new maximum = i in list
			((eql max max) max)				; maximum remains the same				
		)	

		(setq sum (+ sum i))				; sum = sum + i (add each new element)
		(setq count (+ count 1))			; count = count + 1 (keep track of # of elements in list)
		(setq avg (/ sum count))			; calculate average		

		; return minimum, maximum, and average values
		(setq final(append (list min) (list avg)))		; (minimum average)
		(setq final(append final (list max)))			; (minimum average maximum)
	)
)

;; 4. search-count()
(defun search-count (elem lst)
	(setq count 0)
	(dolist (e lst count)
		(cond ((equal elem e) (setq count (+ 1 count)))
			(t))
	)
)


;; 5. pivot function 
(defun pivot(n l)
	(setq list1 nil)					; initialize list1
	(setq list2 nil)					; initialize list2
	(setq final nil)					; initialize final list
	
	(dolist(i l final)
		(cond
			((< i n) (setq list1 (append list1 (list i)))) 					; if i < n, append current value to list1
			((>= i n) (setq list2 (append list2 (list i))))					; if i >= n, append current value to list2
		)
	)
	(setq final(append (list list1) (list list2)))
)


;; 6. quick-sort function
(defun quicksort(lst) 						; tweaked pivot function to output lists in separate functions instead of one
	; if list is empty return nil
	(cond
		((null lst) lst)
	
	; else (if list is not empty), recursively quick-sort the list
		(t		
			(setq p (car lst))							; set pivot = head (element) of list
			(setq rest (cdr lst))						; rest = rest of list after pivot	
			(append 
				;(quicksort(pivot p rest))
				(quicksort (less p rest))				; individually sort each sublist
				(cons (car lst) nil)
				(quicksort (more (car lst) (cdr lst)))
			)
		)
	)
)		


(defun less(n l)						; returns list 1 (<)
	(setq list1 nil)					; initialize list1
	(setq final1 nil)					; initialize final list
	
	(dolist(i l final1)
		(cond
			((< i n) (setq list1 (append list1 (list i)))) 					; if i < n, append current value to list1
		)
	)
	(setq final1(append final1 list1))
)

(defun more(n l)						; returns list 2 (>)
	(setq list2 nil)					; initialize list2
	(setq final2 nil)					; initialize final list
	
	(dolist(i l final2)
		(cond
			((>= i n) (setq list2 (append list2 (list i))))					; if i >= n, append current value to list2
		)
	)
	(setq final2(append final2 list2))
)



				;; END OF PART 1
				

;; PART 2

;;  1. match function
(defun match (lhs rhs)
	(cond 
		((or (null lhs) (null rhs)) (equal lhs rhs))
		((equal (car lhs) '?)
			(match (cdr lhs) (cdr rhs)))
		((and (equal (car lhs) '!) (match (cdr lhs) (cdr rhs)))
			t)
		((equal (car lhs) '!)
			(match lhs (cdr rhs)))
		(t
			(and (equal (car lhs) (car rhs)) (match (cdr lhs) (cdr rhs))))
	)
)


				;; END OF PART 2 

;; PART 3: Matrix Operations (matrix = list of lists)

;; 1. matrix-add function
(defun matrix-add (m1 m2)
	(mapcar
		(lambda (e1 e2) (mapcar #'+ e1 e2)) ; function to sum each element of each sublist
			m1 m2)                          ; apply to m1 and m2
)

;; 2. matrix-multiply function
(defun matrix-multiply (ma mb)
	(cond
			((null ma) ma)
			((null mb) mb)
		
		(t		
			(setq finalmat nil)
			(dolist(iter (matrix-transpose mb) finalmat)
				(setq i nil)
				(setq r nil)
				(dolist(i ma r)
					(setq op(apply #'+ (mapcar #'* i iter)))
					(setq r(append r (list op)))
				)
				(setq finalmat(append finalmat (list r)))
			)
			(matrix-transpose finalmat)
		)
	)
)
				
;; 3. matrix-transpose()
(defun matrix-transpose (m)
	(apply #'mapcar #'list m)
)


				 ;; END OF PART 3
