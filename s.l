(defun cross1-recursive (x y)
    (cond
        ( (atom x) nil)
        ( (not (listp y)) nil)
        (t (cons (cons (car x) (list y)) (cross1-recursive (cdr x) y) ))
    )
)


(defun cross1-iterative (x y)
    (cond 
        ( (atom x) nil)
        ( (not (listp y)) nil)
        (t 
            (do ( (q1 x (cdr q1))
                  (res nil (append res (list (list (car q1) y)  ))   )
                )

                ((null q1) res)
            )
        )
    )
)

(defun cross1-mapcar (x y)
    (cond 
        ( (atom x) nil)
        ( (not (listp y)) nil)
        (t 
            (mapcar #'(lambda (a) (list a y)) x)
        )
    )
)


(defun cross2-recursive-helper (xElement y)
    (cond
        ( (null y) nil)
        (t
            (cons
                (list xElement (car y))
                (cross2-recursive-helper xElement (cdr y))
            )
        )
    )
)

(defun cross2-recursive (x y)
    (cond
        ( (atom x) nil)
        ( (atom y) nil)
        ( t
            (append
                (cross2-recursive-helper (car x) y)
                (cross2-recursive (cdr x) y)
            )
        )
    )
)

(defun cross2-iterative (x y)
    (cond 
        ( (atom x) nil)
        ( (atom y) nil)
        (t 
            (do ( (q1 x (cdr q1))
                  (res1 nil
                    (append
                        res1

                        (do ( (q2 y (cdr q2))
                              (res2 nil
                                (append
                                    res2
                                    (list (list (car q1) (car q2)))
                                )
                              )
                            )
                            ((null q2) res2)
                        )
                    )
                  )
                )
                ((null q1) res1)
            )
        )
    )
)

(defun cross2-mapcar (x y)
    (cond 
        ( (atom x) nil)
        ( (atom y) nil)
        (t 
            (apply 'append
            (mapcar
                #'(lambda (xElement)
                    (mapcar
                        #'(lambda (yElement)
                            (list xElement yElement)
                        )
                        y
                    )
                )
                x
            )
            )
        )
    )
)

(defun my-every (fun q)
    (cond
        ((null q) t)
        (t (and
                (funcall fun (car q))
                (my-every fun (cdr q))
            )
        )
    )
)

(defun flatp (x)
    (my-every
        #'(lambda (xElement)
            (cond
                ((listp xElement) nil)
                (t t)
            )
        )
        x
    )
)

(defun get-lists (l)
    (cond
        ((null l) nil)
        ((listp (car l)) (cons (car l) (get-lists (cdr l))))
        (t (get-lists (cdr l)))
    )
)


(defun lenLFL (x)
    (cond 
        ((flatp x) (length x))
        (t (let
                ((lists (get-lists x) ))
                (max
                    (lenLFL (car lists))
                    (lenLFL (cdr lists))
                )
            )
        )
    )
)

(defun justkeeplookingoutforcond (l) 
    (cond
        ((atom l) t) 
        ((atom (car l)) (justkeeplookingoutforcond (cdr l))) 
        (t 
            (cond 
                ((equal (car (car l)) 'cond)
                    (and (legalcondp (car l)) (justkeeplookingoutforcond (cdr l)))
                ) 
                (t
                    (and (justkeeplookingoutforcond (car l)) (justkeeplookingoutforcond (cdr l)))
                )
            )
        )
    )
)

(defun checkthatalllistsareoflength1or2 (l)
    (cond
        ((null l) t)
        ((atom (car l)) nil)
        ((or (eq (length (car l)) 1) (eq (length (car l)) 2))
            (and
                (justkeeplookingoutforcond (car l))
                (checkthatalllistsareoflength1or2 (cdr l))
            )
        )
        (t nil)
    )
)

(defun legalcondp (l)
    (cond
        ( (atom l) t)
        ( (equal (car l) 'cond)
            (cond
                (nil (cdr l) nil) 
                (t (checkthatalllistsareoflength1or2 (cdr l)))
            )
        )

        ( t 
            (justkeeplookingoutforcond (cdr l))
        )
    )
)


(defun justlookforacond(l) 
    (cond
        ((null l) nil)
        ((atom l) l)
        (t 
            (cond
                ((equal (car l) 'cond) (rewrite-helper l))
                (t 
                    (cons
                        (justlookforacond (car l))
                        (justlookforacond (cdr l))
                    )
                )
            )
        )
    )
)

(defun convertlisttoif(l) 
    (cond
        ((null l) nil)
        (t 
            (cond
                ( (eq (length l) 1) (append (list 'if) (justlookforacond l) (justlookforacond l) ))
                (t (append (list 'if) (justlookforacond l)))
            )
        )
    )
)


(defun converliststoif(ls)
    (cond 
        ((null ls) nil)
        (t 
            (cond
                ((null (cdr ls)) (convertlisttoif (car ls)))
                (t
                    (append
                        (convertlisttoif (car ls))
                        (list (converliststoif (cdr ls)))
                    )
                )
            )
        )
    )
)


(defun rewrite-helper(x)
    (cond 
        ((equal (car x) 'cond) 
            (append (converliststoif (cdr x)))
        )
        (t
            (justlookforacond x)
        )
    )
)



(defun rewrite(x)
    (cond
        ((atom x) x)
        ((null (legalcondp x)) nil)
        (t (rewrite-helper x))
    )
)


(defun check (x)
    (cond
        ((not (legalcondp x)) nil)
        (t
            (prog (n2)
                (setq n2 (eval x))
                (return (prog (n3)
                    (setq n3 (eval (rewrite x)))
                    (return
                        (list
                            (cond ((equal n2 n3) t) (t nil))
                            n2
                            n3
                        )
                    )
                ))
            )
        )
    )
)


(defun justlookforacond-ite(l) 
    (cond
        ((null l) nil)
        ((atom l) l)
        (t 
            (cond
                ((equal (car l) 'cond) (rewrite-helper-ite l))
                (t 
                    (cons
                        (justlookforacond-ite (car l))
                        (justlookforacond-ite (cdr l))
                    )
                )
            )
        )
    )
)

(defun convertlisttoif-ite(l) 
    (cond
        ( (eq (length l) 1)
            (append (list 'if) (justlookforacond-ite l) (justlookforacond-ite l))
        )
        (t 
            (cond
                ((eq (car l) 't) (car (cdr l)))
                (t
                    (append
                        (list 'if)
                        (justlookforacond-ite l)
                    )
                )
            )
        )
    )
)

(defun converliststoif-ite(ls)
    (cond 
        ((null ls) nil)
        (t 
            (cond
                ((null (cdr ls)) (convertlisttoif-ite (car ls)))
                (t
                    (append
                        (convertlisttoif-ite (car ls))
                        (list (converliststoif-ite (cdr ls)))
                    )
                )
            )
        )
    )
)


(defun rewrite-helper-ite(x)
    (cond 
        ((equal (car x) 'cond) 
            (append (converliststoif-ite (cdr x)))
        )
        (t
            (justlookforacond-ite x)
        )
    )
)



(defun rewrite-ite(x)
    (cond
        ((atom x) x)
        ((null (legalcondp x)) nil)
        (t (rewrite-helper-ite x))
    )
)



(defun check-ite (x)
    (cond
        ((not (legalcondp x)) nil)
        (t
            (prog (n2)
                (setq n2 (eval x))
                (return (prog (n3)
                    (setq n3 (eval (rewrite-ite x)))
                    (return
                        (list
                            (cond ((equal n2 n3) t) (t nil))
                            n2
                            n3
                        )
                    )
                ))
            )
        )
    )
)