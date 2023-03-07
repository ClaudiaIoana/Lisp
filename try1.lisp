;;write  a function to insert an element E on the n-th position of a linear list.
;;insert_elem(E - the element to be added, poz - the position, l - the list)
;;flow model(i, i, i), (i, i, o)

(defun insert_elem(E poz l)
  (cond
   ((null l) nil)
   ((< 0 poz) (cons (car l) (insert_elem E (- poz 1) (cdr l))))
   ((cons E l))
))

(defun test_insert_elem()
  (assert
      (and
       (equal '(1 2 3) (insert_elem 2 1 '(1 3)))
       (equal '(1 3 4 5 6 9) (insert_elem 5 3 '(1 3 4 6 9)))
       (equal '(A B C 3 5 2) (insert_elem 3 3 '(A B C 5 2)))
)))


;;write a function to return the sum of all numerical atoms of a list at any level

(defun sum_atom(l)
  (cond
    ((null l) 0)
    ((numberp (car l)) (+ (car l) (sum_atom(cdr l))))
    ((listp (car l)) (+ (sum_atom (car l)) (sum_atom(cdr l))))
    (t (sum_atom (cdr l)))
 )
 )

(defun test_sum_atom()
  (assert
      (and
       (equal 21 (sum_atom '((1 2 3) (4 (5 6)))))
       (equal 5 (sum_atom '(1 3 (1))))
       (equal 10 (sum_atom '(2 -2 (10))))
)))
                    

;write a function to return the set of all sublists of a given linear list. ex. for list((1 2 3)((4 5) 6)) => ((1 2 3)(4 5) ((4 5) 6)

(defun reun(l m)
  (cond
   ((null l) m)
   (t (cons (car l) (reun (cdr l) m)))
))


(defun sublists(l)
  (cond
   ((null l) nil)
   ((listp(car l)) (reun (reun (list(car l)) (sublists(car l)))(sublists(cdr l))))
   (t (sublists(cdr l)))
)
)

(defun test_sublists()
 (assert
      (and
       (equal '((1 2 3)((4 5)6)(4 5)) (sublists '((1 2 3)((4 5 )6))))
       (equal '((1 2)(1 (23 (10)))(23 (10))(10)) (sublists '((1 2)(1 (23 (10))))))
)))

;write a function to test the equality of two sets, without using the difference of two sets

(defun numb_occurences(e l)
  (cond
   ((null l) 0)
   ((equal(car l) e) (+ 1 (numb_occurences e (cdr l))))
   (t (numb_occurences e (cdr l)))
))

(defun equal_set(s1 s2)
  (cond
   ((null s1) t)
   ((equal 0 (numb_occurences (car s1) s2)) nil)
   (t (equal_set (cdr s1) s2))
)
)

(defun is_set(s1)
  (cond
   ((null s1) t)
   ((> (numb_occurences (car s1) s1) 1) nil)
   (t (is_set (cdr s1)))
))


(defun two_sets(s1 s2)
  (cond
   ((is_set s1) (is_set s2))
   ((equal_set s1 s2) (equal_set s2 s1))
   (t nil)
)
)

(defun test_two_sets()
 (assert
      (and
       (equal nil (two_sets '(1 2 3) '(1 3)))
       (equal t (two_sets '(0 231 23 1 3) '(0 23 231 3 1)))
)))

(defun test_is_set()
  (assert
      (and
       (equal nil (is_set '(1 2 3 4 4)))
       (equal t (is_set '(1 2 3 4 67)))
       (equal nil (is_set '(1 3 1 31 2)))
)))

(test_insert_elem)
(test_sum_atom)
(test_sublists)
(test_two_sets)
(test_is_set)
