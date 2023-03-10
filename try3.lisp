;;Write a function to check if an atom is member of a list(non-linear) - use map functions

(defun check_member(l e)
  (cond
   ((and (atom l) (equal l e)) 1)
   ((listp l)(apply '+(mapcar #'(lambda (x)(check_member x e)) l)))
   (t 0)
))

(defun check_member2(l e)
  (cond
   ((and (atom l) (equal l e)) 1)
   ((listp l) 0)
   ((t 0))
))

(defun check_member_help (l e)
  (apply '+(mapcar #'(lambda (x)(check_member2 x e)) l))
)
  

(defun is_member(l e)
  (if (> (check_member2 l e) 0) t nil)
)
  
(defun test_member()
  (assert
      (and
       (equal (is_member '(1 (4 2 4) (21 3 (9)) 3) 9) t)
       (equal (is_member '(1 3 2 (4 2 4) 3) 11) nil)
)))
