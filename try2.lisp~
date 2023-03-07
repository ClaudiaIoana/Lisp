;return the number of levels of trees of type(1)
; vertices = noduri
; edges = muchii
; when number edges + 1 = number vertices we know we finished it

(defun left_subtree_parse(tree nv ne)
  (cond
   ((null tree) nil)
   (( = nv (+ 1 ne)) nil)
   (t (cons (car tree) (cons (cadr tree) (left_subtree_parse (cddr tree) (+ 1 nv) (+(cadr tree) ne)))))
)
)

(defun left_subtree(tree)
  (left_subtree_parse (cddr tree) 0 0)
)

(defun right_subtree_parse(tree nv ne)             
  (cond
   ((null tree) nil)
   ((= nv (+ 1 ne)) tree)
   (t (right_subtree_parse (cddr tree) (+ 1 nv)(+(cadr tree) ne)))
)
)

(defun right_subtree(tree)
  (right_subtree_parse (cddr tree) 0 0)
)

(defun nr_levels(tree)
  (cond
   ((null tree) 0)
   ((+ 1 (max (nr_levels(left_subtree tree)) (nr_levels(right_subtree tree)))))
)
)

(defun test_nr_levels()
  (equal 3 (nr_levels '(A 2 B 0 C 2 D 0 E 0)))
  (equal 4 (nr_levels '(A 2 B 0 C 2 D 1 E 0 F 0))
))
