(in-package :graph-tools)

(eval-when (:compile-toplevel :execute)
  (defvar *test-group* "")
  (defmacro test-group (descr &body body)
    `(eval-when (:compile-toplevel :execute)
       (format t "BEGIN TEST-GROUP ~:@(~A~)~%" ',descr)
       (let ((*test-group* ,descr))
         ,@body)
       (format t "END TEST-GROUP ~:@(~A~)~%" ',descr)))
  (defmacro test (descr &body forms)
    `(eval-when (:compile-toplevel :execute)
       ,@(loop for form in forms
               for test = (first form)
               for i = 1 then (1+ i)
               collect  
               `(unless ,form
                  (format t "~&  Failed \"~A\"~%" ,descr)
                  (error
                   "~@[In test group \"~@(~A~)\":~]~%~
                    Failed while performing test \"~A\" ~@[#~A~]:~%~
                    ~<(~S~<~{ ~S~}~>)~>~% => NIL~%~
                    FORM: ~S"  
                   *test-group*
                   ',descr ,(if (> (length forms) 1) i)
                   '',test (list ,@(rest form))
                   ,form )))
       (format t "~&  Passed \"~A\ [~A test~:P]~%" ,descr ,(length forms)))))

(test-group "node-tree"  
  ; a tree with different order for depth-first versus breadth-first search
  (let ((tree '(1 
                (2
                 (3
                  (4)))
                (5
                 (6))))
        (tree2 '(1 (2 (3 (4)))
                   (5 (6 (7 (8))))
                   (9 (10)))))

    (flet ((>3 (x) (> x 3))
           (^2 (x) (* x x)))
      
          (test "map over node-cars, depth first"
            (equal (map-nodes #'identity tree :car t)
                   '(1 2 3 4 5 6)))
          (test "map over node-cars, breadth-first"
            (equal  (map-nodes #'identity tree :car t :breadth-first t)
                    '(1 2 5 3 6 4))
            (equal (map-nodes #'identity tree2  :car t :breadth-first t)
                   '(1 2 5 9 3 6 10 4 7 8)))
          (test "map over nodes"
            (equal (map-nodes #'identity tree)
                   '((1 (2 (3 (4))) (5 (6))) (2 (3 (4))) (3 (4)) (4) (5 (6)) (6))))
          (test "reverse node map"
            (equal (map-nodes #'identity tree
                              :reverse t
                              :car t)
                   '(6 5 4 3 2 1)))
          (test "reverse map child nodes"
            (equal (map-nodes #'identity tree :car t :reverse-children t)
                   '(1 5 6 2 3 4)))
          (test "reverse node map & reverse children"
            (equal (map-nodes #'identity tree
                              :car t  :reverse t :reverse-children t)
                   '(4 3 2 6 5 1)))      
          (test "map-nodes depth search"
            (equal (map-nodes #'identity tree :depth 0 :car t)
                   ())
            (equal (map-nodes #'identity tree :depth 1 :car t)
                   '(1))
            (equal (map-nodes #'identity tree :depth 2 :car t)
                   '(1 2 5))
            (equal (map-nodes #'identity tree :depth 3 :car t)
                   '(1 2 3 5 6))
            (equal (map-nodes #'identity tree :depth 4 :car t)
                   '(1 2 3 4 5 6))
            (equal (map-nodes #'identity tree :depth 5 :car t)
                   '(1 2 3 4 5 6)))
          (test "reverse depth search"
            (equal (map-nodes #'identity tree :depth 0 :reverse t :car t)
                   ())
            (equal (map-nodes #'identity tree :depth 1 :reverse t :car t)
                   '(4))
            (equal (map-nodes #'identity tree :depth 2 :reverse t :car t)
                   '(6 4 3))
            (equal (map-nodes #'identity tree :depth 3 :reverse t :car t)
                   '(6 5 4 3 2))
            (equal (map-nodes #'identity tree :depth 4 :reverse t :car t)
                   '(6 5 4 3 2 1))
            (equal (map-nodes #'identity tree :depth 4 :reverse t :car t)
                   '(6 5 4 3 2 1)))
          (test "find a node using a key"
            (equal (find-node 3 tree :key #'car)
                   '(3 (4)))
            (equal (find-node 2 tree :key #'car) '(2 (3 (4)))))
          (test "find a node-car"
            (= 4
               (find-node 4 tree :car t)
               (find-node-if #'>3 tree :car t))
            (= (find-node 2 tree :car t) 2)
            (null (find-node :this-doesnt-exist tree :car t)))
          (test "collect-nodes-if"
            (equal (collect-node-if #'>3 tree :car t)
                   '(4 5 6)))
          (test "delete-nodes"
            (equal (delete-node 3 tree :car t)
                   '(1 (2) (5 (6)))))

          (test "find-route"
            (equal (find-route 8 tree2 :key #'car)
                   '(1 5 6 7 8))
            (equal (find-route 1 tree2 :key #'car)
                   '(1))
            (equal (find-route 0 tree2 :key #'car) nil)))))

