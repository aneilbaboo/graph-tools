;;;; This file is part of little b.

;;;; The MIT License

;;;; Copyright (c) 2003-2008 Aneil Mallavarapu

;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;;; THE SOFTWARE.


;;; $Id: node-tree.lisp,v 1.6 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $

;;; Description: 
;;;     Functions for dealing with trees composed of "nodes"
;;;     Breadth and depth-first traversals in forward or reverse order are supported.
;;;       
;;;     API:
;;;         traverse-nodes - maps a function over a node tree, returning the tree
;;;         map-nodes - maps a function over a node tree, returning a list of
;;;                          values returned by the function
;;;         find-node/find-node-if - finds a node 
;;;         collect-node/collect-node-if - returns a list of matching nodes 
;;;         delete-node/delete-node-if - deletes a node (side-effecting the tree!)
;;;         count-node/count-node-if - counts matching nodes
;;;         find-route/find-route-if - finds a route from the node to a matching node
;;;
;;;     Adding and removing children is a supported by simple list operations:
;;;        See the section on Representation.
;;;        
;;;     
;;;     Most of the functions in the API support a standard set of keywords:
;;;         BREADTH-FIRST - causes the traversal to be breadth- rather than depth-first
;;;         TEST - specifies a test when matching an item (default is EQL)
;;;         KEY - specifies a key into the node or datum
;;;         CAR - specifies that the function being mapped expects the NODE-CAR
;;;               as an argument (if NIL (default), the NODE is passed)
;;;         NOT - inverts the match criterion
;;;         REVERSE - performs the mapping in reverse order
;;;         REVERSE-CHILDREN - reverses the order in which children are accessed
;;;
;;; Representation:
;;;     Nodes are represented as lists.  
;;;
;;;     The CAR (the "node-car") contains user-supplied data (any Lisp object).
;;;     The CDR of the node are the children of the node.
;;;     Thus, the node-car is accessed with CAR, and the children with CDR.
;;;     NOTE:  All node-children must also be NODEs.
;;;
;;;     E.g.,  (1 (2) (3) (4 (5) (6))) ; a valid tree (1...) has 3 children
;;;                                    ; 4 has two children
;;;            (1 2 3) ; is invalid 2 and 3 are not nodes
;;; 
(in-package :graph-tools)

(eval-when (:compile-toplevel :execute)
(defmacro flet-node-test ((name (node-arg) (test key car not)
                                &body success-body)
                          &body body)
 (let ((fn     '#:fn))
   `(let* ((,key       (or ,key #'identity))
           (,fn        (lambda (,node-arg)
                         (declare (ignorable ,node-arg))
                         (when (funcall ,test (funcall key ,node-arg))
                           ,@(or success-body '(t)))))
           (,fn        (if ,not (lambda (n) (not (funcall ,fn n)))
                         ,fn))
           (,fn        (if ,car (lambda (o) (funcall ,fn (car o))) ,fn)))
      (flet ((,name (,node-arg)
               (funcall ,fn ,node-arg)))
        ,@body)))))

(defmacro with-accumulator (accum &body body)
  (let ((result '#:result)
        (last-cons '#:last-cons)
        (new-cons  '#:new-cons)
        (new-elt   '#:new-elt))
    `(let ((,result    ())
           (,last-cons nil))
       (flet ((,accum (,new-elt)
                (let ((,new-cons (cons ,new-elt nil)))
                  (if ,result
                      (setf (cdr ,last-cons) ,new-cons)
                    (setf ,result ,new-cons))
                  (setf ,last-cons ,new-cons))))
         ,@body
         ,result))))
;;;
;;; NODE-TREE API:
;;;
(defun traverse-nodes (fn tree &key car depth breadth-first reverse reverse-children)
  "Applies function to each node in tree.  If car is T, fn is called on the car of the node."
  (funcall (get-traversal-fn breadth-first reverse reverse-children) 
           (if car (lambda (node) (funcall fn (car node))) fn)
           tree
           (or depth -1)))

(defun map-nodes (fn tree &key car depth breadth-first reverse reverse-children)
  (with-accumulator collect
    (funcall (get-traversal-fn breadth-first reverse reverse-children)
             (if car (lambda (node) 
                            (collect (funcall fn (car node))))
               (lambda (node) (collect (funcall fn node))))
             tree
             (or depth -1))))

(macrolet ((def-item-test-traversal (name)
             (let ((name-if (intern (format nil "~A-IF" (symbol-name name)))))
               `(defun ,name (item tree &key test key depth car not)
                  (let ((test (or test #'eql)))
                    (,name-if (lambda (node) (funcall test item node))
                              tree
                              :key key
                              :depth depth
                              :car car
                              :not not))))))
  (def-item-test-traversal find-node)
  (def-item-test-traversal count-node)
  (def-item-test-traversal collect-node)
  (def-item-test-traversal delete-node)
  (def-item-test-traversal find-route))

(defun find-node-if (fn tree &key 
                        key
                        breadth-first
                        depth 
                        car 
                        not
                        reverse
                        reverse-children)
  (flet-node-test (node-test (node)
                             (fn key car not)
                             (throw 'found node))
    (let ((traversal-fn (get-traversal-fn breadth-first reverse reverse-children)))
      (catch 'found 
        (funcall traversal-fn #'node-test tree depth)
        nil))))


(defun collect-node-if (fn tree
                           &key 
                           key
                           breadth-first
                           car 
                           depth
                           not
                           reverse
                           reverse-children)
  (with-accumulator add-result
    (flet-node-test (node-test (node)
                               (fn key car not)
                               (add-result node))
      (let ((traversal-fn (get-traversal-fn breadth-first reverse reverse-children)))
        (funcall traversal-fn #'node-test tree depth)))))

(defun delete-node-if (fn tree
                          &key 
                          key
                          breadth-first
                          car 
                          depth
                          not                          
                          reverse
                          reverse-children)
  (flet-node-test (node-test (node)
                             (fn key car not))
    (let ((traversal-fn (get-traversal-fn breadth-first reverse reverse-children)))
      (funcall traversal-fn (lambda (node)
                       (setf (cdr node)
                             (delete-if #'node-test
                                        (cdr node))))
                               tree depth)
      tree)))

(defun count-node-if (fn tree
                         &key
                         key 
                         breadth-first
                         car
                         depth
                         not                         
                         reverse
                         reverse-children)
  (let ((traversal-fn  (get-traversal-fn breadth-first reverse reverse-children))
         (count  0))
    (flet-node-test (counter (node)
                             (fn key car not)
                             (incf count))
      (funcall traversal-fn #'counter
               tree depth)
      count)))

(defun find-route-if (fn tree &key depth car key not)
  "Where x may be an item or a node"
  (let ((last-depth -1)
        (current-route ()))
    (flet ((route-generator (node)
             (declare (special depth))
             (if (> depth last-depth)
                 (setf current-route (last current-route)))
             (setf last-depth depth)
             (push node current-route)
             (funcall fn node)))
      (when (find-node-if #'route-generator tree 
                          :depth depth :car car :key key :not not)
        (nreverse current-route)))))
      

;;;
;;; support functions
;;;
(defun depth-first-traversal-fn (reverse-children)
  (let ((child-accessor (if reverse-children (lambda (x) (reverse (rest x))) #'rest)))
    (flet ((depth-first-traversal (fn tree depth)
             (labels ((internal-traversal (node depth)
                        (declare (special depth)) ; used by other funcs in this file
                        (unless (zerop depth)
                          (funcall fn node)
                          (mapc (lambda (elt)
                                  (declare (special depth))
                                  (internal-traversal elt (1- depth)))
                                (funcall child-accessor node))
                        node)))
             (internal-traversal tree (or depth -1)))))
      #'depth-first-traversal)))


(defun breadth-first-traversal-fn (reverse-children)
  (let ((child-accessor (if reverse-children (lambda (x) (reverse (rest x))) #'rest))) 
    (flet ((breadth-first-traversal (fn tree depth)
             (labels ((internal-traversal (nodes depth)
                      (declare (special depth)) ; used by other funcs in this file
                      (when (and nodes (not (zerop depth)))
                        (let ((next-nodes ()))
                          (dolist (node nodes)
                            (funcall fn node)
                            (setf next-nodes (append next-nodes 
                                                     (funcall child-accessor node))))
                          (internal-traversal next-nodes (1- depth))))))
               (internal-traversal (list tree) (or depth -1))
               tree)))
      #'breadth-first-traversal)))

  
(defun reverse-traversal (traversal-fn)
  (lambda (fn tree &optional requested-depth)
    (let ((rev-nodes ()))
      (funcall traversal-fn
               (lambda (node)
                 (declare (special depth))
                 (push (cons node (abs depth)) rev-nodes))
               tree
               -1)
      (loop with lowest-depth = (1+ (cdar rev-nodes))
            with requested-depth = (if (= -1 requested-depth)
                                       lowest-depth requested-depth)
            for (node . ndepth) in rev-nodes 
            when (< (- lowest-depth ndepth)
                    requested-depth)
            collect (funcall fn node)))))


(defun get-traversal-fn (breadth-first reverse child-reverse)
  (let* ((fn (if breadth-first
                 (breadth-first-traversal-fn child-reverse)
               (depth-first-traversal-fn child-reverse)))
         (fn  (if reverse (reverse-traversal fn) fn)))
    fn))

;;;; node accessors (meh - why bother?)
;;;;
;;;; (defun node-add-child (node child)
;;;;   (assert (listp child))
;;;;   (push child (cdr node))
;;;;   node)

;;;; (defun nodep (x)
;;;;   (and (listp x)
;;;;        (every #'nodep (node-children x))))
;;;; (defun node-data (node) (car node))
;;;; (defun node-children (node) (cdr node))
;;;; (defun (setf node-children) (c n) (setf (cdr node) c))
;;;; (defun (setf node-data) (d n) (setf (car node) d))
;;;; (defun make-node (data &rest children)
;;;;   (assert (every #'nodep children) (children)
;;;;     "One or more children passed to make-node are not nodes."
;;;;   (list* data children)))
;;;; (defun breadth-first-traversal-fn (reverse-children)
;;;;   (let ((child-accessor (if reverse-children (lambda (x) (reverse (rest x))) #'rest))) 
;;;;     (flet ((breadth-first-traversal (fn tree depth)
;;;;              (labels ((internal-map-children (children depth)
;;;;                         (declare (special depth))
;;;;                         (unless (zerop depth)
;;;;                           (mapc (lambda (child)
;;;;                                   (funcall fn child))
;;;;                                 children)
;;;;                           (mapc (lambda (child)
;;;;                                   (declare (special depth))
;;;;                                   (internal-map-children 
;;;;                                    (funcall child-accessor child)
;;;;                                    (1- depth)))
;;;;                                 children))))
;;;;                (internal-map-children
;;;;                 (list tree)
;;;;                 (or depth -1))
;;;;                tree)))
;;;;       #'breadth-first-traversal)))
