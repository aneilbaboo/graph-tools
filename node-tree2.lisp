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

(in-package :graph-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
         ,result)))))

(defun depth-first-map (fn tree &optional depth)
  (with-accumulator accum 
    (depth-first-traversal (lambda (x) accum (funcall fn x)) tree depth)))

(defun breadth-first-map (fn tree &optional depth)
  (with-accumulator accum
    (breadth-first-traversal (lambda (x) accum (funcall fn x)) tree depth)))

(defun depth-first-traversal (fn tree &optional depth)
  (labels ((internal-traversal (node depth)
             (unless (zerop depth)
               (funcall fn node)
               (mapc (lambda (elt)
                       (internal-traversal elt (1- depth)))
                     (rest node))
               node)))
    (internal-traversal tree (or depth -1))))

(defun breadth-first-traversal (fn tree &optional depth)
  (labels ((internal-map-children (children depth)
             (unless (zerop depth)
               (mapc (lambda (child)
                       (funcall fn child))
                     children)
               (mapc (lambda (child)
                       (internal-map-children (rest child) (1- depth)))
                     children))))
    (let ((depth (or depth -1)))
      (unless (zerop depth)
        (funcall fn (first tree))
        (internal-map-children (rest tree) (1- depth))
        tree))))

(defun resolve-traversal (traversal)
  (case traversal
    (:depth-first 'depth-first-traversal)
    (:breadth-first 'breadth-first-traversal)
    (t              (assert (or (functionp traversal) (symbol-function traversal))
                        (traversal)
                      "Expecting either :DEPTH-FIRST, :BREADTH-FIRST or a traversal function, but received ~S"
                      traversal)
                    traversal)))

(defun find-node-if-car (fn tree &key test key (traversal 'depth-first-traversal) depth)
  (let ((key (or key #'identity)))
    (catch 'found
      (funcall traversal (lambda (node)
                           (when (funcall fn (funcall key (car node)))
                             (throw 'found node)))
               tree)
      nil))) 

(defun collect-node-if ())

(defun remove-node-if (


;;;; (defun depth-first-map-list (fn tree)
;;;;   (list* (funcall fn (first tree))
;;;;          (mapcan (lambda (elt)
;;;;                    (depth-first-map-list fn elt))
;;;;                  (rest tree))))

;;;; (defun depth-first-traversal (fn tree &optional depth)
;;;;   (labels ((internal-traversal (tree depth)
;;;;              (unless (zerop depth)
;;;;                (funcall fn (first tree))
;;;;                (mapc (lambda (elt)
;;;;                        (internal-traversal fn elt (1- depth)))
;;;;                      (rest tree))
;;;;                tree)))
;;;;     (internal-traversal tree (or depth -1))))

;;;; (defun breadth-first-map-list (fn tree)
;;;;   (labels ((internal-map-children (children)
;;;;              (append (mapcar (lambda (child)
;;;;                                (funcall fn (car child)))
;;;;                              children)
;;;;                      (mapcan (lambda (child)
;;;;                                (internal-map-children (rest child)))
;;;;                              children))))
;;;;     (list* (funcall fn (first tree))
;;;;            (internal-map-children (rest tree)))))
;;;;                              