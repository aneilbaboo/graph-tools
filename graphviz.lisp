;; -*- mode:Lisp -*-
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


;;; Description: Provides a LISP interface to dot (http://...),
;;;              a language for displaying graphs

;;; $Id: graphviz.lisp,v 1.3 2008/09/06 00:23:08 amallavarapu Exp $

(in-package :graph-tools)

(defstruct dot-format-info type attribute priority value)

(defvar *dot-format-attributes* ())

;;;;   '(:size '(2 2)
;;;;     :fontsize 12
;;;;     :site-style :filled
;;;;     :site-color :lightgrey
;;;;     :monomer-style :filled
;;;;     :monomer-color :white
;;;;     :node-separation 0
;;;;     :complex-style :filled
;;;;     :complex-color :grey
;;;;     :complex-fontsize 15))

(defmacro with-dot-format-attributes (prop-values &body body)
  "Associates dot attributes with LISP types.
   USAGE: (with-dot-format-attributes 
             ((TYPE1 (:PROP1 VAL1 [Priority1])
                     (:PROP2 VAL2 [Priority2])
                     ...)
              (TYPE2 ...))
            ..body)"
  `(let ((*dot-format-attributes* (copy-list *dot-format-attributes*)))
     ,@(mapcar (lambda (type-plist)
                 (destructuring-bind (type attribute value &optional (priority 0))
                     prop-value
                   `(set-dot-format-attribute ',type ,attribute ,value ,priority)))
               prop-values)
     ,@body))

(defun dot-format-attribute (type attribute)
  (ifit (find-if (lambda (o)
                   (and (subtypep type (dot-format-info-type o))
                        (eq attribute (dot-format-info-attribute o))))
                 *dot-format-attributes*)
      (dot-format-info-attribute it)))

(defun dot-format-attributes (type) 
  (remove-if-not (lambda (o)
                   (subtypep type (dot-format-info-type o)))
                 *dot-format-attributes*))

(defun set-dot-format-attribute (type attribute value &optional (priority 0))
  (setf *dot-format-attributes*
        (sort 
         (push 
          (make-dot-format-info
           :type type :attribute attribute :priority priority :value value)
          (delete-if (lambda (o) 
                       (and (eq (dot-format-info-attribute o) attribute)
                            (eq (dot-format-info-type o) type)))
                     *dot-format-attributes*))
         #'>
         :key #'dot-format-info-priority)))

(defconstant +curfile+ (or *load-truename* *compile-file-truename*))

(defun show-graph-object (o)
  (let ((tmpimg (merge-pathnames ".graph.png" +curfile+)))
    (write-graph-image o tmpimg :format :png)
    (asdf:run-shell-command "firefox ~A" tmpimg)))

(defun dotfile-to-image-file (dotfile output &optional (format :png))
  (asdf:run-shell-command "dot -T~(~A~) ~A > ~A" format dottmp output))

(defun write-image-file (object file &optional (format :png))
  (let ((dottmp (merge-pathnames ".graph.dot" +curfile+)))
    (with-open-file (dotstr dottmp
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :direction :io)
      (write-dot-script o dotstr))
    (dotfile-to-image-file dottmp file format)))

(defun at-symbol-p (x) 
  (and (symbolp x) (string= (symbol-name x) "@")))

(defconstant +lisp-to-dot-script-pprint-table+ (copy-pprint-dispatch))
(set-pprint-dispatch 'symbol (lambda (stream symbol)
                               (princ (string-downcase (symbol-name symbol)) stream))
                     0 +lisp-to-dot-script-pprint-table+)
(set-pprint-dispatch 'character (lambda (stream char)
                                  (with-standard-io-syntax (princ char stream)))
                     0 +lisp-to-dot-script-pprint-table+)
(set-pprint-dispatch 'null (lambda (stream char) ()) 1 +lisp-to-dot-script-pprint-table+)

(defun write-dot-script (form &optional (stream *standard-output*) (terminator #\;))
  (let ((head  (first form))
        (*print-pprint-dispatch* +lisp-to-dot-script-pprint-table+))
    (pprint-logical-block (stream nil)
      (etypecase head
        ((eql :cluster)
         (format stream "~{~A~}" form))
        (cons
         (pprint-logical-block (stream form :prefix "{ " :suffix "}")
           (loop while t
                 do (pprint-newline :mandatory stream)
                 (pprint-exit-if-list-exhausted)
                 (lisp-to-dot-script (pprint-pop) stream))))
        ;; class definitions: (:graph x (a b c)) => "graph x { a b c }"
        (keyword 
         (loop for elt in form
               do
               (cond 
                ((consp elt)
                 (pprint-newline :mandatory stream)
                 (lisp-to-dot-script elt stream))
                (t (format stream "~A " elt)))))
      
        ;; attributes (@ NAME x y z) => "NAME[x y z];"
        ((satisfies at-symbol-p)
         (princ (second form) stream)
         (pprint-logical-block (stream (nthcdr 2 form) :prefix "[" :suffix "];")
           (loop while t
                 do (pprint-exit-if-list-exhausted)
                 (lisp-to-dot-script (pprint-pop) stream nil)
                 (pprint-exit-if-list-exhausted)
                 (princ #\, stream)
                 (pprint-newline :mandatory stream))))
      
        ;; infix (= x y z) => "x = y = z;"
        (symbol (format stream (format nil "~~S~~{~~^~S~~S~~}~S" head terminator)
                        (second form) 
                        (cddr form)))))))
'(:graph g 
  ((:subgraph cluster0 
    ((@ :node 
        (= style :filled)
        (= :invertcolor :white))
     (= :label "mapk")
     (= :style :filled)
     (= :site0 :label)
     (-- mapk mapk mapk)))))
  


(defgeneric dot-form (o)
  (:documentation "Returns a list representing a dot expression for object O")
  (:method ((o cons) &optional (stream *standard-output*))
   `(:subgraph (dot-name ,o)
     
     

(defmethod dot-form :around (o &optional (stream *standard-output*))
  (let ((dform (call-next-method)))
    (ecase (first dform)
      ((:subgraph :record)
       `(,(first dform)
         ,(second dform)
         ,@(dot-attributes (type-of o))
         ,@(nthcdr 2 dform))))))
      
              
      

(defun write-dot-script (o &optional (stream *standard-output*))
  (let ((dform (dot-form o)))
    (format stream "graph ~A ~:@_" (type-of o))
    (pprint-logical-block (dotstr () :prefix "{" :suffix "}")
      (format stream "~:@_~
                      size=\"~{~A,~A~}\";~:@_~
                      ratio=compress;~:@_~
                      fontsize=~A;~:@_"
              (dot-format-attribute t :size)
              (dot-format-attribute t :fontsize))
      (apply #'write-dot-script-fragment cg dotstr properties))
    (fresh-line dotstr)))


