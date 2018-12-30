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


;;; $Id: labelled-graph.lisp,v 1.7 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $

;;; File: labelled-graph.lisp
;;; Description: defines the LABELLED-GRAPH structure which represents a directed or undirected
;;;              labelled graph; the labels are stored as a vector as long as the graph-matrix.
;;;              
;;;
;;; LABELLED-GRAPH: vertex-labelled graph
;;; -----------------------------------
;;;       - data structure for representing vertex-labelled graphs
;;;
;;;

(in-package :graph-tools)

;;;
;;; DATA STRUCTURES
;;;
(defstruct (labelled-graph (:include graph)
                           (:conc-name graph-)
                           (:constructor %make-labelled-graph
                            (&key matrix directedp labels)))
  (labels (make-array (array-dimension matrix 0)) :type simple-vector))       ;; a vector, elements are lists, representing independent attributes

(defmethod print-object ((g labelled-graph) stream)
  (print-unreadable-object (g stream :type t :identity t)
    (let ((directedp (graph-directedp g)))
      (pprint-logical-block (stream ())
        (format stream 
                ;; clisp doesn't tolerate pretty formatting (~I, ~_) inside print-unreadable-object
                (if #-:clisp *print-pretty* #+:clisp nil
                  "~:I~A,~:[UN~;~]DIRECTED ~{~_(~{~{~S=~S~}~@[:~{~S~^,~}~]~})~^ ~}" 
                  "~A,~:[UN~;~]DIRECTED ~{(~{~{~S=~S~}~@[:~{~S~^,~}~]~})~^ ~}")
                (graph-vertex-count g)
                directedp
                (loop for from from 0 below (graph-vertex-count g)
                      for neighbors = (graph-vertex-outputs g from)
                      ;;;;                     ;; for undirected graphs, avoid printing duplicates
                      ;;;;                     for print-neighbors = (if directedp neighbors
                      ;;;;                                               (remove-if (lambda (to) (< to from)) 
                      ;;;;                                                          neighbors))
                      for label = (graph-vertex-label g from)
                      collect `((,from ,label) 
                                ,neighbors)))))))

(defun deep-copy-seq (seq)
  (map (type-of seq) (lambda (elt)
                       (if (typep elt 'sequence) (deep-copy-seq elt)
                         elt))
       seq))

(defmethod copy-graph ((g labelled-graph))
  (let ((newg  (call-next-method g)))
    (setf (graph-labels newg) (deep-copy-seq (graph-labels newg)))
    newg))


;;;
;;; constructor
;;;
(defun make-labelled-graph (edge-specs 
                            vertex-labels
                            &optional
                            directed 
                            (constructor '%make-labelled-graph))
  (let* ((labels-vector (coerce vertex-labels 'vector))
         (matrix        (make-graph-matrix (list* (1- (length vertex-labels))
                                                  edge-specs) 
                                           directed)))
    (when (> #1=(array-dimension matrix 0) #2=(array-dimension labels-vector 0))
      (error "Too few labels provided to MAKE-LABELLED-GRAPH - ~S vertexes, but only ~S labels." #1# #2#))
    (funcall constructor
             :matrix matrix
             :directedp directed
             :labels labels-vector)))

;;;
;;; reorder
;;;
(defmethod reorder-graph ((g labelled-graph) r &optional %hint)
  (declare (type labelled-graph g) (type list r) (ignorable %hint))
  (let* ((oldlabels (graph-labels g)))
    (declare (type simple-vector oldlabels))
    (setf (graph-labels g) 
          (map 'vector 
               (lambda (oldi) (svref oldlabels oldi))
               r))
    (call-next-method)))


(defun graph-vertex-label (g i)
  (svref (graph-labels g) i))

(defun (setf graph-vertex-label) (value g i)
  (setf (svref (graph-labels g) i) value))
;;; 
;;; LABEL TESTING, SORTING
;;; 
(defun labelled-graph-canonical-labels-set (g &optional label-test)
  "Computes an ordered set of labels for graph g (eliminating duplicates)"
  (sort (delete-duplicates (coerce (graph-labels g) 'list)
                           :test (or label-test (graph-type-label-equal-predicate g)))
        (graph-type-label-less-predicate g)))
                         
(defun labelled-graph-sorted-labels (g)
  "Returns a list of labels in a sorted order"
  (sort (coerce (graph-labels g) 'list)
        (graph-type-label-less-predicate (type-of g))))

;;;
;;; LABEL TEST PREDICATES - subclasses may provide specialized methods
;;;
(defgeneric graph-type-label-equal-predicate (type)
  (:documentation "Returns a predicate of two arguments (labels for this type of graph).  The predicate returns T if, for this graph type, the two labels are equal. Default is #'EQUAL. Users who wish to provide subclasses of LABELLED-GRAPH can override this method to enable specific handling of labels.")
  (:method ((g structure-object))
   (graph-type-label-equal-predicate (type-of g)))
  (:method ((s symbol))
   #'equal))

(defgeneric graph-type-label-less-predicate (type)
  (:documentation "Given a subtype of labelled graph, returns a predicate of two arguments, LABEL1 and LABEL2.  This predicate is used for sorting labels.")
  (:method ((g structure-object))
   (graph-type-label-less-predicate (type-of g)))
  (:method ((s symbol))
   (lambda (x y)
     (or (< (sxhash (type-of x)) (sxhash (type-of y)))
         (let ((*package* #.(find-package "GRAPH-TOOLS"))
               (strx (format nil "~S" x))
               (stry (format nil "~S" y)))
           (string< strx stry))))))

(defgeneric graph-label-test-predicate (s)
  (:documentation "Given a graph S, returns a predicate of 3 arguments which takes a index into the graph S, a second graph G and an index into that graph (Gi).  The predicate returns T if the label Si of S is satisfied by label Gi of G.")
  (:method ((s labelled-graph))
   (let ((slabels (graph-labels s)))
     (lambda (si glabel)
       (equal (svref slabels si) glabel)))))

;;;; (defgeneric graph-type-label-test-predicate (type)
;;;;   (:documentation "Given a subtype of labelled graph, returns a predicate of 2 arguments, where the first is a label of a graph, the second represents a test.  Default resturs the GRAPH-TYPE-LABEL-EQUAL-PREDICATE for the graph-type in question.  Subclasses of labelled-graph can override this test to provide specialization, for example subset testing.  This predicate is used in subgraph isomorphism testing.")
;;;;   (:method ((g structure-object)) (graph-type-label-test-predicate (type-of g)))
;;;;   (:method ((s symbol)) (graph-type-label-equal-predicate s)))

;;;
;;; METHODS for add/delete-verticies:
;;;
(defmethod graph-add-verticies ((g labelled-graph) (g2 labelled-graph))
  (graph-add-verticies g (graph-labels g2)))

(defmethod graph-add-verticies ((g labelled-graph) (labels sequence))
  ;; add these 
  (setf (graph-labels g)
        (concatenate 'vector (graph-labels g) labels))
  (graph-extend-verticies g (length labels)))

(defmethod graph-add-verticies ((g labelled-graph) (n number))
  ;; extend the labels array:
  (setf (graph-labels g)
        (map-into (make-array (+ n (graph-vertex-count g)))
                  #'identity (graph-labels g)))
  ;; and extend the verticies array
  (graph-extend-verticies g n))

(defmethod graph-delete-verticies ((g labelled-graph) vlist &key not)
  (call-next-method) ; deletes the verts 
  (setf (graph-labels g)  ; delete the labels
        (delete-indicies (graph-labels g) vlist not))
  g)


#|
;;;
;;; LABEL SORTING:
;;;
;;; these two vars can be changed to specify a different sorting mechanism:
(defvar *labelled-graph-label-sort-key* 'identity
  "Function used to access the key on which graph labels are sorted (default is IDENTITY)")
(defvar *labelled-graph-label-sort-predicate* 'default-label-sort-predicate
  "Binary predicate (suitable for SORT function) used to sort graph labels")

(defun default-label-sort-predicate (x y)
  (or (< (sxhash (type-of x)) (sxhash (type-of y)))
      (let ((*package* #.(find-package "GRAPH-TOOLS"))
            (strx (format nil "~S" x))
            (stry (format nil "~S" y)))
        (string< strx stry))))

(defun sort-labels (seq)
  "Sorts a set of labels"
  (sort seq *labelled-graph-label-sort-predicate*
        :key *labelled-graph-label-sort-key*))

(defun labels< (x y)
  (funcall *labelled-graph-label-sort-predicate*
           (funcall *labelled-graph-label-sort-key* x)
           (funcall *labelled-graph-label-sort-key* y)))

|#
