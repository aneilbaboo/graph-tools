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



;;; $Id: graph-matrix.lisp,v 1.5 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $

;;; File: graph-matrix.lisp
;;; Description: defines functions for creating and manipulating adjacency matricies

;;;
;;; GRAPH-MATRIX:
;;; ---------------------
;;; Graph adjacency matrix represented as a vector of integers.  
;;;
;;; Each integer represents a 0-based vertex of the graph.
;;; Each 1 bit of the integer represents an edge to another vertex.
;;; Bit position corresponds to vertex number:  bit at pos 0 corresponds to
;;; vertex 0, bit at pos 1 corresponds to vertex 1, etc.
;;;
;;; Thus, the triangle graph which looks like this
;;;   
;;;    0 -> 1 -> 2 -> 0
;;;        
;;; is the adjacency matrix:
;;; BIT-POS:    2 1 0
;;; -----------------
;;; VERTEX 0      0 1 0      ; edges from vertex 0: 0 -> 1
;;; VERTEX 1      1 0 0      ; edges from vertex 2: 1 -> 2
;;; VERTEX 2      0 0 1      ; edges from vertex 2: 2 -> 0
;;;
;;; Equivalent to the vector #(#B010 #B100 #B1) == #(2 4 1)
;;;
;;; The number of verticies in the graph can be determined from the length
;;; of the vector.  The length of the graph matrix is always 1+ the highest
;;; vertex.
;;;


;;;    
;;; Main functions in this file:
;;; ----------------------------
;;;
;;; make-graph-matrix   - given a list of edge specs, produces a vector representing 
;;;                         the graph (see below)
;;; make-labelled-graph-matrix - labelled version
;;; graph-matrix-edges           - returns the edge spec which constitutes
;;;                         the graph.  Useful for displaying graph structure.
;;; find-subgraph-matrix-isomorphisms - given two graph-matricies, s and a graph g, 
;;;                              finds all ismorphisms of s in g.
;;; make-numeric-edge-spec    - converts an labelled edge spec to a numeric edge spec
;;; make-labelled-edge-spec   - makes a labelled edge spec given a numeric edge spec + list of labels
;;;
;;;             
;;; Typical usage:
;;; (setf s (make-graph-matrix '((0 1 2) (1 2)) t) ; 0->1, 0->2, 1->2
;;;       g (make-graph-matrix '((3 2 1) (2 1) (2 4) (0 3 4))))
;;;
;;; (mapcar #'graph-matrix-edges (find-subgraph-matrix-isomorphisms s g))
;;;
;;; Edge Spec:  graph-matrix input spec
;;; ----------------------------
;;;   Informally a list of edge-entries and vertex-entries
;;;   '((vertex1 vertex1-neighbor1 vertex-neighbor2 ... to-vertex1-neighborn)
;;;     (vertex2 vertex2-neighbor2 ...)
;;;     vertex-n)
;;;  Edge entries are lists.  The first element is the originating vertex,
;;;    the rest are destination verticies.  
;;;  Vertex entries are labels.  Including them in the edge-spec forces the graph-matrix
;;;    to include these verticies even if the vertex has no edges to or from it.
;;;
;;;  E.g., '((0 1 2) (1 3) (2 3)) represents a diamond:      1
;;;                                                         / \
;;;                                                        0   3
;;;                                                         \ /
;;;                                                          2
;;;
;;; Pseudo BNF for edge spec:
;;;   ( [vertex-entry | edge-entry]* )
;;;   vertex ::= any lisp object
;;;   vertex-entry ::= non-list
;;;   edge-entry ::= ( vertex* )


(in-package :graph-tools)

(deftype graph-matrix (&rest args) `(simple-vector ,@args))

;; constructors
(defun make-graph-matrix (edge-specs &optional directedp)
  "Given a list of edge specifiers, returns an vector representing the graph 
   adjacency matrix.  An edge specifier is a list beginning with a vertex number
   and followed by verticies which it points to.
   Each element represents one vertex; the value is an integer, representing 
   all the adjacent verticies pointed at from element vertex. 
   E.g., (make-graph-matrix '((0 1))) => #(2 1) == #(#B10 #B01)
         (make-graph-matrix '((0 1)) t) => #(2 0) = #(#B10 #B0)"
  (etypecase edge-specs
    (simple-vector edge-specs)
    (t             (extend-graph-matrix nil edge-specs directedp))))

(defun compute-max-vertex-from-edge-specs (especs)
  (loop for espec in especs
        maximize (if (atom espec) espec
                   (loop for e in espec maximize e))))


;;;
;;; INFO
;;; 
(defun graph-matrix-vertex-outputs (g i)
  "Lists the vertex indicies connected by outgoing edges of the graph g"
  (declare (type simple-vector g) (type integer i))
  (map-bits 'list #'bit-position (graph-vertex-bitfield g i)))

(defun graph-matrix-vertex-inputs (g i)
  (declare (type simple-vector g) (type integer i))
  (loop with m = (matrix-of g)
        for v from 0 below (graph-vertex-count g)
        when (logbitp i (svref m i))
        collect i))
  
(declaim (inline graph-matrix-size))
(defun graph-matrix-size (g)
  (array-dimension g 0))
;;;
;;; IMPLEMENTATION
;;;                                  
(defun calculate-graph-matrix-size-from-edge-specs (edge-specs)
  (1+ (loop for es in edge-specs
            maximize (if (atom es) es (apply #'max es)))))
  

(defun graph-matrix-edges (g &key (directed t) (pairs nil))
  "returns the list of edges (pairs of verticies) which constitute
   the graph-matrix.  Useful for displaying graph structure."
    (do (verticies
         (end (array-dimension g 0))
         (from-vertex 0 (1+ from-vertex)))
        ((= from-vertex end) (reverse verticies))
      (cond 
       (pairs
        (loop with to-vertex-bits = (svref g from-vertex)
              for to-vertex = (log-lowbit-pos to-vertex-bits)
              until (zerop to-vertex-bits)
              do (setf (ldb (byte 1 to-vertex) to-vertex-bits) 0)
              (cond
               (directed       (push (list from-vertex to-vertex) verticies))
               (t              (pushnew (if (< from-vertex to-vertex)
                                            (list from-vertex to-vertex)
                                          (list to-vertex from-vertex))
                                        verticies
                                        :test #'equalp)))))
       (t (let* ((to-verticies (map-bits 'list #'bit-position (svref g from-vertex)))
                 (to-verticies (if directed to-verticies
                                 (remove-if (lambda (o) (< o from-vertex)) to-verticies))))
            (if to-verticies (push (list* from-vertex to-verticies) verticies)))))))


(defun print-graph-matrix (g &optional directed (stream *standard-output*))
  (let ((pts (graph-matrix-edges g :directed directed :pairs t)))
    (cond
     (directed (format stream "~{~{~A->~A~}~^, ~}" pts))
     (t        (format stream "~{~{~A-~A~}~^, ~}" pts)))))

(defun print-graph-matricies (gs &optional directed (stream *standard-output*))
  (mapcar (lambda (g) (print-graph-matrix g directed stream)) gs))



;;;
;;; reordering
;;;
(defun reorder-graph-matrix (g r)
  "Reorders a graph-matrix according to reordering r, which is a list which stores the old indexes
in order of the new indexes.
E.g., (reorder g '(2 1 0)) ; reorders g with verticies reversed"
  (loop with len = (array-dimension g 0)
        with newg = (make-array len :initial-element 0)
        with rbits = (loop with a = (make-array len) 
                           for i = 0 then (1+ i)
                           for old-vertex in r
                           do (setf (svref a old-vertex) (make-bit  i))
                           finally (return a))
        for old-vertex in r
        for new-vertex = 0 then (1+ new-vertex)
        for old-bits = (svref g old-vertex)
        do (loop with new-bits = 0 
                 for low-bit = (log-lowbit old-bits) ; bit representing the vertex to map to
                 until (zerop low-bit)
                 do (setf new-bits (logior new-bits
                                           (svref rbits (bit-position low-bit)))
                          old-bits (logandc1 low-bit old-bits))
                 finally (setf (svref newg new-vertex) new-bits))
        finally (return newg)))

(defun remap-graph-matrix (g m)
  "Same as reorder graph-matrix, but m is a bit-matrix which maps verticies to new verticies"
  (reorder-graph-matrix g (map 'list #'bit-position m)))


;;; random graph-matricies
(defun make-random-graph-matrix (num-verticies num-edges &key directedp (min-edges 0))
  (let ((edges (nconc
                (loop for j from 0 below min-edges
                      nconc (loop for i from 0 below num-verticies
                                  collect (list i (random num-verticies))))
                (loop for i from 1 to (- num-edges (* min-edges num-verticies))
                      collect (list (random num-verticies) (random num-verticies))))))
    (make-graph-matrix edges directedp)))


          
;;; comparison
(defun graph-matrix< (g1 g2)
  "Compares the adjacency matricies of 2 graph-matrixs"
  (loop with l1 = (length g1)
        with l2 = (length g2)
        for i from 0 to (1- (min l1 l2))
        for n1 = (svref g1 i)
        for n2 = (svref g2 i)
        do (cond 
            ((< n1 n2) (return t))
            ((< n2 n1) (return nil)))
        finally (return (< l1 l2))))

(defun graph-matrix= (g1 g2) (equalp g1 g2))

(defun graph-matrix> (g1 g2) (not (or (graph-matrix< g1 g2) (graph-matrix= g1 g2))))

(defun nth-position (n item sequence &key (test 'eql) (start 0) from-end (key 'identity))
  "Finds the position of the nth matching item in sequence (n is zero-based)"
  (flet ((test (elt)
           (and (funcall test elt (funcall key item))
                (minusp (decf n)))))
    (typecase sequence
      (list (if from-end
                (loop with rev-seq = (reverse sequence)
                      for i from (length sequence) downto start 
                      for elt in rev-seq
                      when (test elt)
                      do (return (1- i)))
              (loop with subseq = (subseq sequence start)
                    for i = start then (1+ i)
                    for elt in subseq
                    when (test elt)
                    do (return i))))
      (sequence (if from-end
                    (loop with rev-seq = (reverse sequence)
                          for i from (length sequence) downto start 
                          for elt across rev-seq
                          when (test elt)
                          do (return (1- i)))
                  (loop with subseq = (subseq sequence start)
                        for i = start then (1+ i)
                        for elt across subseq
                        when (test elt)
                        do (return i)))))))
     
;;;
;;; Matrix modification:
;;;
(defun graph-matrix-add-edges (m edge-specs &optional directed)
  (dolist (edge-spec edge-specs)
    (let ((from (car edge-spec))
          (to-verticies (cdr edge-spec)))
      (setf (svref m from) (logior (svref m from) (make-bits to-verticies)))
      (unless directed 
        (loop with fbit = (make-bit from)
              for to-vertex in to-verticies
              do (setf (svref m to-vertex)
                       (logior (svref m to-vertex) fbit))))))
  m)

(defun graph-matrix-delete-edges (m edge-specs &optional directed)
  (dolist (edge-spec edge-specs)
    (let ((from (car edge-spec))
          (to-verticies (cdr edge-spec)))
      (setf (svref m from) (logandc2 (svref m from) (make-bits to-verticies)))
      (unless directed 
        (loop with fbit = (make-bit from)
              for to-vertex in to-verticies
              do (setf (svref m to-vertex)
                       (logandc2 (svref m to-vertex) fbit))))))
  m)
  
(defun extend-graph-matrix (g edge-specs &optional directed size)
  (loop with max-vertex = (max (length g)
                             (or size
                                 (1+ (compute-max-vertex-from-edge-specs edge-specs))))
        with matrix = (map-into (make-array max-vertex :initial-element 0)
                                #'identity g)
        for espec in edge-specs
        when (consp espec)
        do (let* ((origin (first espec))
                  (oribit (make-bit  origin)))
             (setf (svref matrix origin)
                   (if directed
                       (loop for dest in espec
                             for bits = (svref matrix origin) 
                                        then (logior bits (make-bit  dest))
                             finally (return bits))
                     (loop for dest in espec
                           for bits  = (svref matrix origin) 
                                       then (logior bits (make-bit  dest))
                           do (setf (svref matrix dest) 
                                    (logior oribit (svref matrix dest)))
                           finally (return bits)))))
        finally (return matrix)))

;;;
;;; VERTEX REMAPPING - allows copying of edge info from one vertex to another
;;;   
(defun graph-matrix-copy-vertex-outputs (m v1 v2)
  "Copies the outputs of one or more verticies (V2) to another (V1), 
   M - graph matrix
   V1 - a vertex index.
   V2 - a vertex index or list
   Returns M"
  ;; add v2 outputs to v1 outputs:
  (setf (svref m v1) 
        (if (listp v2) 
            (apply #'logior (svref m v1) (mapcan (lambda (f)
                                                   (graph-vertex-outputs m f))
                                                 v2))
          (logior (graph-vertex-outputs m v1) (graph-vertex-bitfield m v2))))
  m)

(defun graph-matrix-merge-verticies (m v1 v2)
  "Returns a new graph matrix where verticies V2 (an index or list) are merged onto vertex V1"
  (let ((work (copy-seq m)))
    (graph-matrix-copy-vertex-outputs work v1 v2)
    (graph-matrix-copy-vertex-inputs work v1 v2)
    (graph-matrix-remove-verticies work v2)))

(defun graph-matrix-copy-vertex-inputs (m v1 v2 &optional bitfieldp)
  "Copies all the V2 incoming edges to V1.  
   In other words, all verticies that output to V2 will also output ot V1 after this
   operation completes.
   M - graph matrix
   V1 - a vertex index.
   V2 - a vertex index or list.
   BITFIELDP - bool indicating that V2 is a bitfield"  
  ;; add inputs
  (loop with v2-bits = (if bitfieldp v2 (make-bits v2))
        with v1-bit = (make-bit v1)
        for i from 0 below (graph-vertex-count m)
        ;; i->from contain bits representing outputs from i to any of the from edges
        for i->from = (logand (graph-vertex-bitfield m i) v2-bits)
        do (map-bits nil 
                     (lambda (ibit) 
                       (let ((v2 (bit-position ibit)))
                         (setf (svref m v2) (logior (svref m v2) v1-bit))))
                     i->from)
        finally (return m)))

(defun graph-matrix-remove-verticies (m v &key not)
  "Efficiently get rid of verticies, returning a new matrix."
  (let ((vbits (if not (logandc2 (bitmask (graph-vertex-count m))
                                 (make-bits v))
                 (make-bits v))))
    (graph-matrix-remove-verticies2 m vbits)))

(defun graph-matrix-remove-verticies2 (m vbits)
  (loop with mlen = (graph-vertex-count m)
        with newm = (make-array (- mlen (logcount vbits)))
        with newmi = 0
        for i from 0 below mlen
        unless (logbitp i vbits)
        do (setf (svref newm newmi) 
                 (delete-bits (svref m i) vbits))
           (incf newmi)
        finally (return newm)))

(defun add-graph-matricies (m1 m2)
  (loop with m1len = (graph-vertex-count m1)
        with m2len = (graph-vertex-count m2)
        with combined = (map-into (make-array (+ m1len m2len)) #'identity m1)
        for m2i from 0 below m2len
        do (setf (svref combined (+ m2i m1len))
                 (ash (graph-vertex-bitfield m2 m2i) m1len))
        finally (return combined)))
  
