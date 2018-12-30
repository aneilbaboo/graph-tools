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



;;; $Id: arc-graph.lisp,v 1.3 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $


;;; File: arc-graph.lisp
;;; Description: Describes a graph composed of nodes and arcs; 
;;;              really an abstraction over a labelled graph

;;;
;;;  *******  NOTE: FUNCTIONALITY IN THIS FILE HAS NOT BEEN TESTED *******
;;;


;;;
;;; ARC-GRAPH - graph composed of nodes and arcs
;;;           - conceptually, arcs and nodes are similar to verticies and edges,
;;;               with the exceptions that two nodes can be connected by multiple
;;;               arcs, and that arcs can be labelled.
;;;           - Nodes and Arcs are represented at verticies.  Nodes verticies connect
;;;               to arc verticies (never directly to other node verticies).  
;;;           - Arc verticies have 1 outgoing edge in directed graphs,
;;;                and 2 outgoing edges in undirected graphs
;;;           - Arc verticies always are labelled with the symbol GRAPH-TOOLS:ARC,
;;;                as well as any user-specified values.
;;;           
;;;
(in-package :graph-tools)

(defstruct (arc-graph (:include labelled-graph)
                      (:constructor %make-arc-graph
                       (matrix directedp &optional labels descriptors)))
  ;; descriptors - each elt corresponds to a vertix, either 
  ;;                       a number = the "node number"
  ;;                       a cons = (from-node . to-node)             
  (descriptors (make-default-vertex-descriptors matrix) :type simple-vector))

(defmethod copy-graph ((g arc-graph))
  (let ((newg (call-next-method g)))
    (setf (arc-graph-descriptors newg) (copy-seq (arc-graph-descriptors newg)))
    newg))

(defun coerce-to-arc-graph (g)
  (flet ((make-graph (m &key directedp node-labels)
           (let ((g (make-arc-graph (list '(arc) (graph-matrix-edges m)) :directed nil
                                    :node-labels node-labels)))
             (setf (graph-directedp g) directedp)
             g)))
    (etypecase g
      (simple-vector  (make-graph g :directedp t))
      (graph          (make-graph (matrix-of g) :directedp (graph-directedp g)))
      (labelled-graph (make-graph (matrix-of g) :directedp (graph-directedp g)
                                  :node-labels (graph-labels g))))))

(defun make-default-vertex-descriptors (matrix)
  (loop with len = (array-dimension matrix 0)
        with d = (make-array len)
        for i from 0 below len
        do (setf (svref d i) i)
        finally (return d)))

(defun compute-max-vertex-from-arcspecs (arcspecs)
  (loop for aspec in arcspecs
        for especs = (rest aspec)
        maximize (compute-max-vertex-from-edge-specs especs)))

(defun fix-arc-descriptor (orinode tonode directed)
  (if (and (< tonode orinode) (not directed))
      (cons tonode orinode)
    (cons orinode tonode)))
             
(defun make-arc-graph (arcspecs &key node-labels directed)
  (let* ((num-nodes      (max (length node-labels)
                              (1+ (compute-max-vertex-from-arcspecs arcspecs))))
         (node-labels    (map-into (make-array num-nodes) node-labels))
         (next-vertex    num-nodes)
         (arclabels      ())
         (arcdescriptors ()))
    (labels ((make-edge (label orinode tonode)
               (prog1 next-vertex
                 (push label arclabels)
                 (push (fix-arc-descriptor orinode tonode directed) arcdescriptors)
                 (incf next-vertex)))
             (convert-edges-to-edgespecs () 
               ;; this procedure is simplified by mapping nodes to the first N verticies
               ;;    where N is the number of nodes.  The remaining verticies are used to 
               ;;    represent edges
               (loop for lespec in arcspecs
                     for edge-label = (cons 'arc (first lespec))
                     nconc (loop for espec in (rest lespec)
                                 nconc (loop with orinode = (first espec)
                                             for tonode in (rest espec)
                                             for edge = (make-edge edge-label orinode tonode)
                                             nconc `((,orinode ,edge) (,edge ,tonode))))))
             (compute-descriptors ()
               (make-array next-vertex 
                           :initial-contents (nconc (loop for i from 0 below num-nodes collect i)
                                                    (nreverse arcdescriptors)))))
      (let ((matrix      (make-graph-matrix (convert-edges-to-edgespecs) directed))
            (descriptors (compute-descriptors))
            (labels      (concatenate 'vector
                                      node-labels
                                      (nreverse arclabels))))
        (assert (= (length descriptors) (length labels) (length matrix)))
        (%make-arc-graph matrix directed labels descriptors)))))



(defun arc-graph-node-vertex (g n)
  "Returns the vertex index corresponding to the node N"
  (position n (arc-graph-descriptors g)))

(defun arc-graph-node-arcs (g n)
  "Returns the arcs in the form (arc-label . to-node)"
  (map-bits 'list (lambda (bit)
                    (bit-position bit))
            (svref (matrix-of g)
                   (arc-graph-node-vertex g n))))

(defun %map-arc-graph (result-type g fn descriptor-type)
  (loop with descriptors = (arc-graph-descriptors g)
        for i from 0 below (graph-vertex-count g)
        when (typep (svref descriptors i) descriptor-type)
          if result-type collect (funcall fn i) into result
          else           do (funcall fn i)
        finally (return (coerce result result-type))) )

(defun arc-graph-map-node-verticies (result-type g fn)
  "Function of one argument, the vertex index"
  (%map-arc-graph result-type g fn 'integer))

(defun arc-graph-map-arc-verticies (result-type g fn)
  (%map-arc-graph result-type g fn 'cons))
  
(defun arc-graph-arc-verticies (g from to &optional test)
  "Returns a list of matrix vertex indexes which represent ARCS which connect FROM and TO nodes.  Note that FROM and TO are node indexes, not vertex indexes.  TEST is a label list or predicate (of 1 arg) which evaluates evaluates the labels."
  (let* ((matrix       (matrix-of g))
         (to-index     (arc-graph-node-vertex g to)) ; to vertex index
         (tobit        (bit-position to-index))
         (from-index   (arc-graph-node-vertex g from)) ; from vertex index
         (labels       (arc-graph-labels g))
         (label-test   (graph-type-label-test-predicate g)))
    (delete-if #'null 
               (map-bits 
                'list 
                (lambda (edgebit)
                  (let ((edge-bit-vertex (bit-position edgebit)))
                    (when (and (logtest tobit (svref matrix edge-bit-vertex))
                               (funcall label-test
                                        test
                                        (svref labels edge-bit-vertex)))
                      edge-bit-vertex)))
                (svref matrix from-index)))))

(defun arc-graph-node-neighbors (g n)
  "Returns a list of neighbor nodes that arcs from this node point to"
  (let* ((matrix         (matrix-of g))
         (arc-verticies  (map-bits 'list
                                   #'bit-position
                                   (svref matrix
                                          (arc-graph-node-vertex g n))))
         (descriptors     (arc-graph-descriptors g)))
    (map-into arc-verticies
              (lambda (arc-vert)
                (let* ((ad (svref descriptors arc-vert))
                       (orinode (car ad)))
                  (if (= orinode n) (cdr ad) orinode)))
              arc-verticies)))


