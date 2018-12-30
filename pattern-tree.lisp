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


;;; $Id: pattern-tree.lisp,v 1.6 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $

;;; Description: Implements an database for storing a set of graphs, S = S1...Sn.
;;;              A query of a graph G on this database returns all subisomorphisms
;;;              which map graphs in S into G.

;;; $Id: pattern-tree.lisp,v 1.6 2008/09/06 00:23:08 amallavarapu Exp $

 
;;; The pattern-tree enables efficient matching of a large number of related 
;;; subgraphs to a set of test graphs.   The matching algorithm, presented with a
;;; test graph, iterates through the subgraphs beginning with the smallest first.
;;; 
;;; If it encounters a matching subgraph S1, all subgraphs which contain S1
;;; are potential matches.  Conversely, if a test graph fails to match a subgraph
;;; S2, all subgraphs which contain S2 cannot possibly match the test graph.  
;;;
;;; Subgraphs are stored in a tree which allows branches representing fruitless
;;; isomorphism tests to be pruned.  This is achieved by storing the subgraphs
;;; such that the smallest (common) subgraphs are near the root of the tree, and
;;; larger subgraphs toward the leaves.  Along any branch, the relationship  between
;;; nodes is such that the node closer to the root will always contain a subgraph of 
;;; its children nearer the leaf.  
;;;
;;; As test graphs are added to the 
;;;
;;; A subgraph tree is a node-tree of pnode structures
;;;   (PARENT-PNODE (CHILD-PNODE1 ) (CHILD-PNODE2 ))
;;;
;;; Each pnode contains a subgraph representing a pattern which may be found
;;;   in matching graphs (stored in mdata structures).
;;;
(in-package :graph-tools)

(defstruct (pattern-tree
            (:constructor %make-pattern-tree (pattern-isofn match-isofn)))
  root            ;; a list of 0 or more pnode structures
  pattern-isofn   ;; computes isomorphisms from a pattern graph to a pattern graph
  match-isofn     ;; computes isomorphisms from a pattern graph to a match graph
  match-graphs)   ;; stores a list of all graphs which have been matched 
  

(defstruct pnode
  object              ;; an object which is either a graph or an object which contains a graph, 
                      ;;    accessible by pattern-tree-graph-accessor
  isomorphisms        ;; a list of isomorphisms from the parent graph to this node's graph
  children            ;; 0 or more pnode structures representing patterns
  id)

(defun make-pattern-tree (&key pattern-label-test match-label-test)
  (%make-pattern-tree (make-ptree-isofn match-label-test)
                      (make-ptree-isofn pattern-label-test)))

(defun map-pattern-tree (fn ptree)
  (labels ((map-pnode (pnode)
             (list* (funcall fn pnode)
                    (mapcar #'map-pnode (pnode-children pnode)))))
    (mapcar #'map-pnode (pattern-tree-root ptree))))
             
(defun show-pattern-tree-growth (ptree &rest args)
  "A debugging function: args = pattern/id pairs (show-pattern-tree-growth ptree pgraph1 :myid1 pgraph2 :myid2 ..."
  (loop with ptree = (or ptree (make-pattern-tree))
        for graph = (pop args)
        for id = (pop args)
        while graph
        do (pattern-tree-add-pattern ptree graph id)
           (format t "~<+ ~S => ~>~S~%" id 
                   (map-pattern-tree #'pnode-id ptree))))

(defun make-ptree-isofn (label-test)
  (flet ((isofn (pnode graph parent-isos)
           ;; returns isomorphisms from pnode-graph to graph
           ;; parent-isos is a list of isomorphisms from the parent of pnode to graph
           (declare (ignorable parent-isos))
           
           (find-subgraph-isomorphisms (pnode-graph pnode) graph :label-test label-test)))
    #'isofn))

(defun pattern-tree-find-matches (ptree graph)
 "Finds all a list of matches of graph in the pattern tree, a match is a list of the form
  (PATTERN-GRAPH ISOMORPHISMS ...)"
 (let ((match-isofn (pattern-tree-match-isofn ptree)))
   (mapcan (lambda (pnode)
             (find-matches-on-pnode pnode graph match-isofn nil))
           (pattern-tree-root ptree)))) 

(defun find-matches-on-pnode (pnode graph match-isofn parent-isos)
  "Returns a list of matches of graph against PNODE's graph and its children
   each match has the form: (PATTERN-GRAPH ISOMORPHISMS...)
   GRAPH is the graph to be matched
   PNODE is the pnode to be matched against 
   MATCH-ISOFN is the isomorphism function
   PARENT-ISOS are the isomorphisms of GRAPH to the parent of PNODE"
  (let ((isos (funcall match-isofn pnode graph parent-isos)))
    (when isos 
      (list* (list* pnode isos)
             (mapcan (lambda (child)
                       (find-matches-on-pnode child graph match-isofn isos))
                     (pnode-children pnode))))))
                   

(defun pattern-tree-add-pattern (ptree p &optional id)
  "Adds the pattern to the tree and returns matches"
  (let ((pnode (make-pnode :graph p :id id))
        (itable (make-hash-table :test #'equal))) ; cache of isomorphisms
    (setf (pattern-tree-root ptree)
          (add-pnode pnode
                     (pattern-tree-root ptree)
                     (pattern-tree-pattern-isofn ptree)
                     itable))))

(defun find-pnode-isos (pnode1 graph parent-isos isofn itable)
  (let ((key (cons pnode1 graph)))
    (multiple-value-bind (isos cached) 
        (gethash key itable)
      (cond
       (cached isos)
       (t      (setf (gethash key itable)
                     (funcall isofn pnode1 graph parent-isos)))))))

(defun find-pnode-parent-match (pnode pnode-list isofn itable)
  "Finds the first match where a pnode in pnode-list is a subgraph of pnode. "
  (let ((pnode-graph (pnode-graph pnode)))
    (find-if (lambda (p)
               (find-pnode-isos p pnode-graph nil isofn itable))
             pnode-list)))

(defun add-pnode (pnode pnode-list isofn itable)
  "Returns a new list.  Pnode will either be an element of pnode-list or a descendent of one of the elements of pnode-list (via the pnode-children operator)"
  (or (inject-pnode pnode pnode-list isofn itable)
      (introduce-pnode pnode pnode-list isofn itable)))

(defun introduce-pnode (pnode pnode-list isofn itable)
  "Adds pnode to pnode-list, subsuming members of pnode-list as children, if necessary"
  (loop for c in pnode-list
        ;; collect the supergraphs of pnode
        if (find-pnode-isos pnode (pnode-graph c) nil isofn itable)
        collect c into pnode-children
        else
        collect c into new-pnode-list
        finally (progn
                  ;; pnode children are subsumed by pnode:
                  (setf (pnode-children pnode) pnode-children)
                  (return (list* pnode new-pnode-list)))))

(defun inject-pnode (pnode pnode-list isofn itable)
  "Tries to add pnode as a child of one of the elements of pnode-list. May return NIL, indicating failure."
  (let ((parent-pnode (find-pnode-parent-match pnode pnode-list isofn itable)))
    (when parent-pnode
      (setf (pnode-children parent-pnode)
            (add-pnode pnode 
                       (pnode-children parent-pnode)
                       isofn
                       itable))
      pnode-list)))

(defun compute-pnode-match (pnode graph parent-isos match-isofn)
  (let ((isos (funcall match-isofn (pnode-graph pnode) graph parent-isos)))
    (when isos
      (list* graph isos))))

(defun remap-isomorphism (i1 i2)
  "Where I1 is an isomorphism mapping verticies of subgraph G1 to graph G2.   I2 is an isomorphism of G2 to another graph, G3.  The returned value is an isomorphism of G1 to G3."
  ;; I1 = G1 --subgraph--> G2
  ;; I2 = G2 --subgraph--> G3
  ;; Output:   I3 = G1--subgraph-->G3
  (map 'vector (lambda (g2vertex) (svref i2 g2vertex)) i1))


(defun compute-remapped-isomorphism-set (i1set i2set)
  (remove-duplicates  
   (mapcan (lambda (i1)
             (mapcar (lambda (i2)
                       (remap-isomorphism i1 i2))
                     i2set))
           i1set)
   
   :test #'equalp))