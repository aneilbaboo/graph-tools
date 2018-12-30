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


;;; $Id: pattern-network.lisp,v 1.4 2008/09/06 00:23:08 amallavarapu Exp $

;;; Description: Implements an database for storing a set of graphs, S = S1...Sn.
;;;              A query of a graph G on this database returns all isomorphisms
;;;              mapping S in G.
 
;;; The pattern-network enables efficient matching of a large number of related 
;;; subgraphs ("pattern graphs") to an input graph.   A network-building function accepts 
;;; a pattern-network structure and a pattern graph, and adds that graph to a network.  
;;; A matching algorithm, accepts a pattern-network and an input graph, and returns
;;; a list of matched subgraphs and isomorphisms from each subgraph to the input graph.
;;; 
;;; The pattern-network structured contains a directed acyclic graph, the nodes of which
;;; contain pattern-graphs.  The top of the network is a set of nodes is called the root.
;;; Each node has an associated list of 0 or more child-nodes.  A parent node always 
;;; contains a subgraph of its children.  Every node also stores the subgraph isomorphisms
;;; from the pattern graph in the node to the pattern graph in the parent node (root nodes
;;; excepted since they have no parents).  

;;; This structure allows the matching algorithm to prune unproductive isomorphism tests
;;; by observing that if an input graph doesn't match a parent node, it cannot match the
;;; child nodes.  Also, the structure provides the possibility (currently not utilized)
;;; to reuse the isomorphisms calculated from a match with a parent node to prune the 
;;; ismorphism search on child nodes.  

;;; The pattern-network is currently structured as a tree (no merges between branches).  
;;; However, merges would permit a more efficient matching operation: 
;;; When a child pattern contains matches to more than 1 other graph in the network, and 
;;; the matches involve exclusive nodes. The child-pattern should be attempted only if
;;; the input graph matches all parents. In the future, this may be implemented via a 
;;; join node.
;;;
(in-package :graph-tools)

(defstruct (pattern-network
            (:constructor %make-pattern-network (pattern-isofn match-isofn)))
  root            ;; a list of 0 or more pnode structures
  pattern-isofn   ;; computes isomorphisms from a pattern graph to a pattern graph
  match-isofn     ;; computes isomorphisms from a pattern graph to a match graph
  match-graphs)   ;; stores a list of all graphs which have been matched 
  
(defstruct pnode
  graph            ;; a pattern graph
  isomorphisms     ;; a list of isomorphisms from the parent graph to this node's graph
  children         ;; 0 or more pnode structures representing patterns
  id)

(defun make-pattern-network (&key pattern-label-test match-label-test)
  (%make-pattern-network (make-pnet-isofn match-label-test)
                         (make-pnet-isofn pattern-label-test)))

(defun map-pattern-network (fn pnet)
  (labels ((map-pnode (pnode)
             (list* (funcall fn pnode)
                    (mapcar #'map-pnode (pnode-children pnode)))))
    (mapcar #'map-pnode (pattern-network-root pnet))))
             
(defun compute-pattern-network-growth (pnet &rest args)
  "A debugging function: args = pattern/id pairs (show-pattern-network-growth pnet pgraph1 :myid1 pgraph2 :myid2 ..."
  (loop with pnet = (or pnet (make-pattern-network))
        for graph = (pop args)
        for id = (pop args)
        while graph
        do (pattern-network-add-pattern pnet graph id)
        collect (list* id (map-pattern-network #'pnode-id pnet))))

(defun make-pnet-isofn (label-test)
  (flet ((isofn (pnode graph parent-isos)
           ;; returns isomorphisms from pnode-graph to graph
           ;; parent-isos is a list of isomorphisms from the parent of pnode to graph
           (declare (ignorable parent-isos))
           
           (find-subgraph-isomorphisms (pnode-graph pnode) graph :label-test label-test)))
    #'isofn))

(defun pattern-network-find-matches (pnet graph)
 "Finds all a list of matches of graph in the pattern tree, a match is a list of the form
  (PATTERN-GRAPH ISOMORPHISMS ...)"
 (let ((match-isofn (pattern-network-match-isofn pnet)))
   (mapcan (lambda (pnode)
             (find-matches-on-pnode pnode graph match-isofn nil))
           (pattern-network-root pnet)))) 

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
                   

(defun pattern-network-add-pattern (pnet p &optional id)
  "Adds the pattern to the networks and returns matches"
  (let ((pnode (make-pnode :graph p :id id))
        (itable (make-hash-table :test #'equal))) ; cache of isomorphisms
    (setf (pattern-network-root pnet)
          (add-pnode pnode
                     (pattern-network-root pnet)
                     (pattern-network-pattern-isofn pnet)
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

;;;; (compute-pattern-network-growth nil +graph1+ :g1
;;;;                              +graph3+ :g2
;;;;                              +house-graph+ :hg
;;;;                              +house-graph-plus1+ :hg+1
;;;;                              +house-graph-plus2+ :hg+2
;;;;                              +double-house-graph+ :dbl-hg
;;;;                              +double-house-graph2+ :dbl-hg2
;;;;                              +double-house-graph2-plus1+ :dbl-hg2+1
;;;;                              +double-house-graph2-plus2+ :dbl-hg2+2)