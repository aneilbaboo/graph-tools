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


;;; $Id: package.lisp,v 1.9 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $

;;; File: package.lisp
;;; Description: defines the graph-tools package

;;; $Id: package.lisp,v 1.9 2008/09/06 00:23:08 amallavarapu Exp $

(defpackage graph-tools
  (:nicknames #:gtools)
  (:use cl)
  (:export 
   ;; types
   #:graph #:labelled-graph #:arc-graph
   #:graph-p #:labelled-graph-p #:arc-graph-p

   ;; constructors
   #:make-graph #:make-labelled-graph #:make-arc-graph
   #:reorder-graph

   ;; copiers
   #:copy-graph #:copy-labelled-graph #:copy-arc-graph

   ;; accessors
   #:matrix-of #:graph-edges #:graph-vertex-edge-p
   #:graph-vertex-count #:graph-vertex-outputs #:graph-vertex-inputs
   #:graph-vertex-outdegree
   #:graph-matrix #:graph-directedp #:graph-vertex-bitfield
   #:graph-labels #:graph-vertex-label

   ;; connectivity
   #:verticies-connected-p
   #:vertex-paths
   #:connected-vertex-groups
   #:merge-graphs #:merge-graph-matricies
   #:unconnected-subgraphs

   ;; modification
   #:graph-add-edges #:graph-delete-edges
   #:graph-add-verticies #:graph-delete-verticies
   #:graph-delete-verticies-if
   #:transpose #:merge-graphs #:merge-verticies
   #:coerce-to-undirected-graph 

   ;; randomization
   #:make-random-graph-matrix #:randomly-reorder-graph
   #:randomized-sequence

   ;; labelled graph comparison
   #:graph-type-label-equal-predicate
   #:graph-type-label-less-predicate
   #:graph-label-test-predicate

   #:arc-graph-matrix #:arc-graph-directedp
   #:arc-graph-descriptors #:arc-graph-arc-verticies

   ;; subgraph isomorphism
   #:find-subgraph-isomorphisms #:find-subgraph-isomorphism-maps
   #:ullman

   ;; nauty
   #:canonical-graph #:graph-automorphisms
   #:graph-matrix-automorphisms #:canonical-graph-matrix
   
   ;; node-tree api
   #:traverse-nodes #:map-nodes 
   #:find-node #:count-node #:collect-node #:delete-node #:find-route
   #:find-node-if #:collect-node-if #:delete-node-if #:count-node-if
   #:find-route-if

   ;; pattern-network
   #:make-pattern-network #:pattern-network-add-pattern #:pattern-network-find-matches
   
   ;; graph matrix functions
   #:print-graph-matricies #:graph-matrix-edges
   #:make-graph-matrix 
   #:graph-matrix-add-edges

   ;; bit operators
   #:make-bit #:make-bits #:pop-bit #:map-bits #:map-bits-into #:bitmask
   #:log-lowbit #:log-lowbit-pos #:log-highbit #:log-highbit-pos
   #:low-byte #:delete-bits #:find-bit #:bit-position 
   #:setbitsf #:clearbitsf #:setbitf #:clearbitf

   ;; graph-viz
   #:dot-form #:write-dot-script #:write-image-file #:show-object 
   #:with-dot-attributes
   ))
