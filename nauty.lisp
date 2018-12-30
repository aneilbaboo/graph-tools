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


;;; $Id: nauty.lisp,v 1.6 2008/11/04 22:56:47 amallavarapu Exp $
;;; $Name:  $

;;; File: nauty.lisp
;;; Description: implements the NAUTY algorithm
;;;
;;;  Main functions:
;;;        NAUTY - given a graph, returns a canonical graph plus other info
;;;        CANONICAL-GRAPH - returns canonical graph and reordering
;;;

(in-package :graph-tools)

;; graph-matrix: basic algorithm works on graph-matrix 
;;                 representation: an array of bitfields (numbers)
;;                 each bitfield representing one node of the graph,
;;                 each bit of the number representing and edge from 
;;                 that node to the node represented by the bit.
;;               labelled/arc graphs are handled by computing an initial
;;                 partition 
;;               

;; partition: nodes of a graph split into 1 or more cells (mutually exclusive subsets)
;;            representation: a list of cells, each cell represented as an integer (used as a bit field)
;; 
;;;;
;;;;
;;;;  public functions: 
;;;;           GRAPH-AUTOMORPHISMS - the nauty algorithm, computes canonical graph,
;;;;                                   as well as automorphisms and orbits
;;;;           CANONICAL-GRAPH - wrapper around graph-automorphisms, returns 
;;;;                                   canonical graph and a relabelling order
;;;;

;;;
;;; Implementation:
;;; 
(defun neighbor-node-invariant (g v c)
  "Computes # neighbors of vertex v in the node subset c of graph g.
   Where g is an array of bitfields, 
   v is a bit, the position of which is a vertex index in g
   c is a bitfield representing the node subset."
  (declare (type number c v) (simple-vector g))
  (logcount (logand (svref g (bit-position v)) c)))

(defun leaf-partition-p (p)
  (declare (type list p))
  (every (lambda (c)
           (declare (type integer c))
           (= (logcount c) 1)) p))

(defun graph-matrix-initial-partition (g)
  (list (bitmask (length g))))
  
(defun refine-cell (g cell separator)
  "Separates CELL into a list of cells.  
   Each returned cell has the same NEIGHBOR-NODE-INVARIANT w.r.t SEPARATOR
   G = adjacency matrix 
   CELL = integer bit vector of verticies which will be separated by the neighbor-node-invariant
   SEPARATOR = integer bit vector of verticies used to separate CELL"
  (declare (type integer cell separator) (simple-vector g))
  (loop with invar-table = (make-array (1+ (length g)) :initial-element 0) ; table of invariants
        for vbit = (log-lowbit cell)  ; vertex bit
        until (zerop (the integer vbit))
        do (let ((invar ; invariant for current vbit w.r.t separator
                  (neighbor-node-invariant g vbit separator))) 
             (setf ;; group the vbit with others which have same invar value:
                   (svref invar-table invar) (logior (svref invar-table invar) vbit)
                   ;; remove this vbit from the current cell
                   cell (logandc1 vbit cell)))
        finally (return (loop for cell across invar-table
                              unless (zerop cell) 
                              collect cell into cells
                              and sum 1 into num-cells
                              finally (return (values cells num-cells))))))

(defun introduce-stack-cells (s old-cell new-cells)
  "Given stack s, and the existing cell OLD-CELL, replaces OLD-CELL with NEW-CELLS (if OLD-CELLS is found in S)"
  (let ((new-cells (copy-list new-cells)))
    (cond
     ((null s)  new-cells)
     ((= old-cell (first s))
      (return-from introduce-stack-cells (nconc new-cells (rest s))))
     (t
      (loop for last = s then cons
            for cons on (rest s)
            for pcell = (first cons)
            when (= pcell old-cell)
            do (setf (cdr last) new-cells
                     (cdr (last new-cells)) (cdr cons))
            (return-from introduce-stack-cells s)
            finally (setf (cdr last) new-cells)
            (return-from introduce-stack-cells s))))))

(defun refine-partition (g &optional (p (graph-matrix-initial-partition g)) (s (copy-list p)))
  "Returns two values: REFINED, DISCRETEP.
   REFINED - the refined partition
   DISCRETEP - T if this is a leaf (discrete) partition"
  (declare (type simple-vector g) (type list p s))
  (multiple-value-bind (r l)
      (cond
       ((null s)             (values p (leaf-partition-p p)))
       ((leaf-partition-p p) (values p t))
       (t (loop with scell = (first s)
                with rstack = (rest s)
                for pcell in p
                if (> (logcount pcell) 1)
                nconc (multiple-value-bind (rcells count)
                          (refine-cell g pcell scell) 
                        (when (> count 1)
                          (setf rstack (introduce-stack-cells rstack pcell rcells)))
                        rcells) into refined-p 
                else collect pcell into refined-p
                finally (return (refine-partition g refined-p rstack)))))
    (values r l)))

(defun check-partition (g p &optional (msg "") &rest args)
  "Throws an error if cells have duplicate nodes"
  (loop for accum = 0 then (logior cell accum)
        for cell in p
        for i = 0 then (1+ i)
        unless (zerop (logand accum cell))
        do (error "~&~?Duplicate nodes in cell ~S of partition ~
                   (~{#B~B ~}) of graph ~S. ~%"
                  msg args i p g)
        finally (unless (equalp accum (first (graph-matrix-initial-partition g)))
                  (error "~?~%Missing nodes in partition (~{#B~B ~}) of graph ~S." p g msg args)))
  p)
     
(defun partition-children (p)
  (let ((pos-target (position-if (lambda (n) (> (logcount n) 1)) p)))
    (when pos-target
      (loop with prefix = (if (> pos-target 0) (subseq p 0 pos-target) nil)
            with orbits = (loop with bits = (nth pos-target p)
                                with remainingbits = bits
                                for curbit = (log-lowbit remainingbits)
                                until (zerop remainingbits)
                                collect (list curbit (logandc1 curbit bits))
                                do (setf remainingbits (logandc1 curbit remainingbits)))
            with suffix = (if (< pos-target (1- (length p))) (subseq p (1+ pos-target)))
            for orbit in orbits
            collect  (list (first orbit) (append prefix orbit suffix))))))

(defun internal-nauty (g initial-partition compute-canonical save-automorphisms)
  "This is the function which does the nauty algorithm" 
  (let ((canonical ())
        (canonical-partition ())
        (prev-gens (make-hash-table :test #'equalp)) ; a map from automorphisms to the most recent generators 
        (automorphisms (if save-automorphisms (make-hash-table :test #'equalp))))
    (labels ((update-canonical (newg newp)
               (when (and compute-canonical (or (null canonical) (graph-matrix< newg canonical)))
                 (setf canonical-partition newp
                       canonical newg))
               canonical)

             (greatest-common-ancestor-level (gen1 gen2 length)
               ;; length passed as a parameter so we don't have to recompute it
               (- length (mismatch gen1 gen2 :from-end t)))
             (traverse-tree (urp gen level)
               ;; urp = unrefined partition, gen = generator, level = 0 based level in tree
               (multiple-value-bind (p leafp) (refine-partition g urp)
                 (cond
                  ;; we've reached a leaf:
                  (leafp
                   (let* ((rg (reorder-graph-matrix g (mapcar #'bit-position p)))) ; reordered graph
                     (cond
                      (gen ; indicates partition children have been generated & orbits generated...
                       (let ((prev-gen (gethash rg prev-gens)))
                         (setf (gethash rg prev-gens) gen) ; record this most recent generator
                         (when save-automorphisms
                           (push p (gethash rg automorphisms)))
                         (when prev-gen ; generator already exists
                           ;; backtrack to the greatest common ancestor
                           (throw (greatest-common-ancestor-level prev-gen gen level) ; level=(length gen) here
                                  canonical))
                         (update-canonical rg p)))
                      
                      ;; no generator => we're at top of search tree...
                      ;; just return the reordered graph
                      (t (update-canonical rg p)))))

                  ;; non-leaf - iterate over each child
                  (t
                   (loop with new-level = (1+ level)
                         for (o c) in (partition-children p)
                         do
                         (catch level ;; set backtrack point
                           (traverse-tree c (list* o gen) new-level))
                         finally (return canonical)))))))
      (catch nil (traverse-tree (or initial-partition (graph-matrix-initial-partition g)) nil 0)
        (values canonical (mapcar #'bit-position canonical-partition) automorphisms)))))

(defun graph-matrix-automorphisms (g initial-partition canonical automorphisms orbits)
  "Depending on switches, returns CG,CGR,IS,ISR,OS
    If canonical is set:
    CG = canonical graph
    CRG = canonical graph reordering (list showing order of original nodes in CG)
    If automorphisms is set:
    AS = list of automorphisms 
    ASR = list, each element is a list of reorderings, corresponding an element of IS
    If orbits is set:
    OS = list of orbits (node automorphisms)"
  (multiple-value-bind (canon crelab automorphism-table)
      (internal-nauty g initial-partition canonical (or automorphisms orbits))
    (multiple-value-bind (automorphisms autrelab orbits)
        (convert-nauty-automorphism-table automorphism-table automorphisms orbits)
      (values canon crelab automorphisms autrelab orbits))))

(defun partition-to-reordering (p)
  (mapcar #'bit-position p))

(defun convert-nauty-automorphism-table (table automorphisms-p orbits-p)
  "Uses the automorphism table to calculate the set of automorphisms found.
   RETURNS: graphs, reorderings, orbits.
   There is 1 reordering for each graph;  
   One of the graphs will be the default ordering, (the reordering is an ordered list of numbers).   
   Orbits are lists of automorphic verticies, given in terms of the default ordering."
  (flet ((calculate-orbits (relabs)
           (remove-if (lambda (o) (= (length o) 1))
                      (reduce (lambda (orbits neworbit)
                                (loop with merged = nil
                                      for rest on orbits
                                      for orbit = (first rest)
                                      if (intersection neworbit orbit)  
                                      do (setf merged t
                                               (first rest) (if (> (length neworbit) (length orbit))
                                                                neworbit orbit))
                                      (loop-finish)
                                      finally (return (if merged orbits (list* neworbit orbits)))))
                              (list* () (apply #'mapcar (lambda (&rest nodes)
                                                          (remove-duplicates nodes))
                                               relabs))))))
    (if (or automorphisms-p orbits-p)
        (loop for g being the hash-keys in table
              for ptns being the hash-values in table
              for relabs = (mapcar #'partition-to-reordering ptns)
              if automorphisms-p
                collect g into graphs
                and collect relabs into reorderings
              if orbits-p
                collect (calculate-orbits relabs) into orbits
                finally (return (values graphs reorderings orbits)))
      (values nil nil nil))))

(defun canonical-graph-matrix (g &optional initial-partition)
  (multiple-value-bind (canon partition)
      (internal-nauty g initial-partition t nil)
    (values canon partition)))

;;;
;;; Support for graph objects:
;;; 
(defun initial-partition (g &optional label-test) 
  "The partition is a vector of 1 or more integers (bitfields), the bits of each integer representing some subset of the verticies.  The initial partition specifies an initial separation of verticies from one another.  For simple (unlabelled edge) graphs, all nodes are in one group (an integer with all bits set). Other graph types may distinguish nodes into groups (e.g., based on labels)."
  (etypecase g
    (simple-vector  (graph-matrix-initial-partition g))
    (labelled-graph (labelled-graph-initial-partition g label-test))
    (graph          (graph-matrix-initial-partition (matrix-of g)))))

;;;
;;; CANONICAL GRAPH:
;;;    Note: if a large # of identical verticies are present,
;;;    this function can be very slow.  A simple solution is to 
;;;    check for and remove identical verticies BEFORE calling
;;;    GRAPH-AUTOMORPHISMS, then add them back after reordering the
;;;    graph.  
;;;    Since this could be slightly costly, add an extra boolean arg
;;;    IDENTICAL-VERTEX-CHECK to allow the user to explicitly request
;;;    this check.
;;;
(defun canonical-graph (g)
  "Returns C,R 
   Where C is a canonical reordered graph isomorphic to G
   and   R is the isomorphism"
  (multiple-value-bind (cg crg) 
      (graph-automorphisms g :canonical t :automorphisms nil :orbits nil)
    (values cg 
            crg)))

(defun graph-automorphisms (g &key (canonical t) (automorphisms t) (orbits t) label-test)
  (etypecase g
    (simple-vector (graph-matrix-automorphisms (matrix-of g)
                                               (initial-partition g)
                                               canonical automorphisms orbits))
    (graph         (multiple-value-bind (cg crg as asr os)
                       (graph-matrix-automorphisms (matrix-of g)
                                                   (initial-partition g label-test)
                                                   canonical
                                                   automorphisms
                                                   orbits)
                     (declare (ignorable cg))
                     (values (reorder-graph (copy-graph g) crg cg)
                             crg as asr os)))))

(defun labelled-graph-initial-partition (g &optional label-test)
  (declare (type labelled-graph g)
           (ignorable options))
  (let* ((matrix               (matrix-of g))
         (lab-vertex-map       (make-hash-table :test #'equal))
         (labels               (graph-labels g))
         (canonical-labels-set (labelled-graph-canonical-labels-set g label-test)))
    (declare (type simple-vector matrix labels)
             (type hash-table lab-vertex-map)
             (type list canonical-labels-set))
    (loop for i from 0 below (array-dimension matrix 0)
          for lab = (svref labels i)
          do (setf (gethash lab lab-vertex-map)
                   (logior (gethash lab lab-vertex-map 0)
                           (make-bit i)))
          finally (return 
                   (map-into canonical-labels-set 
                             (lambda (lab)
                               (gethash lab lab-vertex-map))
                             canonical-labels-set)))))


;;;;
;;;;  EXAMPLE GRAPHS:
;;;;    
;;;; (defconstant +refine-only-graph+ (make-graph-matrix '((6 0 7 4 5) (7 1 4 2) (4 5) (3 5 2)))
;;;;   "The example graph from the SAUCY presentation which only requires refinement to distinguish all nodes")

;;;; (defconstant +problem-graph1+ #(0 0 512 0 32768 0 131072 16384 0 528388 0 4096 2560 16384 8320 16 0 524352 524288 393728))

;;;; (defconstant +triangle-corners-graph+ (make-graph-matrix '((0 1 2) (1 2) (3 0) (4 1) (5 2))))


;;;; (defun canonical-graph-test (&key (graphs 100) (trials 20) (nodes 30) (edges 40))
;;;;   (format t "Reordering ~S random graphs ~S times.   Each graph has ~S nodes ~S edges.~%~
;;;;              Testing that all reorderings are isomorphic."
;;;;           graphs trials nodes edges)
;;;;   (let ((begin-time (get-universal-time)))
;;;;     (dotimes (x graphs)
;;;;       (let* ((g (make-random-graph-matrix nodes edges))
;;;;              (c (canonical-graph-matrix g))) (format t "~%===============> ~S"x)
;;;;         (dotimes (y trials) 
;;;;           (let* ((rg (randomly-reorder-graph-matrix g))
;;;;                  (crg (canonical-graph-matrix rg)))
;;;;             (unless (equalp crg c)
;;;;               (error "Original graph => ~S~%Reorderled graph ~S => ~S~%" g c rg crg))))))
;;;;     (multiple-value-bind (sec min hour)
;;;;         (decode-universal-time (- (get-universal-time) begin-time))
;;;;       (format t "~%Finished testing ~S random graphs each reordered ~S times.  ~S nodes ~S edges.~%~
;;;;                  Elapse time: ~2,'0D:~2,'0D:~2,'0D~%~
;;;;                  Average time/graph: ~S seconds"
;;;;               graphs trials nodes edges
;;;;               hour min sec (coerce (/ (- (get-universal-time) begin-time)
;;;;                                       (* graphs trials)) 'float)))))

