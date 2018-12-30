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



;;; $Id: graph.lisp,v 1.12 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $

;;; File: graph.lisp
;;; Description: defines the GRAPH structure which represents a directed or undirected
;;;              unlabelled graph
;;;              
(in-package :graph-tools)

(defstruct (graph (:constructor %make-graph (matrix directedp))
                  (:copier %copy-graph))
  (matrix #() :type vector)
  (directedp nil))

#-(or :sbcl :cmu)(declaim (inline graph-vertex-count))
(defun graph-vertex-count (g)
  (array-dimension (matrix-of g) 0))

(defgeneric directedp (g)
  (:method ((g vector)) t)
  (:method ((g graph)) (graph-directedp g)))

(defmethod print-object ((g graph) stream)
  (print-unreadable-object (g stream :type t :identity t)
    (format stream 
            #-:clisp "~:I~A,~:[UN~;~]DIRECTED ~{~_(~{~S:~{~S~^,~}~})~^ ~}" 

            ;; clisp does not tolerate line breaks or indents inside print-unreadable-object:
            #+:clisp "~A,~:[UN~;~]DIRECTED ~{(~{~S:~{~S~^,~}~})~^ ~}" 
            (graph-vertex-count g)
            (graph-directedp g)
            (mapcar (lambda (espec) (list (first espec) (rest espec)))
                    (graph-matrix-edges (matrix-of g) :directed nil)))))

#-(or :sbcl :cmu)(declaim (inline graph-vertex-bitfield))
(defun graph-vertex-bitfield (g i)
  "A bitfield map of outgoing edges from the vertex i"
  (etypecase g
    (vector (svref g i))
    (graph         (graph-vertex-bitfield (graph-matrix g) i))))

#-(or :sbcl :cmu)(declaim (inline graph-vertex-bitfield))
(defun (setf graph-vertex-bitfield) (bitfield g i)
  "Allows direct manipulation of the bitfield representing the adjacency matrix"
  (etypecase g
    (vector (assert (<= (integer-length bitfield) (graph-vertex-count g)) ()
                     "Graph has only ~S verticies, when attempt was made to set
                      output of edge ~S to vertex ~S." 
                     (graph-vertex-count g)
                     (integer-length bitfield)
                     i)
                   (setf (svref g i) bitfield))
    (graph         (setf (graph-vertex-bitfield (graph-matrix g) i) bitfield))))


#-(or :sbcl :cmu)(declaim (inline graph-vertex-edge-p))
(defun graph-vertex-edge-p (g i1 i2)
  (logtest (make-bit i2) (graph-vertex-bitfield g i1)))

(defun (setf graph-vertex-edge-p) (set g i1 i2 &optional (directedp (directedp g)))
  (setf (graph-vertex-bitfield g i1)
        (if set
            (logior (graph-vertex-bitfield g i1) (make-bit i2))
          (logandc2 (graph-vertex-bitfield g i1) (make-bit i2))))
  (unless (or directedp (= i1 i2))
    (setf (graph-vertex-edge-p g i2 i1 t) set))
  set)

#-(or :lispworks4 :cmu)
(define-compiler-macro (setf graph-vertex-edge-p) (&whole form set g i1 i2 &optional (directedp '(directedp g)))
  (case set
    ((nil t) (let* ((i1val '#:i1val)
                    (i2val (if (numberp i2) i2 '#:i2val))
                    (2bit  (if (numberp i2) (make-bit i2) `(make-bit ,i2val)))
                    (1bit  (if (numberp i1) (make-bit i1) `(make-bit ,i1val)))
                    (op    (if set 'logior 'logandc2))
                    (graph '#:graph)
                    (directed-form (case directedp
                                     ((t) nil) ; no need to set reverse edge
                                     ((nil)    ; this is an undirected graph, must set reverse edge
                                      `(setf (graph-vertex-bitfield ,graph ,i2val)
                                             (,op (graph-vertex-bitfield ,graph ,i2val)
                                                  ,1bit)))
                                     (otherwise 
                                      `(unless ,directedp
                                         (setf (graph-vertex-bitfield ,graph ,i2val)
                                               (,op (graph-vertex-bitfield ,graph ,i2val)
                                                    ,1bit)))))))
               `(let ((,graph ,g)
                      (,i1val ,i1)
                      ,@(unless (or (numberp i2) (null directed-form))
                          `((,i2val ,i2))))
                  ,directed-form                      
                  (setf (graph-vertex-bitfield ,graph ,i1val)
                        (,op (graph-vertex-bitfield ,graph ,i1val) ,2bit))
                  ,set)))
    (otherwise form)))
    

#-(or :sbcl :cmu) (declaim (inline graph-vertex-outputs (setf graph-vertex-outputs)))
(defun graph-vertex-outputs (g i)
  "A list of verticies connected by outgoing edges"
  (graph-matrix-vertex-outputs (matrix-of g) i))

(defun graph-vertex-outdegree (g i)
  "The outdegree of vertex i in graph g"
  (logcount (graph-vertex-bitfield g i)))

(defun (setf graph-vertex-outputs) (outputs g i &optional (directedp (directedp g)))
  "Changes the outgoing connectivity of the graph"
  (setf (graph-vertex-bitfield g i) (make-bits outputs))
  (unless directedp (setf (graph-vertex-inputs g i nil) outputs))
  outputs)

#-(or :sbcl :cmu)(declaim (inline graph-vertex-inputs (setf graph-vertex-inputs)))
(defun graph-vertex-inputs (g i &optional (directedp (directedp g)))
  "A list of verticies connected by incoming edges"
  (if directedp (graph-vertex-outputs g i)
    ;; shortcut, we assume that undirected graph has not been corrupted:
    (graph-matrix-vertex-inputs (matrix-of g) i)))

(defun (setf graph-vertex-inputs) (inputs g i &optional (directedp (directedp g)))
  (loop with m = (matrix-of g)
        with bit = (make-bit i)
        for input in inputs 
        do (setf (svref m input) (logior (svref m input) bit)))
  (unless directedp (setf (graph-vertex-outputs g i) inputs))
  inputs)

(defgeneric copy-graph (g)
  (:documentation "Deep copy graph")
  (:method ((g vector)) (copy-seq g))
  (:method ((g graph)) 
   (let ((newg (%copy-graph g)))
     (setf (graph-matrix g) (copy-seq (graph-matrix g)))
     newg)))
 
(defun matrix-of (g)
  (etypecase g 
    (vector g)
    (graph (graph-matrix g))))

(deftype directed-graph ()
  `(and graph
        (satisifies graph-directedp)))

(defun make-graph (edge-specs &optional directedp)
  (%make-graph (make-graph-matrix edge-specs directedp) directedp))

;;;
;;; GENERIC GRAPH REORDERING 
;;;
(defgeneric reorder-graph (g r &optional %internal)
  ;; the %internal argument is used to provide a precomputed graph-matrix
  (:documentation "Destructively reorders the verticies of graph g where r is a list of current vertex indicies in the new desired order.")
  (:method ((g vector) r &optional %internal) 
   (or %internal (map-into g #'identity (reorder-graph-matrix g r))))
  (:method ((g graph) r &optional %internal)
   (setf (graph-matrix g) (or %internal
                              (reorder-graph-matrix (graph-matrix g) r))) g))
         
(defgeneric print-graph (g &key directedp stream)
  (:documentation "A printed representation of the graph.  DIRECTEDP is a hint which asks the print systemto ignore duplicate reverse edges.")
  (:method :around (g &key (stream *standard-output*) directedp)
   (call-next-method :directedp directedp :stream stream)))


;;;
;;; RANDOMIZATION
;;;
(defun randomized-sequence (n)
  (loop for list = (loop for i from 0 to n collect i) then (delete selected list :test #'=)
        for len from (1+ n) downto 1
        for selected = (nth (random len) list)
        collect selected))

(defun randomly-reorder-graph (g)
  "Recomputes graph g, destructively modifying it, with a new vertex order."
  (let ((seq    (randomized-sequence (1- (array-dimension (matrix-of g) 0)))))
    (values (reorder-graph g seq) seq)))

;;;
;;; TRANSPOSE - destructive fn
;;;
(defgeneric transpose (g)
  (:documentation "Destructively transposes the graph")
  (:method ((g vector)) (map-into g #'identity (transpose-graph-matrix g)))
  (:method ((g graph))
     (setf (graph-matrix g) (transpose-graph-matrix g))
     g))

(defun transpose-graph-matrix (m)
  (loop with len = (array-dimension m 0)
        with tm = (make-array len :element-type 'integer :initial-element 0)
        for row from 0 below len
        for mrow = (svref m row)     
        for rowbit = (ash 1 row)
        do (loop for low-bit = (log-lowbit mrow)
                 until (zerop low-bit)
                 do (setf mrow (logandc2 mrow low-bit))
                    (let ((col (bit-position low-bit)))
                      (setf (svref tm col)
                            (logior (svref tm col)
                                    rowbit))))
        finally (return tm)))

;;;
;;; GRAPH CONNECTIVITY:
;;;

;;;
;;; UNCONNECTED-SUBGRAPHS
;;; 
(defun unconnected-subgraphs (g &key (verticies t) output-connected-p )
  "Returns a list of subgraphs of g which are disconnected from each other.  
   G is a graph
   OUTPUT-CONNECTED is a boolean which if set causes the function to consider
   verticies to be connected when they share an output vertex.  This setting
   only has meaning for directed graphs, since all edges in undirected graphs 
   are bidirectional.  If OUTPUT-CONNECTED-P is NIL, shared output verticies do
   not cause their inputs to be in a connected group; the shared output verticies
   will be copied to each subgraph they are a part of."
  (let ((working-graph (if (and output-connected-p (directedp g))
                           (coerce-to-undirected-graph (copy-graph g))
                         g)))

    (multiple-value-bind (partition reachables) (connected-vertex-groups working-graph :verticies verticies)
      (declare (ignorable partition))
      (loop for r in reachables ; r is a set of reachable verticies
            collect (graph-delete-verticies (copy-graph g) r :not t)))))
;;;
;;; CONNECTED-VERTEX-GROUPS - computes the sets of verticies which are connected (by output edges)
;;;                         - if one vertex is included in multiple groups, it will be returned only
;;;                           once in the partition (first returned value), and multiple times in 
;;;                           the reachable sets (second returned value). this is only relevant for 
;;;                           directed graphs.
;;;                         - an implementation of union-find 
(defun connected-vertex-groups (g 
                                &key
                                (verticies t)
                                (max-steps -1) 
                                (ignore nil))
  "Computes which verticies are connected to each other.      
    G = a graph, 
    VERTICIES = an optional list of vertex indexes (default T means use all indicies), 
                if NIL is passed, an empty graph is returned
    MAX-STEPS = the number of edges which may be traversed, 
    IGNORE = a list of  verticies which should not be traversed. 
   Returns: P,R
    P = a partition of verticies 
        (a list of lists each of which is a subset of VERTICIES, and the union of which
         is the same as VERTICIES (considered as a set))
    R = a list of lists of reachable verticies, one for each sublist of P."
  (let* ((m (matrix-of g))
         (all (bitmask (length m)))
         (verts (if (eq verticies t) all (make-bits verticies)))
         (i (make-bits ignore)))
    (unless (zerop verts)
      (loop for v1-bit = (log-lowbit verts) ;; select an arbitrary vertex (the lowest)
            for v1 = (bit-position v1-bit) 
            for unctd = (compute-unconnected-verticies
                         m                           
                         (logior (svref m v1) v1-bit) ; origin are all the adjacent verts + current vertex
                         all ; destination
                         max-steps
                         i)
            collect (map-bits 'list #'bit-position (logandc2 verts unctd)) into connected
            collect (map-bits 'list #'bit-position (logandc2 all unctd)) into included
            do (setf verts (logand verts unctd)) ; continue w/ unconnected verticies
            until (zerop verts)              ; until there are no more
            finally (return (values connected included))))))

;;;
;;; VERTICIES-CONNECTED-P
;;;
(defun verticies-connected-p (graph verticies &key (max-steps -1) ignore)
  (let ((m (matrix-of graph)))
    (zerop (compute-unconnected-verticies 
            m
            (svref m (first verticies))
            (make-bits (rest verticies))
            max-steps
            (make-bits ignore)))))

;; efficient internal-use recursive fn:
(defun compute-unconnected-verticies (m o d s i)
  "Returns U, J
           U = a bitfield representing a subset of D which cannot be reached from O,
           J = a bitfield of verticies that were traversed or ignored
      Where M = adjacency matrix, 
            O = a bitfield of origin verticies, 
            D = a bitfield of destination verticies, 
            S = the number of steps allowed (-1 = unlimited), 
            I = a bitfield of verticies that should be ignored (0 = none)"
  (cond
   ((zerop s) (values d i))
   (t
    (loop with valid-o = (logandc2 o i) ; remove ignored origin verticies
          with unconnected-d = (logandc2 d o) ; the remaining unconnected destinations
          for cur-o = (pop-bit valid-o) ; get next origin
          until (or (zerop unconnected-d) ; until either no more unconnected destinations
                    (zerop cur-o))        ;              or no more origins
          do (multiple-value-setq (unconnected-d i)
                 (compute-unconnected-verticies
                  m
                  (svref m (bit-position cur-o))  ;
                  unconnected-d
                  (1- s)
                  (logior i cur-o))) ; include current vertex in ignored verticies
          finally (return (values unconnected-d i))))))


;;; VERTEX-PATHS
;;;
(defun vertex-paths (g v1 v2 &key (max-steps -1) (ignore nil))
  "Returns NIL or a list of paths.  Each path is a list of vertexes beginning with V1 and ending with V2, representing transitions to get from V1 to V2"
  (graph-matrix-vertex-paths (matrix-of g) v1 v2 
                             max-steps (make-bits ignore)
                             (svref g v1)))
  
(defun graph-matrix-vertex-paths (g v1 v2 max-steps ignore neighbors)
  (unless (zerop max-steps)
    (cond
     ((logbitp v2 neighbors) ;; if v2 is a neighbor of v1, we're done
      (list (list v1 v2)))

     (t
      (loop with remaining-neighbors = (logandc2 neighbors ignore)
            for low-bit = (pop-bit remaining-neighbors) ; (log-lowbit remaining-neighbors)
            until (zerop low-bit)
            append (mapcar (lambda (path)
                             (list* v1 path))
                           (graph-matrix-vertex-paths
                            g
                            (bit-position low-bit)  ; try the next neighboring vertex
                            v2              ; still trying to reach v2
                            (1- max-steps)  ; one less step
                            (make-bits v1 ignore) ; also ignore v1
                            (svref g (bit-position low-bit))))))))) ; get the neighbors of the neighbor


;;;
;;; DIRECTED/UNDIRECTED CONVERSION:
;;;  
(defun coerce-to-undirected-graph (g)
  "Converts a graph to undirected form, by adding reverse edges"
  (cond
   ((directedp g) (add-reverse-edges g))
   (t             g)))


(defun add-reverse-edges (g)
  "Destructively modifies a graph to convert it to its undirected form (the conversion cannot be undone since information is lost)"
  (etypecase g
    ((not (satisfies directedp)) g)
    (vector
     (map-into g (lambda (row trow) (logior row trow)) g (transpose-graph-matrix g)))
    (graph
     (setf (graph-directedp g) nil
           (graph-matrix g) (add-reverse-edges (graph-matrix g)))
     g)))

;;
;; delete-reverse-edges:
;; this is a silly function - deletes arbitrary edges to make a directed graph ... get rid of it??
;;
(defun delete-reverse-edges (g) 
  "Destructively removes the reverse edges of an undirected graph"
  (loop with matrix = (matrix-of g)
        with len = (array-dimension matrix 0)
        for clear-bits = 0 then (logior clear-bits (make-bit i))
        for i from 0 below len
        do (setf (svref matrix i) (logandc1 clear-bits (svref matrix i)))
        finally (setf (graph-directedp g) t)
                (return g)))

;;;
;;; GRAPH-EDGES
;;;
(defun graph-edges (g)
  (graph-matrix-edges (matrix-of g) :directed (directedp g)))



;;;
;;; adding/delete operations
;;;

;;; ADD/DELETE-EDGES
(defun graph-add-edges (g edge-specs)
  "Modifies the graph by adding edges given in edgespecs"
  (graph-matrix-add-edges (matrix-of g) edge-specs (directedp g))
  g)

(defun graph-delete-edges (g edge-specs)
  "Modifies the graph by deleting edges given in edgespecs"
  (graph-matrix-delete-edges (matrix-of g) edge-specs (directedp g))
  g)

    
;;; ADD/DELETE-VERTICIES
(defgeneric graph-add-verticies (g n)
  (:documentation "Extends the graph by n verticies, which are numbered from 1+ the current highest vertex index")
  (:method ((g graph) (n integer))
   (graph-extend-verticies g n)))

(defun graph-extend-verticies (g n)
  (etypecase g
    (vector (map-into (make-array (+ (graph-vertex-count g) n) :initial-element 0)
                             #'identity g))
    (graph         (setf (graph-matrix g) (graph-extend-verticies (graph-matrix g) n))
                   g)))

(defgeneric graph-delete-verticies (g vlist &key not)
  (:documentation "Deletes the verticies")
  (:method :around (g (vlist null) &key not)
   (if not (call-next-method) g))
  (:method ((g graph) vlist &key not)
   (setf (graph-matrix g) (graph-matrix-remove-verticies (graph-matrix g) vlist :not not))
   g))

(defun graph-delete-verticies-if (g pred &key not)
  "Deletes all verticies of graph G for which pred is true.
   PRED is a function of 1 argument (the vertex index),
   which returns T if vertex should be deleted.  
   If NOT parameter is non-NIL, then complement of PRED is used."
  (graph-delete-verticies
   g
   (loop with test = (if not (complement pred) pred)
         for i from 0 below (graph-vertex-count g)
         when (funcall test i)
         collect i)))


;;;
;;; ADD-GRAPH
;;;      
(defun graph-add-graph (g1 g2 &optional g1-g2-edges g2-g1-edges)
  "Adds the verticies and edges of g2 to g1, creating new edges between
   the two sets of verticies as specified in g1-g2-edges and g2-g1-edges.
   If G1 is an undirected graph, edges are duplicated automatically.
   E.g., if g1 is undirected, then the g2-g1-edges parameter 
         in the following is redundant:
      (graph-add-graph g1 g2 :g1-g2-edges '((0 1)) 
                             :g2-g1-edges '((1 0)))"
  (let ((g1len (graph-vertex-count g1))
        (newg (graph-add-verticies g1 g2)))
    (loop with newgm = (matrix-of newg)
          with g2m = (matrix-of g2)
          for i from g1len below (graph-vertex-count newg)
          do (setf (svref newgm i)
                   (ash (svref g2m (- i g1len)) g1len)))
    (when (or g1-g2-edges g2-g1-edges)
      (graph-add-edges g1
                       (append (offset-to-specs g1-g2-edges g1len) 
                               (offset-from-specs g2-g1-edges g1len))))
    g1))


;; HELPER FNS FOR GRAPH-ADD-GRAPH
(defun offset-from-specs (edge-specs n &optional (max-tospec (1- n)))
  "Helper function for graph-add-edge - modifies edge-specs by adding N to the 'from' indicies"
  (mapcar (lambda (espec) (list* (+ (first espec) n)
                                 (mapc (lambda (tospec)
                                         (unless (<= tospec max-tospec)
                                           (error "~S is out of range." tospec)))
                                  (rest espec))))
          edge-specs))

(defun offset-to-specs (edge-specs n &optional (max-fromspec (1- n)))
  "Helper function for graph-add-edge - modifies edge-specs by adding N to the 'to' indicies"
  (when edge-specs
    (let ((from (first edge-specs)))
      (unless (and max-fromspec (<= from max-fromspec))
        (error "~S is out of range." from))
      (mapcar (lambda (espec) (list* from
                                     (mapcar (lambda (to)
                                               (+ to n))
                                             (rest espec))))
              edge-specs))))
;;;
;;; MERGING
;;;
(defun merge-verticies (g vremaps)
  "Merges verticies in graph G.  
   VREMAPS is a list of lists each of the form (Vnew Vold1 Vold2...) 
      where Vnew is the vertex to which edges will be copied and
            Voldx are the verticies to be copied from.  
            The Vold verticies will be deleted.
      when Vnew is NIL, the verticies in the rest of the list are simply deleted."
  (loop for (v1 . vrest) in vremaps
        when v1 ; (NIL ...) remaps are to be deleted only
        do (graph-matrix-copy-vertex-inputs (graph-matrix g) v1 vrest)
           (graph-matrix-copy-vertex-outputs (graph-matrix g) v1 vrest)
        finally (graph-delete-verticies g (apply #'append (mapcar #'rest vremaps)))))

(defun merge-graphs (graphs &key edges remap (directedp nil directedp-provided))
  "Combines 2 or more graphs.  
   EDGES is a list of lists of the form (Vfrom Vto0 Vto1 ...) specifying verticies to be connected.
   REMAP is a list of lists of the form (Vnew Vold0 Vold1 ...), specifying
         verticies to be remapped.  All edges into/out of Voldx will be copied over to Vnew.
   
         where Vn is a cons pair (G . I), 
         where G is the 0-based index of a graph in graphs
               I is the 0-based index of a vertex in that graph."         
  (when graphs
    (let* ((directedp (if directedp-provided directedp (graph-directedp (first graphs))))
           (gstarts (loop with counts = (make-array (length graphs))
                          for i = 0 then (1+ i)
                          for count = 0 then (+ count (graph-vertex-count g))
                          for g in graphs
                          do (setf (svref counts i) count)
                          finally (return counts)))
           (merged  (reduce #'graph-add-graph (list* (copy-graph (first graphs)) (rest graphs)))))
      (labels ((vertex (gi)
                 (if gi (+ (svref gstarts (car gi)) (cdr gi))))
               (merged-verticies (gilist) 
                 (mapcar #'vertex gilist)))
        (setf (graph-directedp merged) directedp)

        (when edges
          (graph-add-edges merged (mapcar #'merged-verticies edges)))

        (when remap
          (merge-verticies merged
                           (mapcar #'merged-verticies remap)))
        merged))))


