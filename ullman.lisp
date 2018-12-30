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


;;; $Id: ullman.lisp,v 1.15 2009/05/06 07:25:21 amallavarapu Exp $
;;; $Name:  $

;;; File: ullman.lisp
;;; Description: Implementation of the Ullman subgraph isomorphism algorithm
;;   graphs are bitmatricies (vectors of integers treated as bitfields)
;;
;; PUBLIC FUNCTIONS:
;l   find-subgraph-isomorphism-maps - give graph S (subgraph) and graph G (graph),
;;                                    returns a list of bit vectors.  Each bit vector
;;                                    is a map, the element indexes corresponding to
;;                                    verticies of S, and the elements being 1-bit integers,
;;                                    where the bit-position represents the vertex in G which
;;                                    S is mapped to.
;;   find-subgraph-isomorphisms     - wrapper around find-subgraph-isomorphism-maps, simply
;;                                    returns lists of isomorphisms.  Each isomorphism is a list
;;                                    of binary lists, each of which represent a mapping of an 
;;                                    S vertex to a G vertex.
;;   
;; NOTES:
;;   "MAP" - a bit matrix (actually represented using numbers) which maps subgraph (s) verticies (rows)
;;           to graph (g) verticies (columns).   
;;
;;   Eg., given   S= 0 - 1 - 3     G= 3 - 2 - 1 - 0
;;                    \ /              \ /     \
;;                     2                4       5
;;
;;        FIND-SUBGRAPH-ISOMORPHISM-MAPS would return two maps:
;;           #(#B001000  ; 0->3    and  #(#B010000  ; 0->4
;;             #B000100  ; 1->2           #B000100  ; 1->2
;;             #B010000  ; 2->4           #B001000  ; 2->3
;;             #B000010) ; 3->1           #B000010) ; 3->1
;; 
;;        The "initial map", returned by COMPUTE-INITIAL-ISOMORPHISM-MAP contains an initial
;;        guess of what GVerticies could be matched to SVerticies, currently determined by seeing
;;        if the G outdegree > S outdegree
;;         
;;          S       0 1 2 3     G       0 1 2 3 4 5
;;          degree  2 3 2 1     degree  1 3 3 2 2 1
;;          
;;        Therefore, the initial map is:
;;          #(#B011110
;;            #B000110  ; only two other G verticies are possible matches for SVertex 1
;;            #B011110
;;            #B111111) ; all G verticies are possible matches for SVertex 3
;;          
(in-package :graph-tools)

;;;
;;; PUBLIC FUNCTIONS
;;;
(defun find-subgraph-isomorphism-maps (s g &key base-map (continue-if t) vertex-test row-fn)
  "Returns a list of subgraph isomorphism maps, bit vectors mapping verticies from S to G"
  (let* ((m0          (compute-initial-isomorphism-map s g :vertex-test vertex-test :row-fn row-fn))
         (m0          (if base-map (map-into m0 #'logand m0 base-map) m0)))
    (when m0
      (ullman (matrix-of s) (matrix-of g) m0 continue-if))))

(defun find-subgraph-isomorphisms (s g &rest options &key (continue-if t) vertex-test row-fn &allow-other-keys)
  "Returns a list of mappings (isomorphisms) of S verticies to G verticies.  Each
mapping is a vector, the index is the SVERTEX, and the value at that position is
the GVERTEX to which SVERTEX has been mapped.  E.g., (#(3 5 8) #(1 3 4)) indicates
that S is isomorphic to G: S(0,1,2)->G(3,5,8) and S(0,1,2)->G(1,3,4).  LABEL-TEST provides
a way to override the default label-test procedure.  CONTINUE-IF is either NIL, T or 
a unary predicate which receives the most recently found graph as an argument.  It should
return NIL to terminate the search."
  (declare (ignorable continue-if row-fn vertex-test))
  (mapcar (lambda (map) (map 'vector #'bit-position map)) 
          (apply #'find-subgraph-isomorphism-maps s g options)))

;;;
;;; Recursive version of Ullman's backtracking algorithm.  
;;; Here, the Lisp stack is used to keep track of the position in the search tree.
;;;
(defun ullman (s g m0 continue-if)
  "Input S G are graph-matricies, returns a list of isomorphism maps, which are
      bit-matricies mapping verticies in S to verticies in G.
      Mappings have one bit set per entry.
   TEST-VERTICIES-IF - a predicate of two arguments, (SVERTEX GVERTEX) returning a boolean
                  indicate whether the algorithm should attempt to detect isomorphisms
                  involving a map of indicated indexes into S and G. 
                  The default predicate is ensures that ndegree of svertex is always <= that
                  of gvertex.
   CONTINUE-IF - a keyword or predicate
                T - causes all isomorphism maps to be returned.
                NIL - causes only the first found isomorphism to be returned
                A user supplied predicate takes an isomorphism map as an argument and returns
                  CONTINUEP, COLLECTP
                  The search will continue only if CONTINUEP is non-NIL
                  The map is included in the results only if COLLECTP is non-NIL."   
  (declare (optimize speed))
  (let ((ssize        (array-dimension s 0))
        (continue-if  (etypecase continue-if
                        (function  continue-if)
                        (integer   (let ((counter continue-if))
                                     (lambda (m) (declare (ignorable m))
                                       (if (minusp (decf counter)) (values nil nil)
                                         (values t t)))))
                        (null      (lambda (m) (declare (ignorable m)) (values NIL T)))
                        (t         (lambda (m) (declare (ignorable m)) (values T T))))))
     (declare (type (function (simple-vector) (values boolean boolean))
                    continue-if))
     (labels ((validate-map (m d k) 
               ;; we are asking whether Sd -M-> Gk 
               ;; where m is the current mapping,
               ;;       d is a index (depth) indicating the svertex to be mapped
               ;;       k a bit-encoded position, ie, the column of M which is a vertex of G
               ;; this means that for each adjacent vertex, Sdx,
               ;; there exists at least one mapping Sdx -M-> Gky
               (let ((srow (svref s d)) ; adjacent s verticies at the svertex to be tested
                     (grow (svref g (log-lowbit-pos k)))) ; adjacent g verticies
                 (if (zerop srow) m
                   (loop
                    for svertex = (log-lowbit-pos srow) ; next available neighbor of Sd

                    for new-mrow = (setf (svref m svertex)                ; clear map bits if
                                         (logand grow (svref m svertex))) ; neighbors don't match

                    ;; unless this adjacent svertex (shibit) has at least
                    ;; one available isomorphism to an adjacent point
                    if (zerop new-mrow)

                    do (return-from validate-map nil)

                    else do (setf srow (logandc1 (make-bit svertex) srow)) ; clear the hibit and continue

                    until (zerop srow)

                    finally (return m)))))

             (select-mapping (m k d f)
               ;; returns a copy of m where the kth gvertex is selected
               ;; for the dth svertex, and adds k to f
               ;; m is the map
               ;; k is the bit (gvertex) to select in m at d
               ;; d is an integer, the "depth" in m, which is the svertex
               ;; f is a bitfield of all bits selected so far
               (let ((newm (copy-seq m)))
                 (setf (svref newm d) k) 
                 (loop for svertex = (1+ d) then (1+ svertex)
                       until (= svertex ssize)
                       do (setf (svref newm svertex)
                                (logandc1 f (svref newm svertex))))
                 (validate-map newm d k)))

             (internal-search (m d f solns)
               ;; m is the map, an array of bitfields represent s->g isomorphisms
               ;; d is the depth of the search (ie., the current svertex)
               ;; k is a bit representing the current gvertex 
               ;; f is a bit mask representing all gverticies which have been assigned at depth d
               ;;      at the current call depth
               ;; this fn returns nil (failure) or a list isomorphisms
               (let ((mrow (svref m d)))
                 (unless (zerop mrow)

                   (loop for k = (log-lowbit mrow) ; last k

                         do (setf mrow (logandc1 k mrow)) ; invalidate k bit
                         
                         (let* ((newf (logior k f))
                                (newm (select-mapping m k d newf))
                                (newd (1+ d)))
                           (when newm 
                             (cond
                              ((= newd ssize) ; last level reached
                               (multiple-value-bind (continuep collectp)
                                   (funcall continue-if newm)
                                 (when collectp (push newm solns))
                                 (unless continuep (throw 'result-found solns))))
                              (t (setf solns 
                                       (internal-search newm newd newf solns))))))
                         until (zerop mrow)))
                 solns)))
                                    
    (catch 'result-found 
      (internal-search m0 0 0 nil)))))

;;;
;;; Isomorphism map computation:
;;;
(defun compute-initial-isomorphism-map (s g &key vertex-test row-fn)
  "Constructs a isomorphism search map.

  S: subgraph representing a pattern to find in G
  G: a graph of the same type as S (ie., a graph-matrix or a subtype of GRAPH)

  The output is a logical bitmatrix of length s-verticies by 'width' g-verticies.
  VERTEX-TEST and ROW-FN are alternate ways to customize the isomorphism map computation.
  If neither are provided, default tests are computed.

  VERTEX-TEST is a FN which take arguments (s i g j), and return T if a mapping from 
  Si to Gj is possible, or NIL.  If VERTEX-TEST is NIL, a default test is provided which 
  is T only when the S vertex contains at least as many outgoing edges as the G vertex.

  ROW-FN is a FN which take arguments (s i g) and returns an integer bitfield representing
  all potential mappings from Si to verticies of G.  The bit in position j is 1 if a mapping 
  from Si to Gj is possible.

  In principle, all that is needed is one or more vertex-tests.  ROW-FN are
  useful when it is possible cache and reuse the computation of an Svertex mapping.
  For example, if the same label label is encountered, a cached copy of the mappings
  to the G verticies can be used." 
  
  (loop with vertex-test = (or vertex-test (make-outdegree-vertex-test s g))
        with row-fn = (or row-fn (make-isomap-default-row-fn s g))
        with m0 = (make-array (graph-vertex-count s) :element-type 'integer)
        for svertex from 0 below (graph-vertex-count s)
        for mrow = (funcall row-fn s svertex g)
        if (zerop mrow)
        do (return nil)
        else
        do (loop for gvertex from 0 below (graph-vertex-count g)
                 unless (funcall vertex-test s svertex g gvertex)
                 ;; remove this vertex from the map
                 do (clearbitf mrow gvertex)
                 finally (cond
                          ((zerop mrow) (return-from compute-initial-isomorphism-map nil))
                          (t            (setf (svref m0 svertex) mrow))))
        finally (return m0)))

(defun make-outdegree-vertex-test (s g)
  (let* ((g             (matrix-of g))
         (s             (matrix-of s))
         (gcount        (map 'vector #'logcount g))
         (scount        (map 'vector #'logcount s)))
    (declare (type simple-vector s g gcount scount m0))
    (flet ((outdegree-test (s svertex g gvertex)
             (declare (ignore s g))
             (<= (svref scount svertex) 
                 (svref gcount gvertex))))
      #'outdegree-test)))

(defun make-isomap-unlabelled-row-fn (s g)
  (declare (ignore s))
  (constantly (bitmask (graph-vertex-count g))))

(defun make-isomap-labelled-row-fn (s g &optional test)
  (let* ((cache   (make-hash-table :test #'equal))
         (slabels (graph-labels g))
         (glabels (graph-labels s))
         (test    (or test (lambda (s svertex glabel)
                             (declare (ignore s))
                             (equal (svref slabels svertex) glabel)))))
    (declare (vector slabels glabels))
    (flet ((label-row-map (s svertex g)
             (declare (ignore g))
             (declare (integer svertex))
             (let ((label  (svref slabels svertex)))
               (or (gethash label cache)
                   (setf (gethash label cache)
                         (loop with row-bits = 0
                               for i from 0 below (array-dimension glabels 0)
                               when (funcall test s svertex (svref glabels i))
                               do (setbitsf row-bits i)
                               finally (return row-bits)))))))
      #'label-row-map)))

(defun make-isomap-default-row-fn (s g)
 (typecase s 
   (labelled-graph (make-isomap-labelled-row-fn s g))
   (t              (make-isomap-unlabelled-row-fn s g))))
    

             
           
    
                           
;;;; (defgeneric compute-initial-isomorphism-map (s g &key label-test m0  &allow-other-keys)
;;;;   (:documentation  "Constructs a isomorphism search map. Only vertex mappings
;;;;   where the graph vertex contains at least as many outgoing edges as
;;;;   the search graph are considered.
;;;;   S: the subgraph-matrix argument (S) to find-subgraph-matrix-isomorphisms
;;;;   G: the graph-matrix argument (G) to find-subgraph-matrix-isomorphisms
;;;;   The output is a logical bitmatrix of length s-verticies by 'width' g-verticies.")
;;;;   (:method (s g &key (m0 (%make-isomorphism-map s g)) &allow-other-keys)
;;;;    (declare (ignore label-test))
;;;;    ;; prunes all mappings where the S vertex has a larger outdegree than the G vertex
;;;;    (let* ((g             (matrix-of g))
;;;;           (s             (matrix-of s))
;;;;           (gvertexmax    (1- (array-dimension g 0)))
;;;;           (s-len         (array-dimension s 0))
;;;;           (gcount        (map 'vector #'logcount g))
;;;;           (scount        (map 'vector #'logcount s)))
;;;;     (declare (type simple-vector s g gcount scount))
;;;;     (loop for svertex from 0 below s-len
;;;;           do (loop with bits = (svref m0 svertex)
;;;;                    for gvertex from 0 to gvertexmax
;;;;                    ;; when # s neighbors > # g neighbors, this s vertex can never map to this g vertex
;;;;                    when (> (svref scount svertex) 
;;;;                            (svref gcount gvertex))
;;;;                    ;; remove this vertex from the map
;;;;                    do (clearbitsf bits gvertex)
;;;;                    finally (if (zerop bits) (return-from compute-initial-isomorphism-map nil)
;;;;                              (setf (svref m0 svertex) bits))))
;;;;     m0))
;;;;   (:method ((s labelled-graph) (g labelled-graph) &key (m0 nil m0-p) label-test &allow-other-keys)
;;;;    (declare (ignore options))
;;;;    ;; computes an initial m0 map by pruning all the non-matching labels, then passes this map to 
;;;;    ;; the unlabelled method above
;;;;    (let ((label-test (or label-test (graph-label-test-predicate s)))
;;;;          (glen (graph-vertex-count g))
;;;;          (m0   (or m0 (%make-isomorphism-map s g nil))))
;;;;      (flet ((compute-mrow (svertex)
;;;;               (loop with bits = 0
;;;;                     for gvertex from 0 below glen
;;;;                     when (funcall label-test
;;;;                                   svertex 
;;;;                                   (graph-vertex-label g gvertex))
;;;;                     do (setbitsf bits gvertex)
;;;;                     finally (return bits))))
;;;;        (loop 
;;;;         with cache = (make-hash-table :test #'equal)
;;;;         with slen = (graph-vertex-count s)
;;;;         for svertex from 0 below slen
;;;;         for slabel = (graph-vertex-label s svertex)
;;;;         for label-row = (or (gethash slabel cache)
;;;;                             (setf (gethash slabel cache)
;;;;                                   (compute-mrow svertex)))
;;;;         for combined-label-row = (if m0-p (logand label-row (svref m0 svertex)) label-row)
;;;;         ;; LOGICAL AND THE M0 ROW with the LABEL ROW
;;;;         if (zerop combined-label-row) return nil
;;;;         else do (setf (svref m0 svertex) combined-label-row)
;;;;         finally (return (call-next-method s g :m0 m0)))))))