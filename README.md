# graph-tools

Tools for efficiently detecting subgraph isomorphisms and graph identity using Ullman and NAUTY algorithms in Common Lisp
```lisp
(require 'graph-tools)
(in-package :graph-tools)
;;
;;

(let* ((graph  (make-graph '((0 1) (0 2) (1 2) (0 3) (2 4))))
        ;; graph:
        ;;           1
        ;;          / \
        ;;         0 - 2
        ;;          \   \
        ;;           3   4
       (subgraph (make-graph '((0 1) (0 2) (1 2))))
        ;; subgraph:
        ;;           1
        ;;          / \
        ;;         0 - 2
       (isomorphisms (find-subgraph-isomorphisms +graph2+ +graph3+)))
  (print isomorphisms)
)
;; 6 isomorphisms:
#(
  #(0 1 2)
  #(0 2 1)
  #(1 2 0)
  #(1 0 2)
  #(2 0 1)
  #(2 1 0)
)

(let* ((g (make-graph '((0 1) (0 2) (1 2) (0 3) (2 4))))
  (destructuring-bind (canonical-g reordering) 
    (canonical-graph g)
    (print canonical-g) ;; a graph with identical structure to g
    (print reordering) ;; array of length 5 mapping verticies in canonical-g to g
  )
) 
```

### Graph functions

```lisp
(make-graph '((0 1) (0 2) (1 2))) ;; an undirected trianglular graph
(make-graph '((0 1) (1 2) (2 0)) t) ;; directed triangular graph

(directedp g) ;; is graph directed or undirected?

(graph-directed-edge-p g i1 i1) ;; t if an edge exists from i1 to i2
(setf (graph-direvted-edge-p g i1 i1) t) ;; connects i1 to i2

(graph-vertex-count g) ;; returns # of verticies

(vertex-paths g v1 v2 &key (max-steps -1) (ignore nil)) 
  ;; returns a list of paths, where each path is a list of verticies starting at v1 and ending in v2


(coerce-to-undirected-graph g) ;; adds reverse edges to a directed graph, making it undirected


(graph-add-edges g edge-specs)
(graph-delete-edges g edge-specs) 

(graph-extend-verticies g n) ;; adds n unconnected verticies to the graph
(graph-delete-verticies g vlist &key not)
(graph-delete-verticies-if g pred &key not)

(graph-add-graph g1 g2 &optional g1-g2-edges g2-g2-edges) 
  ;; adds two graphs and new edges between the two graphs
  ;; e.g., (graph-add-graph g1 g2 :g1-g2-edges '((0 1)) 
                                  :g2-g1-edges '((1 0)))
  ;;        creates an undirected edge between g1vertex 0 to g2vertex 1

(matrix-of g) ;; gets the underlying bit matrix representing the graph

(reorder-graph g #(2 3 0 1)) ;; returns a new graph with same structure
                            ;; but with verticies reordered

(transpose g) ;; destructively transpose graph g - reverses directed edges

(unconnected-subgraphs g &key (verticies t) output-connected-p)
    ;; Returns a list of subgraphs of g which are disconnected from each other.
    ;; G is a graph
    ;; OUTPUT-CONNECTED is a boolean which if set causes the function to consider
    ;; verticies to be connected when they share an output vertex.  This setting
    ;; only has meaning for directed graphs, since all edges in undirected graphs 
    ;; are bidirectional.  If OUTPUT-CONNECTED-P is NIL, shared output verticies do
    ;; not cause their inputs to be in a connected group; the s

(connected-vertex-groups g &key (verticies t) (max-steps -1) (ignore nil))
  ;; computes the sets of verticies which are connected (by output edges)


```




### Ullman subgraph isomorphism functions
Graphs are represented as bitmatricies (vectors of integers treated as bitfields)

#### find-subgraph-isomorphisms
```lisp
(find-subgraph-isomorphisms s g &key base-map continue-if vertex-test row-fn)
```
Returns a list of where each element represents a subgraph isomorphism.

Required arguments:

  `s` - subgraph to find
  `g` - graph to search

Optional arguments:

  `base-map` - allowed mappings from subgraph `s` verticies to graph `g` verticies. This parameter can be used to only allow matches between particular verticies. This is a vector where each index represents the corresponding vertex in the subgraph `s`, and the value is a bitmatrix where each set bit represents an allowed isomorphism to the graph `g`.  An entry containing all bits set means that all mappings are possible; An entry where all bits are 0 means that no mappings are possible between that subgraph vertex and any graph vertex.
  
  `continue-if` - lambda which which takes an isomorphism as an argument and returns two booleans `(continuep collectp)`. If `collectp` is true, the isomorphism is added to the list of returned isomorphisms. If `continuep` is true, search continues for more isomorphisms.

  `vertex-test` - predicate used to limit which vertexes in `s` can match to vertexes in `g`. It takes arguments `(s svertex g gvertex)`, where s and g are the subgraph and graph and verticies being tests, and returns `NIL` if `svertex` cannot map to `gvertex`. 

  `row-fn` - is an alternative way of computing which mappings are possible, and takes arguments `(s svertex g)` and returns an integer bitfield representing the indicies of `g` to which `svertex` may be mapped.

  Note: if neither `vertex-test` nor `row-fn` are provided, a default vertex-test is used which only allows sverticies to map to gverticies with an equal or greater number of outgoing edges.


#### find-subgraph-isomorphism-maps

`(find-subgraph-isomorphism-maps s g &key base-map (continue-if t) vertex-test row-fn)`                         

Identical to find-subgraph-isomorphisms, but returns a list of bit-vectors instead of a list of integer vectors. Useful if you want to avoid the additional overhead of translating from bit-vectors to integer arrays.