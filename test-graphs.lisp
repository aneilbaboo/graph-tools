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


;;; $Id: test-graphs.lisp,v 1.3 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $

;;; File: test-graphs.lisp
;;; Description: a few test-graphs
;;;              
(in-package graph-tools)   

;;;
;;; a few test graphs:
;;;

;; graph1:   0-1
;;            \
;;             2
(defconstant +graph1+ (make-graph '((0 1) (0 2))))

;; graph2:   1
;;          / \
;;         0   2
;;          \ /
;;           3
(defconstant +graph2+ (make-graph '((0 1) (0 3) (1 2) (3 2))))


;; graph3:     1   5
;;            / \ /
;;           0   3
;;            \ / \
;;             2   4
(defconstant +graph3+ (make-graph '((0 1) (0 2) (1 3) (2 3) (3 5) (3 4))))

   
;; house-graph:  1  
;;              / \
;;             0 - 2
;;             |   |
;;             3 - 4
(defconstant +house-graph+ (make-graph '((0 1) (0 3) (0 2) (1 2) (2 4) (3 4))))

;; house-graph-plus1:
;;                4  
;;               / \
;;              0 - 2 
;;              |   |
;;              3 - 1-5
(defconstant +house-graph-plus1+ (make-graph '((0 3 2 4)
                                               (2 4 0 1)
                                               (1 3 5))))

;; house-graph-plus2:
;;                1  
;;               / \
;;              0 - 2 - 5
;;              |   | /
;;              3 - 4
(defconstant +house-graph-plus2+ (make-graph '((0 1) (0 3) (0 2) (1 2)
                                               (2 4) (3 4) (2 5) (4 5))))

;; double-house-graph:  
;;               1-5-6
;;              / \| |
;;             0 - 2-7
;;             |   |
;;             3 - 4
(defconstant +double-house-graph+ (make-graph '((0 1) (0 3) (0 2) (1 2) (2 4) (3 4)
                                                           (1 5) (5 6) (1 2) (2 7) (5 2) (6 7))))


;; double-house-graph2:
;;               6-2-3
;;              / \| |
;;             5 - 4-0
;;             |   |
;;             7 - 1
(defconstant +double-house-graph2+ (make-graph '((6 2) (2 3) (5 4) (4 0) (7 1)
                                                           (6 5) (6 4) (2 4) (3 0)
                                                           (5 7) (4 1))))


;; double-house-graph2-plus1:
;;               6-2-3-8
;;              / \| |
;;             5 - 4-0
;;             |   |
;;             7 - 1 
(defconstant +double-house-graph2-plus1+ 
  (make-graph '((6 2) (2 3) (5 4) (4 0) (7 1)
                           (6 5) (6 4) (2 4) (3 0)
                           (5 7) (4 1) (3 8))))

;; double-house-graph2-plus2:
;;               6-2-3-8
;;              / \| |
;;             5 - 4-0
;;             |   |
;;             7 - 1-9
(defconstant +double-house-graph2-plus2+ 
  (make-graph '((6 2) (2 3) (5 4) (4 0) (7 1)
                           (6 5) (6 4) (2 4) (3 0)
                           (5 7) (4 1) (3 8) (1 9))))


;; big complex graph
;;    11-12-4----5
;;     |    |   / \
;;    10-9--7--3 - 2
;;            /|   |
;;           8 1 - 6       
(defconstant +bc-graph+ (make-graph '((4 5) (12 4) (11 12) 
                                                 (7 3) (3 2) (10 9) (9 7) 
                                                 (1 6)
                                                 (11 10) (4 7) (5 3) (5 2)
                                                 (3 8) (3 1) (2 6))))

(defconstant +valid-isomorphism-1->3+ (make-array '(3 6) :element-type 'bit
                                            :initial-contents '((0 0 0 1 0 0)
                                                                (0 0 0 0 1 0)
                                                                (0 0 0 0 0 1))))

(defconstant +invalid-isomorphism-1->3+ (make-array '(3 6) :element-type 'bit
                                               :initial-contents '((1 0 0 0 0 0)
                                                                   (0 0 0 0 1 0)
                                                                   (0 0 0 0 0 1))))


