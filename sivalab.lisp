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


;;; $Id: sivalab.lisp,v 1.3 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $

;;; File: sivalab.lisp
;;; Description: functions for reading/writing SIVA lab graph files
;;;              the SIVALab database is available at 
;;;              http://amalfi.dis.unina.it/graph/db/
;;;              Documentation may be found at:
;;;              http://amalfi.dis.unina.it/graph/db/doc/graphdbat.html
;;;              
(in-package :graph-tools)

;;;
;;; SIVALAB FORMAT
;;;
(defun read-binary-integer (stream bytes 
                                   &key (byte-order :little-endian) 
                                   signed)
  "Reads an integer BYTES bytes long from a binary stream (element-type 'unsigned-byte).
   Option keywords :BYTE-ORDER (either :little-endian or :big-endian)
   and SIGNED control the behavior of the reader."
  (let* ((int (ecase byte-order
                (:little-endian (loop for i from 0 to (1- bytes)
                                      for byte = (read-byte stream)
                                      sum (ash byte (* 8 i))))
                (:big-endian    (loop for i from (1- bytes) downto 0
                                      for byte = (read-byte stream)
                                      sum (ash byte (* 8 i)))))))
    (cond
     (signed
      (let ((hibit (ash #X80 (* 8 (1- bytes)))))
        (if (logtest int hibit) ; if hibit is set
            (- (- hibit (logxor hibit int))) ; return twos complement value
          int)))
     (t int))))

(defun write-binary-integer (stream int bytes
                                    &key 
                                    (byte-order :little-endian))
  "Writes an integer in binary format to the stream (should be element-type unsigned-byte)."
    (ecase byte-order
      (:little-endian (loop for i from 0 to (1- bytes)
                            do (write-byte (ldb (byte 8 (* 8 i)) int) stream)))
      (:big-endian    (loop for i from (1- bytes) downto 0
                            do (write-byte (ldb (byte 8 (* 8 i)) int) stream))))
    int)

(defun read-sivalab-format (file)
  "Reads graphs of the graph database produced by the sivaLab.  See http://amalfi.dis.unina.it"
  (with-open-file (stream file :direction :input :element-type 'unsigned-byte)
    (flet ((read-int () (read-binary-integer stream 2
                                             :byte-order :little-endian :signed nil)))    
      (funcall #'make-graph 
               (loop for node from 0 to (1- (read-int))
                     collect `(,node ,@(loop for i from 0 to (1- (read-int))
                                             collect (read-int))))))))
   