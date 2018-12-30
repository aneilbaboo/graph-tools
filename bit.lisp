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


;;; $Id: bit.lisp,v 1.6 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $
;;;
;;; Extends Lisp support for bit twiddling
;;;

(in-package :graph-tools)

;; eval when compiling because of inline expansions:
(eval-when (:compile-toplevel :load-toplevel :execute)

;; compiler macro performs no checks
(define-compiler-macro bit-position (n) `(1- (integer-length ,n)))

(declaim (inline make-bit))
(defun make-bit (index)
  (ash 1 index))

(declaim (inline log-lowbit))
(defun log-lowbit (n)
  "Returns an integer where only the lowest bit of n is set. Note: (log-lowbit 0) => 0."
  (declare (integer n) (optimize speed))
  (- n (logand n (1- n))))

(define-compiler-macro log-lowbit (n)
  (if (atom n)
      `(- ,n (logand ,n (1- ,n)))
    (let ((nvar '#:n))
      `(let ((,nvar ,n)) (- ,nvar (logand ,nvar (1- ,nvar)))))))

(defun log-lowbit-pos (n)
  "Returns the 0-based position of the lowest set bit 
           or -1 if no set bits exist."
  (declare (integer n) (optimize speed))
  (1- (integer-length (log-lowbit n))))
(declaim (inline log-highbit-pos))
(defun log-highbit-pos (n)
  "Returns -1 if no high bit is available"
  (declare (integer n) (optimize speed))
  (1- (integer-length n)))

(declaim (inline log-highbit))
(defun log-highbit (n)
  "Returns an integer where only the highest bit of n is set. Note: (log-highbit 0) => 0."
  (declare (integer n) (optimize speed))
  (ash 1 (log-highbit-pos n)))

(declaim (inline bit-position))
(defun bit-position (n)
  "The same as log-highbit-pos, but checks that the argument has only 1 bit set."
  (let ((hibit-pos (log-highbit-pos n)))
    (if (zerop (logxor (make-bit hibit-pos) n)) hibit-pos
      (error "Invalid input to ~S (#B~B). Expecting an integer with no more than 1 set bit." 
             'bit-position n))))

(declaim (inline bitmask))
(defun bitmask (length)
  "Efficiently generates a bit field composed of N set bits"
  (declare (integer length))
  (1- (ash 1 length)))


(define-compiler-macro log-lowbit-pos (n)
  `(1- (integer-length (log-lowbit ,n))))

(defmacro pop-bit (bitfield)
  "Returns the integer representing the lowest bit of bitfield, setting that bit to 0 in bitfield.
   Returns 0 when no more bits are available."
  (let ((i (gensym "I")))
    `(let ((,i (log-lowbit ,bitfield)))
       (setf ,bitfield (logandc1 ,i ,bitfield))
       ,i)))

(defun low-byte (n)
  "Returns a bytespec representing the lowest group of contiguous set bits or NIL"
  (let ((lowpos (log-lowbit-pos n)))
    (unless (minusp lowpos)
      (byte (log-lowbit-pos (lognot (ash n (- lowpos))))
            lowpos))))
                
(defun delete-bits (n b)
  "Deletes the bits represented by B, shifting higher order bits down; B a BYTESPEC or an integer representing a bitfield (the set bits will be deleted)."
  #-:clisp
  (etypecase b
    (#.(type-of (byte 1 1)) (logior (mask-field (byte (byte-position b) 0) n) ; get the lower bits
                                    (logandc2 (ash n (- (byte-size b)))
                                              (bitmask (byte-position b)))))
    (integer (loop for low-byte = (low-byte b)
                   while low-byte
                   do (setf b (delete-bits b low-byte)
                            n (delete-bits n low-byte))
                   finally (return n))))

  #+:clisp ;; temporary CLISP workaround -- bug should be fixed in version 2.44
  (cond
    ((eq (type-of b) 'byte)  (logior (mask-field (byte (byte-position b) 0) n) ; get the lower bits
                                    (logandc2 (ash n (- (byte-size b)))
                                              (bitmask (byte-position b)))))
    ((typep b 'integer)      (loop for low-byte = (low-byte b)
                                   while low-byte
                                   do (setf b (delete-bits b low-byte)
                                            n (delete-bits n low-byte))
                                   finally (return n)))))

(eval-when (:compile-toplevel :execute)
  (defmacro loop-bits ((bits fn &optional fn-result return) head &body body)
    (let ((next-bit '#:next-bit)
          (fn-result (or fn-result '#:result)))
      `(loop ,@head
             with ,fn-result
             for ,next-bit = (log-lowbit ,bits) then (log-lowbit ,bits)          
             until (zerop ,next-bit)
             do (setf ,bits (logandc2 ,bits ,next-bit)
                      ,fn-result (funcall ,fn ,next-bit))
                ,@body
             ,@(if return `(finally (return ,return)))))))

(defun find-bit (fn bits)
  "Where bits is a bitfield and fn is a predicate of one argument (bit).
   Returns: the first bit for which FN returns non-NIL"
  (loop-bits (bits (lambda (bit) (if (funcall fn bit) bit)) result result)
      ()
    (if result (loop-finish))))

(defun map-bits (result-type fn bits)
  "Collects the results of fn applied to each bit in bits"
  (ecase result-type 
    ((nil)
     (loop-bits (bits fn) ()))

    ((or simple-vector array)
     (loop-bits (bits fn result arr)
         (with arr = (make-array :adjustable t :fill-pointer t)
          with bitlen = (logcount bits))
       (vector-push-extend result arr bitlen)))

    ((list cons)
     (loop-bits (bits fn result)
         ()
       collect result))))
                
                
(defun map-bits-into (seq fn bits)
  "Takes a bitfield (an integer) and a function of one argument (bit).
   SEQ: T, NIL sequence to be altered (must be at least (LOGCOUNT BITS) length),
        or NIL
   BITS: an integer (bitfield)
   FN: function of one argument which returns values.
   RETURNS: SEQ"
  (etypecase seq
    (cons          (loop-bits (bits fn result seq)
                       (for rest on seq) 
                     (setf (car rest) result)))
    (simple-vector (loop-bits (bits fn result seq)
                       (for i = 0 then (1+ i))
                     (setf (svref seq i) result)))
    (array         (loop-bits (bits fn result seq)
                       (for i = 0 then (1+ i)) 
                     (setf (aref seq i) result)))))


(defun make-bits (indexes &optional (field 0))
  "Given a sequence of bit indexes, returns a number with those bits set.  Compiler macro is provided for efficiency."
  (cond
   ((numberp indexes) (if (zerop field) (ash 1 indexes)
                        (logior field (ash 1 indexes))))
   (t                 (reduce (lambda (x y) (logior x (ash 1 y))) 
                              (list* field indexes)))))

(defmacro setbitsf (x indexes)
  "Sets bits in the location indicated by X.  Indexes may be one or a sequence of bit-indexes"
  `(setf ,x (logior ,x (make-bits ,indexes))))

(defmacro clearbitsf (x indexes)
  "Clears bits in the location indicated by X.  Indexes may be one or a sequence of bit-indexes"
  `(setf ,x (logandc2 ,x (make-bits ,indexes))))

(defmacro setbitf (x index)
  `(setf ,x (logior ,x (make-bit ,index))))

(defmacro clearbitf (x index)
  `(setf ,x (logandc2 ,x (make-bit ,index))))

(defun quotedp (x) (and (consp x) (eq (first x) 'quote)))

(define-compiler-macro make-bits (&whole form indexes &optional (field 0))
  (cond
   ((and (numberp indexes) 
         (numberp field))    (logior field (make-bit indexes)))
   ((and (quotedp indexes)
         (consp (second indexes))
         (every #'integerp (second indexes))
         (numberp field))    (reduce (lambda (x y) (logior x (ash 1 y)))
                                     (list* field indexes)))
   (t                         form)))

(defun delete-indicies (sequence indicies &optional not)
  "Returns a sequence in which the elements indicated by indicies
   are removed, (possibly modifying the sequence).  
   NOT is a boolean; if set, indicies will be kept."
  ;; a tiny bit hacky since it assumes that delete will call
  ;; the predicate on each element in order exactly once
  (let ((bfield (make-bits indicies))
        (count  0))
    (flet ((test (o) (declare (ignorable o))
             (let ((ret (logbitp count bfield)))
               (incf count)
               ret)))
      (delete-if (if not (complement #'test) #'test)
               sequence))))
)