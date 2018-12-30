;; -*- mode:Lisp -*-
#-:asdf+ (error "ASDF+ is required.")

(defpackage #:graph-tools-system
  (:use #:cl #:asdf)
  (:export #:*graph-tools-root-directory*
           #:load-graph-tools))

(in-package #:graph-tools-system)

(defparameter *graph-tools-root-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defsystem :graph-tools
    :pathname           (make-pathname :name nil :type nil
                                       :defaults *load-truename*)
    ;:depends-on         (:uffi)
    :components ((:file "package")
                 (:file "bit" :depends-on ("package"))
                 (:file "graph" :depends-on ("package" "bit"))
                 (:file "graph-matrix" :depends-on ("bit" "graph"))
                 (:file "labelled-graph" :depends-on ("graph"))
                 (:file "arc-graph" :depends-on ("labelled-graph"))
                 (:file "sivalab" :depends-on ("graph-matrix"))
                 (:file "ullman" :depends-on ("labelled-graph"))
                 (:file "nauty" :depends-on ("labelled-graph"))
                 (:file "node-tree")
                 (:file "pattern-network" :depends-on ("ullman"))
                 ;(:file "graphviz" :depends-on ("graph" "labelled-graph"))
                 ))

(setf (system-bin-directory :graph-tools) (merge-pathnames (format nil "bin/~A/" (platform-name))
                                                           *load-truename*))
 

(defun load-graph-tools (&key (op 'load-op))
  (asdf:operate op '#:graph-tools))
