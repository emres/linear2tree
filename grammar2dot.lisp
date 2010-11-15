(defpackage :org.ileriseviye.linear2tree
  (:use 
   :common-lisp)
  (:export :produce-tree-output))

(in-package :org.ileriseviye.linear2tree)

;; Suggested by Paul Foley
;;
(defstruct node 
  number 
  label 
  children)

(defstruct graph 
  nodes)

(defparameter *fstr* (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t))

(defun frob (list)
  (let ((map (make-hash-table)) (next -1) (nodes '()))
    (labels ((id (node)
	       (or (gethash node map) (setf (gethash node map) (incf next))))
	     (frob1 (n symbol children)
	       (push (make-node :number n :label (get symbol 'label-text symbol) :children children)
		     nodes))
	     (frob (node)
	       (cond ((consp node)
		      (frob1 (id node) (first node) (mapcar #'id (rest node)))
		      (mapcar #'frob (rest node)))
		     (t
		      (frob1 (id node) node '())))))
      (frob list)
      (make-graph :nodes (sort nodes #'< :key #'node-number)))))


(defun dot (graph)
  (with-output-to-string (s *fstr*)
	       (format s "~&digraph L0 {~%~8Tsize = \"8,8\";~%~8Tordering=out;~%~
               ~8Tnode [shape = plaintext];~%~8Tedge [sametail];~%")
	       (dolist (node (graph-nodes graph))
		 (format s "~&~8Tn~D [label=\"~A\"];" (node-number node) (node-label node)))
	       (dolist (node (graph-nodes graph))
		 ;; I assume "n1 -> {n3};" is also legal
		 (when (node-children node)
		   (format s "~&~8Tn~D -> {~{n~D~^ ~}} [arrowhead=none];"
			   (node-number node) (node-children node))))
	       (format s "~&}")))


;;
;; The code to convert a bracketed list (from file)
;; into a Lisp list, suggested by Kent M. Pitman
;;
(defvar *parser-readtable* (copy-readtable))

(defun read-bracketed-list (stream char)
  (read-delimited-list #\] stream))

;; #\K or any other character does
(set-syntax-from-char #\' #\K *parser-readtable*)

(set-syntax-from-char #\] #\) *parser-readtable*)

;; hack for handling Turkish characters
;; (set-syntax-from-char #\ü #\a *parser-readtable*)
;; (set-syntax-from-char #\Ü #\a *parser-readtable*)
;; (set-syntax-from-char #\ö #\a *parser-readtable*)
;; (set-syntax-from-char #\Ö #\a *parser-readtable*)
;; (set-syntax-from-char #\ç #\a *parser-readtable*)
;; (set-syntax-from-char #\Ç #\a *parser-readtable*)

(set-macro-character #\[ 'read-bracketed-list nil *parser-readtable*)

(defun parse-text (text)
  (let ((*readtable* *parser-readtable*))
    (setf (readtable-case *readtable*) :preserve) 
    (with-input-from-string (s (substitute #\* #\' text))
      (read s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-linear-file (file-name)
"Reads file at path FILE-NAME until finding a [,
then parses it to a list of tokens and sublists"
  (let ((line (with-open-file (stream file-name) 
		(peek-char #\[ stream)	
		(read-line stream))))
    (parse-text line)))

(defvar *linear* nil)

;; (setf *linear* (read-linear-file "/home/fz/programming/Lisp/graphviz/grammar-input.txt"))

;; (with-open-file (*standard-output* "/home/fz/programming/Lisp/graphviz/todot-output.dot" 
;; 		:direction :output 
;; 		:if-does-not-exist :create 
;; 		:if-exists :overwrite)
;;   (dot (frob *linear*))
;;   (format t "~a" (substitute #\' #\* *fstr*)))

;; (with-output-to-string 
;;     (stream)
;;      (run-program 
;;       "/usr/bin/dot" 
;;       '("-Tpng" "/home/fz/programming/Lisp/graphviz/todot-output.dot" "-o" "/home/fz/programming/Lisp/graphviz/todot-output.png") 
;;       :output stream))

(defun produce-tree-output ()
  (setf *fstr* (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t))
  (setf *linear* (read-linear-file "/home/fz/programming/Lisp/graphviz/grammar-input.txt"))
  (with-open-file (*standard-output* "/home/fz/programming/Lisp/graphviz/todot-output.dot" 
		   :direction :output 
		   :if-does-not-exist :create 
		   :if-exists :overwrite)
    (dot (frob *linear*))
    (format t "~a" (substitute #\' #\* *fstr*)))
  (with-output-to-string (stream)
    (sb-ext:run-program 
     "/usr/bin/dot" 
     '("-Tpng" "/home/fz/programming/Lisp/graphviz/todot-output.dot" "-o" "/home/fz/programming/Lisp/graphviz/todot-output.png") 
     :output stream)
    (sb-ext:run-program 
     "/usr/bin/dot" 
     '("-Tps" "/home/fz/programming/Lisp/graphviz/todot-output.dot" "-o" "/home/fz/programming/Lisp/graphviz/todot-output.ps") 
     :output stream)))