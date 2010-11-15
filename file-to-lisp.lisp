;; Emre's initial and faulty version:
;; (defun read-linear-file (file-name)
;; "Reads the file with the given name
;; and converts it to a list"
;;   (with-open-file (stream file-name)
;;     (setf *linear* (read-line stream)))
;;   (setf *linear* (substitute #\* #\' (substitute #\) #\] (substitute #\( #\[ *linear*))))
;;   (setf *linear* (list *linear*))
;;   (format t "~a~%" *linear*))


;; This piece is Peter Seibel's suggestion:
;;   (defun read-thing (stream)
;;     "This is the entry point"
;;     (skip-whitespace stream)
;;     (if (member (peek-char t stream t) '(#\[ #\())
;;         (read-list stream)
;;         (read-symbol stream)))

;;   (defun read-list (stream)
;;     (loop with close = (ecase (read-char stream t) (#\[ #\]) (#\( #\)))
;;        for next = (peek-char nil stream t)
;;        while (char/= next close) collect (read-thing stream)
;;        do (skip-whitespace stream)
;;        finally (read-char stream t)))

;;   (defun read-symbol (stream)
;;     (intern 
;;      (with-output-to-string (s)
;;        (loop for next = (peek-char nil stream nil nil)
;;           while (and next (symbol-char-p next)) do 
;;             (write-char (convert-symbol-char (read-char stream t)) s)))))

;;   (defun symbol-char-p (char)
;;     (not (member char '(#\Space #\Newline #\[ #\] #\( #\)))))

;;   (defun convert-symbol-char (char)
;;     (if (char= #\' char) #\* (char-upcase char)))

;;   (defun skip-whitespace (stream)
;;     (loop for char = (peek-char nil stream)
;;          while (member char '(#\Space #\Newline)) do (read-char stream t)))
;; usage: (with-input-from-string (in "[PP [Spec (right)] [P' [P across] [NP the bridge]]]]") (read-thing in))


;; This one suggested by Pascal Bourguignon
;; (defun read-linear-file (file-name)
;;   "Reads the first line in the file at path FILE-NAME and 
;;    parses it to a list of tokens and sublists."
;;   (let ((line (with-open-file (stream file-name) (read-line stream))))
;;     (dolist (substitution '(( #\* #\' )
;;                             ( #\) #\] )
;;                             ( #\( #\[ )))
;;       (setf line (substitute (first substitution) (second substitution) line)))
;;     (let ((*read-eval* nil)) (read-from-string line))))

;; (defparameter *linear* (read-linear-file "input"))
;; (format t "~S~%" *linear*)



;; This was suggested by Kent M. Pitman
;; simpler and more compact code. Emre S. has
;; slightly changed it.

;; (defvar *linear*)
;; (defvar *parser-readtable* (copy-readtable))

;; (defun read-bracketed-list (stream)
;;   (read-delimited-list #\] stream))

;; (set-syntax-from-char #\' #\A *parser-readtable*)

;; (set-syntax-from-char #\] #\) *parser-readtable*)

;; (set-macro-character #\[ 'read-bracketed-list *parser-readtable*)

;; (defun parse-text (text)
;;   (let ((*readtable* *parser-readtable*))
;;     (with-input-from-string (s (substitute #\* #\' text))
;;       (read s))))

; (parse-text "[PP [Spec (right)] [P' [P across] [NP the bridge]]]")
; => (PP (SPEC (RIGHT)) (P* (P ACROSS) (NP THE BRIDGE)))

;Or, if you add this in the syntax table setup code above:

; (setf (readtable-case *parser-readtable*) :preserve)

;you can get

; (parse-text "[PP [Spec (right)] [P' [P across] [NP the bridge]]]")
; => (PP (|Spec| (|right|)) (P* (P |across|) (NP |the| |bridge|)))

;the only disadvantage here is that you're allowing other Lisp syntax
;to creep in.  (Peter's answer gives you more tight control over the 
;allowed syntax, at the cost that you have to write all that detailed
;parsing code.)


;; (defun read-linear-file (file-name)
;;   "Reads the first line in the file at path FILE-NAME and 
;;    parses it to a list of tokens and sublists."
;;   (let ((line (with-open-file (stream file-name) (read-line stream))))
;;     (dolist (substitution '(( #\* #\' )
;;                             ( #\) #\] )
;;                             ( #\( #\[ )))
;;       (setf line (substitute (first substitution) (second substitution) line)))
;;     (let ((*read-eval* nil)) (read-from-string line))))

;; (defparameter *linear* (read-linear-file "input"))
;; (format t "~S~%" *linear*)


;;; Suggested by Andrew Philpot
;;; a node is either a leaf (atom) or an interior node (list)

(defun leaf-node-p (node) (and (atom node) (not (null node))))
(defun interior-node-p (node) (and (consp node) (>= (length node) 2)))
(defun node-label (node) (car node))
(defun node-children (node) (cdr node))

(defun next-id (node cache)
  (declare (ignorable node))
  (format nil "N~D" (hash-table-count cache)))

(defun id (node ids)
  (or (gethash node ids)
      (setf (gethash node ids)
	(next-id node ids))))

(defun emit (node stream ids)
  (cond ((leaf-node-p node)
	 (emit-leaf node ids stream))
	((interior-node-p node)
	 (emit-interior node ids stream)
	 (dolist (child (node-children node))
	   (emit child stream ids)))
	(t (error "Parse error: ~S" node))))
	
(defun emit-node-label (node label ids stream)
  (format stream "~&~A [label=\"~A\"]~%"
	  (id node ids) label))

(defun emit-leaf (node ids stream)
  (emit-node-label node node ids stream))

(defun emit-interior (node ids stream)
  (emit-node-label node (node-label node) ids stream)
  (format stream "~&~A -> {~{~A~^ ~}}~%"
	  (id node ids)
	  (mapcar #'(lambda (n) (id n ids)) (node-children node))))

(defun parse2dot (graph &key (stream *standard-output*) 
			     (graph-label "L0")
			     (size "8,8")
			     (ordering "out")
			     (node-shape "plaintext"))
  (format stream "digraph ~A {~%~8@Tsize = ~S;~%~8@Tordering=~A;~%~8@Tnode [shape = ~A];"
	  graph-label
	  size
	  ordering
	  node-shape)
  (emit graph stream (make-hash-table :test #'eql))
  (format stream "~&}~%"))



;;; Suggested by Icarus Sparry
;;; the code to convert a Lisp list into DOT file (a graph)
(defun todot (list)
  (format t "digraph LO {~%")
  (dotit list)
  (format t "};~%"))

(defun dotit (list)
  (let ((lab (node list)))
    (cond ((and (consp (second list)) (third list))
           (format t "~a ->{~a ~a};~%" lab (dotit (second list))
                   (dotit (third list))))
          ((consp (second list))
           (format t "~a -> ~a;~%" lab (dotit (second list))))
          (t
           (format t "~a -> ~a;~%" lab (node (rest list)))))
    lab))

(defparameter *node* -1)

(defun node (list)
  (let ((lab (format nil "n~d" (incf *node*))))
    (format t "~a [label=\"~s\"]~%" lab (car list))
    lab)) 



;;
;;
;; Suggested by Paul Foley
;;
;;
(defstruct node number label children)

(defstruct graph nodes)

(defun frob (list)
  (let ((map (make-hash-table)) (next -1) (nodes '()))
    (labels ((id (node)
	       (or (gethash node map) (setf (gethash node map) (incf next))))
	     (frob1 (n symbol children)
	       (push (make-node :number n
				:label (get symbol 'label-text
					    (string-downcase symbol))
				:children children)
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
  (format t "~&digraph L0 {~%~8Tsize = \"8,8\";~%~8Tordering=out;~%~
               ~8Tnode [shape = plaintext];~2%")
  (dolist (node (graph-nodes graph))
    (format t "~&~8Tn~D [label=~S];" (node-number node) (node-label node)))
  (dolist (node (graph-nodes graph))
    ;; I assume "n1 -> {n3};" is also legal
    (when (node-children node)
      (format t "~&~8Tn~D -> {~{n~D~^ ~}};"
	      (node-number node) (node-children node))))
  (format t "~&}"))


;;
;;
;; The code to convert a bracketed list (from file)
;; into a Lisp list
;;
(defvar *parser-readtable* (copy-readtable))

(defun read-bracketed-list (stream char)
  (read-delimited-list #\] stream))

(set-syntax-from-char #\' #\A *parser-readtable*)

(set-syntax-from-char #\] #\) *parser-readtable*)

(set-macro-character #\[ 'read-bracketed-list nil *parser-readtable*)

(defun parse-text (text)
  (let ((*readtable* *parser-readtable*))
    (with-input-from-string (s (substitute #\* #\' text))
      (read s)))) 

(defun read-linear-file (file-name)
"Reads the first line in the file at path FILE-NAME 
and parses it to a list of tokens and sublists"  
  (let ((line (with-open-file (stream file-name) (read-line stream))))
    (setf line (parse-text line))))

;; calling an external program, 2 examples:
;; (with-output-to-string (stream) (run-program "/bin/ls" '("-la") :output stream))
;; (run-program "/bin/touch" '("geceyarisi.txt") :output t)

(setf *linear* (read-linear-file "/home/fz/programming/Lisp/graphviz/grammar-input.txt"))
;(setf *linear* (todot *linear* *standard-output*))
;(with-open-file (stream "/home/fz/programming/Lisp/graphviz/todot-output.dot" :direction :output)
;  (todot *linear* stream))

(dot (frob *linear*))

(with-open-file (*standard-output* "/home/fz/programming/Lisp/graphviz/todot-output.dot" 
		:direction :output 
		:if-does-not-exist :create 
		:if-exists :overwrite)
  (todot *linear*))

(with-output-to-string 
    (stream)
     (run-program 
      "/usr/bin/dot" 
      '("-Tpng" "/home/fz/programming/Lisp/graphviz/todot-output.dot" "-o" "/home/fz/programming/Lisp/graphviz/todot-output.png") :output stream))

;(run-program 
; "/usr/bin/dot" 
; '("-Tpng /home/fz/programming/Lisp/graphviz/todot-output.dot" "-o" "/home/fz/programming/Lisp/graphviz/todot-output.png"))