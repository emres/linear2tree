(defvar *linear*)

; had to use P* instead of P' because ' is treated special 
;by the lisp reader
(setf *linear* '(PP (Spec right) (P* (P across) (NP (Spec the) (N* (N bridge))))))

;; (defun convert (linear)
;;   (cond ((and 
;; 	  (car linear) 
;; 	  (not (listp (car (cdr linear)))) 
;; 	  (not (listp (car (cddr linear))))) ; if there is a first element
;; 	 (format t "~a~%" (car linear)))
;; 	(t (convert (cdr linear)))))

;; (defun convert2 (linear)
;;   (cond ((null linear) t)
;; 	(t
;; 	 (format t "~a" (car linear)))))

;; (defun x-traverse-list (list)
;;   (if (null list) 
;;       nil 
;;       (if (listp (car list))
;; 	  (traverse-list (car list))
;; 	  (progn (format t "~a~%" (car list))
;; 		 (traverse-list (cdr list))
;; 		  (traverse-list (cddr list))))))

(defun traverse-list (list)
  (cond
    ((null list) nil)

    ((atom (car list))
     (format t "~a~%" (car list))
     (traverse-list (cdr list)))

    ((listp (car list))
     (traverse-list (car list))
     (traverse-list (cdr list)))

    (t
     (traverse-list (cdr list)))))

;; using higher order function
;; example by Pascal Bourguignon
;; http://groups-beta.google.com/group/comp.lang.lisp/browse_frm/thread/f346f525026e8802/29d2c1882156e0bd?hl=en#29d2c1882156e0bd
(defun traverse-list2 (list func)
   (cond
     ((null list) nil)
     ((atom list)  (funcall func list))
     (t            (mapcar (lambda (item) (traverse-list2 item func)) list)))) 

;; more complicated and sophisticated example
;; again by Pascal Bourguignon
(defun traverse-list3 (list &key (before  (function identity))
                                 (process (function identity))
                                 (after   (function identity)))
   (funcall before list)
   (cond
     ((null list) nil)
     ((atom list)  (funcall process list))
     (t            (dolist (item list)
                       (traverse-list3 item :before  before
                                           :process process
                                           :after   after))))
   (funcall after list))

;; So that we can write
;; (let ((indent 0))
;;   (traverse-list3 (append *linear* *linear*)
;; 		  :before (lambda (item) (when (consp item) (incf indent)))
;; 		  :after  (lambda (item) (when (consp item) (decf indent)))
;; 		  :process (lambda (item)
;; 			     (format t "~VA ~A~%" (* 2 indent) "" item)))) 


;; if list is not null
;;  print the FIRST element (HEAD) of the list
;;    print -> {
;;      if the second element is not null
;;         if the second element is a list 
;;            then print its FIRST element
;;      if the third elements is not null
;;          if the third elements is a list
;;             then print its FIRST element
;;    print };
;;  repeat for the remaining of the list


;; (defun convert (list)
;;   (cond
;;     ((null list) nil)

;;     (t
;;      (format t "~A ->" (car list)))

;;     ((null (cadr list)) nil)

;;     (t
;;      (format t "~A " (caadr list)))

;;     ((null (caddr list)) nil)

;;     (t
;;      (format t "~A " (caaddr list)))))


         
(defun convert (list)
  (if (null list) 
      nil
      (if (atom (car list))
	  (format t "~A -> {" (car list))
	  (format t "~A -> {" (caar list))))

  (if (null (cadr list))
      nil
      (if (atom (cadr list))
	  (format t "~A" (cadr list))
	  (format t "~A" (caadr list))))

  (if (null (caddr list)) 
      nil
      (format t " ~A" (caaddr list)))

  (format t "};~%")

  (if (cadr (list))
      (convert (cadr list))
      nil)

  (if (caddr list)
      (convert (caddr list))
      nil))

