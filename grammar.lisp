(defvar *linear*)

(setf *linear* '(PP (Spec right) (P' (P across) (NP the bridge))))

(defun convert (linear)
  (cond ((and 
	  (car linear) 
	  (not (listp (car (cdr linear)))) 
	  (not (listp (car (cddr linear))))) ; if there is a first element
	 (format t "~a~%" (car linear)))
	(t (convert (cdr linear)))))

(defun convert2 (linear)
  (cond ((null linear) t)
	(t
	 (format t "~a" (car linear)))))

(defun traverse-list (list)
  (if (null list) 
      nil 
      (if (listp (car list))
	  (traverse-list (car list))
	  (progn (format t "~a~%" (car list))
		 (traverse-list (cdr list))
		  (traverse-list (cddr list))))))


;if the list is not null
;   if the first element of the list is not a list
;       then print it
;       otherwise repeat the procedure for the rest of the list

