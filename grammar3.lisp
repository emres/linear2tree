(defvar *linear*)

; had to use P* instead of P' because ' is treated special 
;by the lisp reader
(setf *linear* '(PP (Spec right) (P* (P across) (NP (Spec the) (N* (N bridge))))))

(defun first-value (list)
  (if (atom list)
      list
      (car list)))

(defun print-rule (list)
  (if (and (not (null list)) (not (null (cdr list))))
      (progn
        (format t "~a -> {" (car list))
        (format t "~a" (first-value (cadr list)))
        (if (null (cddr list))
            (format t "};~%")
            (format t " ~a};~%" (first-value (caddr list)))))))

(defun convert (list)
  (if (and (not (null list)) (not (atom list)))
      (progn
        (print-rule list)
        (convert (cadr list))
        (convert (caddr list)))))