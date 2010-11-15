(defpackage :org.ileriseviye.grammar-web
  (:use 
   :common-lisp 
   :net.aserve 
   :net.html.generator
   :org.ileriseviye.linear2tree))

(in-package :org.ileriseviye.grammar-web)

(defun grammar-form (req ent)
  (let* ((body (get-request-body req))
	 (text (if body 
		   (cdr (assoc "quotation" (form-urlencoded-to-query body) :test #'equal)))))
    (with-http-response (req ent)
      (with-http-body (req ent)
	(if text
	    (progn 
	      (with-open-file (stream "/home/fz/programming/Lisp/graphviz/grammar-input.txt" 
			       :direction :output 
			       :if-does-not-exist :create 
			       :if-exists :overwrite)
		(format stream "~a~%" text))
	      (produce-tree-output)
	      (html 
	       (:html
		(:head (:title "Linear to Grammar Parse Tree")
		       (:princ "<link rel=StyleSheet href=\"grammar-style.css\" type=\"text/css\">")
		       (:body 
			(:princ "
		       <table width=500>
		        <tr>
		         <td width=150 valign=top>
		           <table>

		       <tr>
		       <td>
		       <p> <a href=\"linear2tree.html\">Main Page</a> </p>
		       </td>
		       </tr>
   
                       <tr>
                        <td>
                          <p> <a href=\"linear2tree-about.html.\">About the program</a></p>
                        </td>
                       </tr>
     
                      <tr>
                       <td>
                        <p> <a href=\"grammar-form\">Try the program</a> </p>
                       </td>
                      </tr>

                     <tr>
                     <td>
                       <p> <a href=\"linear2tree-author.html\">About the author</a> </p>
                     </td>
                    </tr>

                    <tr>
                     <td>
                      <p> <a href=\"linear2tree-credits.html\">Credits</a> </p>
                     </td> 
                    </tr>
                  </table>
                </td>
                <td width=350 valign=top>")

			(:princ "<p>Here are the files:</p><br><br>")
			(:princ "<a href=http://fz.dyndns.org:8080/todot-output.png>todot-output.png</a><br>")
			(:princ "<a href=http://fz.dyndns.org:8080/todot-output.ps>todot-output.ps</a>")))))
	      (publish-file :path "/todot-output.png" :file "/home/fz/programming/Lisp/graphviz/todot-output.png")
	      (publish-file :path "/todot-output.ps" :file "/home/fz/programming/Lisp/graphviz/todot-output.ps"))

	    ; print the form
	    (html
	     (:html
	      (:head (:title "Linear to Grammar Parse Tree")
		     (:princ "<link rel=StyleSheet href=\"grammar-style.css\" type=\"text/css\">")
		     (:body 
		      (:princ "

		       <table width=500>
		        <tr>
		         <td width=150 valign=top>
		           <table>

		       <tr>
		       <td>
		       <p> <a href=\"linear2tree.html\">Main Page</a> </p>
		       </td>
		       </tr>
   
                       <tr>
                        <td>
                          <p> <a href=\"linear2tree-about.html\">About the program</a></p>
                        </td>
                       </tr>
     
                      <tr>
                       <td>
                        <p> Try the program </p>
                       </td>
                      </tr>

                     <tr>
                     <td>
                       <p> <a href=\"linear2tree-author.html\">About the author</a> </p>
                     </td>
                    </tr>

                    <tr>
                     <td>
                      <p> <a href=\"linear2tree-credits.html\">Credits</a> </p>
                     </td> 
                    </tr>
                  </table>
                </td>

                <td width=350 valign=top>")
		      ((:form :action "grammar-form"
			      :method "POST")
		       "<p>Enter your linear nested bracketed parse tree 
                       and press the Produce the parse tree button, e.g. <br><br>
                       [PP [Spec right] [P' [P across] [NP [Spec the] [N' [N bridge]]]]]
                       </p>
                       <br>"
		       :br
		       ((:textarea
			 :name "quotation"
			 :rows 10
			 :cols 40))
		       :br
		       ((:input :type "submit"
				:name "submit"
				:value "Produce the parse tree")))
		      (:princ "     </td>
   </tr>
  </table>")
		      )))))))))

(publish :path "/grammar-form" :content-type "text/html; charset=utf-8" :function 'grammar-form)
(publish-file :path "/grammar-style.css" :file "/home/fz/programming/Lisp/graphviz/grammar-style.css")
(publish-file :path "/linear2tree.html" :file "/home/fz/programming/Lisp/graphviz/linear2tree.html")
(publish-file :path "/linear2tree-about.html" :file "/home/fz/programming/Lisp/graphviz/linear2tree-about.html")
(publish-file :path "/linear2tree-author.html" :file "/home/fz/programming/Lisp/graphviz/linear2tree-author.html")
(publish-file :path "/linear2tree-credits.html" :file "/home/fz/programming/Lisp/graphviz/linear2tree-credits.html")

