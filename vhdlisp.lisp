(declaim (optimize (speed 3) (debug 0 ))
	 (ftype (function (*) t) read-lisp)
	 (ftype (function (* &optional *) t)  library)
	 (ftype (function (* &optional *) t ) use)
	 (ftype (function (* &optional *) t ) entity)
	 (ftype (function (*) t) operations)
	 (ftype (function (* &optional * * *) entity-creation))
	 (ftype (function (*) t) transform-list-of-atom)
	 (ftype (function (* *) t) if-pars)
	 (ftype (function (* *) t) remove-parentheses)
	 (ftype (function (* *) t) process-pars)
	 (ftype (function (* *) t ) arch-pars)
	 (ftype (function (* *) t) parser)
	 (ftype (function (* &optional *) port-aux))
	 (ftype (function (* &optional *) port-pars))
	 (ftype (function (* &optional *) component))
	 (ftype (function (* *) t ) main ))


(defparameter *version* 0.5)
(defparameter *creator* "Lucas Guerra Borges")
(defparameter *code* nil) 
(defparameter *operators* '( |or| |and| |xor| |nxor| |nor| |nand| |\=| |>| |<| |=>| |=<| ))
(defparameter *assign* '|<=|)

;;function to read lisp object from file-name into tokens list
(defun read-lisp (file-name)
  (let ( (*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (with-open-file (stream file-name)
		    (loop for lines = (read stream nil :eof)
			  until (equal lines :eof)
			  collect lines))))
		
;;function to create library
(defun library (lst &optional stream)
  (format stream "library ")
  (loop for item in lst
     do
       (format stream "~a "  (string item)))
  (format stream ";~%~%"))

;;function to create vhdl use
(defun use (lst &optional stream)
  (loop for item in lst
     do
       (format stream "use ~a;~%~%" (string item))))

;;will receive a list with the head beeing the name
;;aux function to entity
(defun entity-creation (lst &optional (trigger nil) (comma nil) stream )
  (if (null lst) nil
      (let ( (symbol (car lst)))
	(cond ( (and (null trigger) (equal (string-downcase symbol) "is"))
	       (progn (format stream ": ")
		      (entity-creation (cdr lst) t comma stream))) ;;Change trigger to true 
	      ( (and (null trigger) (null comma))
	       (progn (format stream "~a" (string symbol))
		      (entity-creation (cdr lst) trigger t stream))) ;; change commo to true 
	      ( (and (null trigger) comma)
	       (progn  (format stream ", ~a" (string symbol))
		       (entity-creation (cdr lst) trigger comma stream))) ;; none changes
	      ( (and trigger) (progn (format stream " ~a " (string symbol))
				     (entity-creation (cdr lst) trigger comma stream)))
	      (t (error "invalid format at entity declaration"))) )))
  
;;Function to create entity
(defun entity (lst &optional stream)
  (let* ( (name (car lst))
	  (rest (cdr lst))
	  (rest-size (length rest))
	  (count 0 ))1
    (format stream "entity ~a is~%" (string (car lst)))
    (format stream "port( ")
    (loop for item in rest
       do
	 (progn (entity-creation item nil nil stream)
		(if (equal count (1- rest-size)) (format stream "); ~%")
		    (format stream ";~%"))
		(incf count)))
    (format stream "   end ~a;~%~%" (string name)) ))

(defun port-aux (lst &optional stream)
  (let ( (size (length lst))
	 (count 1 ))
    (loop for elem in lst
	  do
	  (progn 
	    (loop for item in elem
		  do
		  (cond ( (equal (string-downcase item) "is") (format stream ": "))
			(t (format stream "~a " (string item))) ))
	    (if (< count size) (progn (incf count)
				      (format stream ";~%"))
	      (format stream ");~%")) )) ))
		
(defun port-pars (lst &optional stream)
  (cond ( (null lst) nil)
	( (equal (string-downcase (car lst)) "port")
 	  (progn (format stream "port ( ")
		 (port-aux (cdr lst) stream)))
	(t (error  "malformated port"))))

(defun component (lst &optional stream)
  (let ( (head (car lst))
	 (rest (cdr lst)))
    (cond ( (null head) (format stream "INSIDE NULL~%"))
	  ( (equal (string-downcase head) "def-comp")
	    (progn (format stream "component ")
		   (format stream "~a is~%" (string (car rest)))
		   (port-pars (cadr rest) stream)))
	  ( t (format stream "~%end component;")) )))

;;Used for transforming all king of operations
(defun operations (lst)
  (cond ( (null lst) " ")
	( (numberp lst) lst)
	( (atom lst) (string lst))
	( (equal (string-downcase (car lst)) "not") (list "not" (operations (cadr lst))))
	( (equal (string-downcase (car lst)) "set") (list (cadr lst) *assign* (operations (caddr lst)))) 
        ( (member (car lst) *operators*) (list (operations (cadr lst))
					       (string (car lst))
					       (operations (caddr lst))))
	(t lst)))

;;Take a list full of atoms and transform it to a list of strigs
(defun transform-list-of-atom(lst)
  (loop for item in lst
     collect
       (cond ( (numberp item) item)
	     ( (atom item) (string item))
	     ( (consp item) (transform-list-of-atom item)))))

;;Parser for if statements
(defun if-pars (lst stream)
  (cond ( (equal (car lst) '|if| ) (progn (format stream "if ")
				       (format stream "~a then~%" (transform-list-of-atom (operations (cadr lst)) ))
				       (if-pars (cddr lst) stream)))
	( (equal (car lst) '|elsif|) (progn (format stream "elsif ")
					    (format stream "~a then~%"
						    (transform-list-of-atom (operations (cadr lst))))
					  (if-pars (cddr lst) stream )))
	( (equal (car lst) '|else|) (progn (format stream "else ~%")
					    (if-pars (cdr lst) stream)))
	( (null lst) (format stream "end if;")) 
	(t (progn (remove-parentheses (operations (car lst)) stream)
		  (if-pars (cdr lst) stream))) ))
			  
(defun remove-parentheses (lst stream)
  (loop for element in lst
	do
	(format stream "~a " element))
    (format stream ";~%"))

;;Parser for process
(defun process-pars (lst stream)
  (format stream "process")
  (format stream "~a~%" (transform-list-of-atom (car lst)))
  (format stream "begin~%")
  (cond ( (equal (string-downcase (caadr lst)) "if") (if-pars (cadr lst) stream))
	( t (remove-parentheses (transform-list-of-atom (operations (cadr lst))) stream)))
  (format stream "~%end process;"))


(defun arch-pars-aux (lst stream)
  (loop for item in lst
	do
	(parser item stream)))
;;architecture parser
;;MUST DO SIGNAL DECLARATION BEFORE BEGIN
(defun arch-pars (lst stream)
  (let ( (name (string (car lst)))
	 (of-at (string-downcase (cadr lst)))
	 (entity-name (string (caddr lst)))
	 (rest (cdddr lst)))	 
  (format stream "~&architecture ~a" name)
  (if (equal of-at "of") (format stream " of ")
      (error 'malformed-architecture-input' :text "You forgot 'of' in the architecture declaration"))
  (format stream "~a is" entity-name)
  (format stream "~&begin~%")
  (arch-pars-aux (cdddr lst) stream)
  (format stream "~&end ~a;~%" name)))


;;Lisp object parser   
(defun parser (lst stream)
  (let ( (head (car lst))
	 (body (cdr lst)))
    (cond ( (equal (string-downcase head) "library") (library body stream))
	  ( (equal (string-downcase head) "use") (use body stream ))
	  ( (equal (string-downcase head) "define-entity") (entity body stream))
	  ( (equal (string-downcase head) "process") (process-pars body stream))
	  ( (equal (string-downcase head) "def-arch") (arch-pars body stream))
	  ( (equal (string-downcase head) "def-comp") (component lst stream)))
	  (t nil))))

;;Function called when compiled version run
(defun compile-main ()
  (let ( (file-name (cadr *posix-argv*))
	 (output-name (caddr *posix-argv*)))
    (main file-name output-name)))

;;Function to compile vhdlisp
(defun compile-vhdlisp ()
  (format t "VHDLISP VERSION ~a~%" *version*)
  (format t "CREATED BY :~a~%" *creator*)
  (format t "Feel free to use and change w/e you want~%")
  (format t "-----------------------------------------~%~%~%")
  (sb-ext:save-lisp-and-die "vhdlisp"
			    :executable t
			    :toplevel #'compile-main
			    ))

;;main function
(defun main (file-name output-file )
  (setf *code* (read-lisp file-name))
  (with-open-file (stream output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
		  (loop for fun in *code*
			do
			(parser fun stream))))

(defun teste1 ()
  (main "samples/driver.vlsp" "driver"))
(defun teste2 ()
  (main "samples/xor.vlisp" "xor"))
