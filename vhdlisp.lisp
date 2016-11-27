(defparameter *code* nil) 
(defparameter *operators* '(or and xor nxor nor nand \= > < => =<))
(defparameter *assign* "<=")

;;function to read lisp object from file-name 
(defun read-lisp (file-name)
  (with-open-file (stream file-name)
    (loop for lines = (read stream nil :eof)
       until (equal lines :eof)
       collect lines)))


;;function to create library
(defun library (lst &optional stream)
  (declare (ignore stream))
  (format t "library ")
  (loop for item in lst
     do
       (format t ",~a "  (string-downcase (symbol-name item)) ))
  (format t ";~%"))

;;function to create vhdl use
(defun use (lst &optional stream)
  (declare (ignore stream))
  (loop for item in lst
     do
       (format t "use ~a;~%" (string-downcase item))))


;;will receive a list with the head beeing the name
;;aux function to entity
(defun entity-creation (lst &optional (trigger nil) (comma nil) stream )
  (declare (ignore stream))
  (if (null lst) nil
      (let ( (symbol (car lst)))
	(cond ( (and (null trigger) (equal symbol 'is))
	       (progn (format t ": ")
		      (entity-creation (cdr lst) t comma))) ;;Change trigger to true 
	      ( (and (null trigger) (null comma))
	       (progn (format t "~a" (string-downcase symbol))
		      (entity-creation (cdr lst) trigger t))) ;; change commo to true 
	      ( (and (null trigger) comma)
	       (progn  (format t ", ~a" (string-downcase symbol))
		       (entity-creation (cdr lst) trigger comma))) ;; none changes
	      ( (and trigger) (progn (format t " ~a" (string-downcase symbol))
				     (entity-creation (cdr lst) trigger comma)))
	      (t (error "invalid format at entity declaration"))) )))
       
  
;;Function to create entity
(defun entity (lst &optional stream)
  (declare (ignore stream))
  (let* ( (name (car lst))
	  (rest (cdr lst))
	  (rest-size (length rest))
	  (count 0 ))1
    (format t "entity ~a is~%" (string-downcase (car lst)))
    (format t "    port( ")
    (loop for item in rest
       do
	 (progn (entity-creation item)
		(if (equal count (1- rest-size)) (format t "); ~%")
		    (format t ";~%"))
		(incf count)))
    
    (format t "   end ~a;~%" (string-downcase name)) ))




;;Used for transforming all king of operations
(defun operations (lst)
  (cond ( (null lst) )
	( (numberp lst) lst)
	( (atom lst) (string-downcase lst))
	( (equal (car lst) 'not) (list "not" (operations (cadr lst))))
	( (equal (car lst) 'set) (list (cadr lst) *assign* (operations (caddr lst)))) 
        ( (member (car lst) *operators*) (list (operations (cadr lst))
					       (string-downcase (car lst))
					       (operations (caddr lst))))
	(t lst)))


;;Take a list full of atoms and transform it to a list of strigs
(defun transform-list-of-atom(lst)
  (loop for item in lst
     collect
       (cond ( (numberp item) item)
	     ( (atom item) (string-downcase item))) ))

	     
       
;;Parser for if statements
(defun if-pars (lst)
  (cond ( (equal (car lst) 'if) (progn (format t "if ")
				       (format t "~a then~%" (transform-list-of-atom (operations (cadr lst)) ))
				       (if-pars (cddr lst))))
	( (equal (car lst) 'elsif) (progn (format t "elsif ")
				       (format t "~a then~%" (transform-list-of-atom (operations (cadr lst)) ))
				       (if-pars (cddr lst))))
	( (equal (car lst) 'else) (progn (format t "else ~%")
					 (if-pars (cdr lst)) ))
	( (null lst) (format t "end if")) 
	(t (progn (format t "        ~a;~%"  (operations (car lst)))
		  (if-pars (cdr lst)) )) ))
			  


;;Parser for process
(defun process-pars (lst)
  (format t "process")
  (format t "~a~%" (car lst))
  (format t "begin~%")
  (cond ( (equal (caadr lst) 'if) (if-pars (cadr lst)))
	( t (format t "~a;~%" (transform-list-of-atom (operations (cadr lst))) )))
  (format t "~%end process;"))
  

;;Architecture parser




;;MUST DO SIGNAL DECLARATION BEFORE BEGIN

(defun arch-pars (lst)
  (let ( (name (string-downcase (car lst)))
	 (of-at (cadr lst))
	 (entity-name (string-downcase (caddr lst)))
	 (rest (cadddr lst)))	 
  (format t "~&architecture ~a" name)
  (if (equal of-at 'of) (format t " of ")
      (error 'malformed-architecture-input' :text "You forgot 'of' in the architecture declaration"))
  (format t "~a is" entity-name)
  (format t "~&begin~%")
  (parser rest)
  (format t "~&end ~a~%" name)))


;;Main function parser	   
(defun parser (lst)
  (let ( (head (car lst))
	 (body (cdr lst))) 
    (cond ( (equal head 'library) (library body))
	  ( (equal head 'use) (use body))
	  ( (equal head 'define-entity) (entity body))
	  ( (equal head 'process) (process-pars body))
	  ( (equal head 'def-arch) (arch-pars body))
	  (t nil))))


;;main function
(defun main (file-name)
  (setf *code* (read-lisp file-name))
  (loop for fun in *code*
     do
       (parser fun)))
