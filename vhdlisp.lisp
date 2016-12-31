(declaim (optimize (speed 3) (debug 0) (safety 0))
	 (ftype (function (*) t) read-lisp)
	 (ftype (function (* &optional *) t)  library)
	 (ftype (function (* &optional *) t ) use)
	 (ftype (function (* &optional *) t ) entity)
	 (ftype (function (*) t) operations)
	 (ftype (function (* &optional * * *) entity-creation))
	 (ftype (function (*) t) transform-list-of-atom)
	 (ftype (function (* &optional *) t) if-pars)
	 (ftype (function (* &optional *) t) remove-parentheses)
	 (ftype (function (* &optional *) t) process-pars)
	 (ftype (function (* &optional *) t ) arch-pars)
	 (ftype (function (* &optional *) t) parser)
	 (ftype (function (* &optional *) t) port-aux)
	 (ftype (function (* &optional *) t) port-pars)
	 (ftype (function (* &optional *) t) component)
	 (ftype (function (*) t) translate-file)
	 (ftype (function (*) t) get-name)
	 (ftype (function (*) t) map-input)
	 (ftype (function (* &optional *) t) port-map)
	 (ftype (function (* &optional *) t) port-map-aux)
	 (ftype (function (*) t ) main ))

(defparameter *version* 0.8) ;;Seems stable
(defparameter *creator* "Lucas Guerra Borges")
(defparameter *code* nil) 
(defparameter *operators* '( |or| |and| |xor| |nxor| |nor| |nand| |\=| |>| |<| |=>| |=<| ))
(defparameter *assign* '|<=|)
(defparameter *repl-input* nil)

(defmacro v-format (opc &optional string &rest body)
  `(format ,opc ,string ,@body))

;;function to read lisp object from file-name into tokens list
(defun read-lisp (file-name)
  (let ( (*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (with-open-file (flag file-name)
		    (loop for lines = (read flag nil :eof)
			  until (equal lines :eof)
			  collect lines))))

;;function to create library
(defun library (lst &optional (flag nil))
  (v-format flag "library ")
  (loop for item in lst
     do
       (v-format flag "~a "  (string item)))
  (v-format flag ";~%"))

;;function to create vhdl use
(defun use (lst &optional (flag nil))
  (loop for item in lst
     do
       (v-format flag "use ~a;~%" (string item)) ))

;;will receive a list with the head beeing the name
;;aux function to entity
(defun entity-creation (lst &optional (trigger nil) (comma nil) (flag nil))
  (if (null lst) nil
      (let ( (symbol (car lst)))
	(cond ( (and (null trigger) (equal (string-downcase symbol) "is"))
	       (progn (v-format flag ": ")
		      (entity-creation (cdr lst) t comma flag))) ;;Change trigger to true 
	      ( (and (null trigger) (null comma))
	       (progn (v-format flag "~a" (string symbol))
		      (entity-creation (cdr lst) trigger t flag))) ;; change commo to true 
	      ( (and (null trigger) comma)
	       (progn  (v-format flag ", ~a" (string symbol))
		       (entity-creation (cdr lst) trigger comma flag))) ;; none changes
	      ( (and trigger) (progn (v-format flag " ~a " (string symbol))
				     (entity-creation (cdr lst) trigger comma flag)))
	      (t (error "invalid format at entity declaration"))) )))
  
;;Function to create entity
(defun entity (lst &optional (flag nil))
  (let* ( (name (car lst))
	  (rest (cdr lst))
	  (rest-size (length rest))
	  (count 0 ))1
    (v-format flag "entity ~a is~%" (string (car lst)))
    (v-format flag "port( ")
    (loop for item in rest
       do
	 (progn (entity-creation item nil nil flag)
		(if (equal count (1- rest-size)) (v-format flag "); ~%")
		    (v-format flag ";~%      "))
		(incf count)))
    (v-format flag "   end ~a;~%~%" (string name)) ))

(defun port-aux (lst &optional (flag nil))
  (let ( (size (length lst))
	 (count 1 ))
    (loop for elem in lst
	  do
	  (progn 
	    (loop for item in elem
		  do
		  (cond ( (equal (string-downcase item) "is") (v-format flag ": "))
			(t (v-format flag "~a " (string item))) ))
	    (if (< count size) (progn (incf count)
				      (v-format flag ";~%"))
	      (v-format flag ");~%")) )) ))
		
(defun port-pars (lst &optional (flag nil))
  (cond ( (null lst) nil)
	( (equal (string-downcase (car lst)) "port")
 	  (progn (v-format flag "port ( ")
		 (port-aux (cdr lst) flag)))
	(t (error  "malformated port"))))

;;This function is so ugly that scare me.
;;Sorry for this monstrosity
(defun port-map-aux (lst &optional (flag nil))
  (let ( ( size (length lst))
	 (aux-count 1 ))
    (loop for item in lst
       do
	 (progn 
	   (cond ( (consp item) (v-format flag "~a => ~a" (car item) (cadr item)))
		 ( (atom item) (v-format flag "~a" item)))
	   (if (< aux-count size) (v-format flag ", ")
	       (v-format flag ");~%"))
	   (incf aux-count))) ))

;;(port-map name entity ( a b c ))
(defun port-map (lst &optional (flag nil))
  (let ((name (car lst))
	(entity (cadr lst))
	(rest (caddr lst)))
  (v-format flag "~a: ~a port map ( " name entity)
  (port-map-aux rest flag)))

(defun vhdl-struct(lst &optional (flag nil))
  (v-format flag "begin ~%~%")
  (loop for item in lst
     do 
       (parser item flag))
  (v-format flag "end struct;~%~%" ))

(defun component (lst &optional (flag nil))
  (let ( (head (car lst))
	 (rest (cdr lst)))
    (cond ( (null head) (v-format flag "INSIDE NULL~%"))
	  ( (equal (string-downcase head) "def-comp")
	    (progn (v-format flag "component ")
		   (v-format flag "~a is~%" (string (car rest)))
		   (port-pars (cadr rest) flag)
		   (v-format flag "~&end component;~%~%")))
	  ( t (v-format flag "~&end component;~%~%")) )))

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
(defun if-pars (lst &optional (flag nil))
  (cond ( (equal (car lst) '|if| ) (progn (v-format flag "if ")
				       (v-format flag "~a then~%" (transform-list-of-atom (operations (cadr lst)) ))
				       (if-pars (cddr lst) flag)))
	( (equal (car lst) '|elsif|) (progn (v-format flag "elsif ")
					    (v-format flag "~a then~%"
						    (transform-list-of-atom (operations (cadr lst))))
					  (if-pars (cddr lst) flag )))
	( (equal (car lst) '|else|) (progn (v-format flag "else ~%")
					    (if-pars (cdr lst) flag)))
	( (null lst) (v-format flag "end if;")) 
	(t (progn (remove-parentheses (operations (car lst)) flag)
		  (if-pars (cdr lst) flag))) ))
			  
(defun remove-parentheses (lst &optional (flag nil))
  (loop for element in lst
	do
	(v-format flag "~a " element))
    (v-format flag ";~%"))

;;Parser for process
(defun process-pars (lst &optional (flag nil))
  (v-format flag "process")
  (v-format flag "~a~%" (transform-list-of-atom (car lst)))
  (v-format flag "begin~%")
  (cond ( (equal (string-downcase (caadr lst)) "if") (if-pars (cadr lst) flag))
	( t (remove-parentheses (transform-list-of-atom (operations (cadr lst))) flag)))
  (v-format flag "~%end process;"))

(defun arch-pars-aux (lst flag)
  (loop for item in lst
	do
	(parser item flag)))

;;architecture parser
;;MUST DO SIGNAL DECLARATION BEFORE BEGIN
(defun arch-pars (lst &optional (flag nil))
  (let ( (name (string (car lst)))
	 (of-at (string-downcase (cadr lst)))
	 (entity-name (string (caddr lst))) )

  (v-format flag "~&architecture ~a" name)
  (if (equal of-at "of") (v-format flag " of ")
      (error 'malformed-architecture-input' :text "You forgot 'of' in the architecture declaration"))
  (v-format flag "~a is" entity-name)
  (v-format flag "~&begin~%")
  (arch-pars-aux (cdddr lst) flag)
  (v-format flag "~&end ~a;~%" name)))

;;Lisp object parser   
(defun parser (lst &optional flag)
  (let ( (head (car lst))
	 (body (cdr lst)))
    (cond ( (equal (string-downcase head) "library") (library body flag))
	  ( (equal (string-downcase head) "use") (use body flag ))
	  ( (equal (string-downcase head) "define-entity") (entity body flag))
	  ( (equal (string-downcase head) "process") (process-pars body flag))
	  ( (equal (string-downcase head) "def-arch") (arch-pars body flag))
	  ( (equal (string-downcase head) "def-comp") (component lst flag))
	  ( (equal (string-downcase head) "def-struct") (vhdl-struct body flag))
	  ( (equal (string-downcase head) "port-map") (port-map body flag))
	  (t lst)) ))

;;Function called when compiled version run
(defun compile-main ()
  (let ( (file-name (cadr *posix-argv*)))
    (main file-name)))

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
(defun main (file)
  (format t "---------------------------~%")
  (if (consp file)
      (map-input file)
      (translate-file file))
  (format t "----------------------------~%"))

(defun get-name (str)
  (let ( (pos 0 ))
    (loop for char = (aref str pos)
	  until (equal char #\.)
	  do
	  (incf pos))
    (incf pos)
    (concatenate 'string (subseq str 0 pos) "vhdl")))

;;Run the necessarys routines to transform vhdlisp into vhdl
(defun translate-file (elem)
  (let ( ( code (read-lisp elem))
	 ( output-name (get-name elem)))
    (with-open-file (stream output-name :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
		    (loop for fun in code
			  do
			  (parser fun stream)))
  (format t "~a CREATED~%" output-name)))

(defun map-input (lst-of-files)
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type LIST lst-of-files))
  (mapcar #'translate-file lst-of-files))

(defun get-token ()
  (let ( ( *readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read)))

(defun vhdl-repl ()
  (let ( ( read-var nil))
    (loop
       until (equal read-var '(|exit-repl|))
       do
	 (progn
	   (format t "&- ")
	   (force-output)
	   (setq read-var (get-token))
	   (if (equal read-var '(|exit-repl|))
	       (return)
	       (parser read-var t ))) )))




(defun teste1 ()
  (main "samples/driver.vlisp" ))
(defun teste2 ()
  (main "samples/xor.vlisp"))
(defun teste3 ()
  (map-input '("samples/comp.vlisp" "samples/driver.vlisp" "samples/or_gate.vlisp" "samples/xor.vlisp")))
