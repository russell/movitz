;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2001-2004, 
;;;;    Department of Computer Science, University of Tromso, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      los-closette.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Jul 23 14:29:10 2002
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(require :muerte/basic-macros)
(require :muerte/typep)
(require :muerte/los-closette-compiler)
(provide :muerte/los-closette)

(in-package muerte)

(defmacro defclass (&environment env name direct-superclasses direct-slots &rest options)
  `(progn
     (eval-when (:compile-toplevel)
       (ensure-class ',name
		     :direct-superclasses
		     ,(canonicalize-direct-superclasses direct-superclasses)
		     :direct-slots
		     ,(canonicalize-direct-slots direct-slots name env)
		     ,@(canonicalize-defclass-options options env name)))))

(defmacro defgeneric (function-name lambda-list &rest options)
  `(eval-when (:compile-toplevel)
     (movitz-ensure-generic-function ',function-name 
				  :lambda-list ',lambda-list
				  ,@(canonicalize-defgeneric-options options))))

(defmacro defmethod (&rest args &environment env)
  (multiple-value-bind (name qualifiers lambda-list specializers body declarations documentation)
      (parse-defmethod args)
    (declare (ignore documentation))
    `(eval-when (:compile-toplevel)
       (let ((gf (movitz-ensure-generic-function (movitz::translate-program ',name :cl :muerte.cl)
					      :lambda-list ',(extract-lambda-list lambda-list))))
	 (ensure-method gf
			:lambda-list ',lambda-list
			:qualifiers ',qualifiers
			:specializers ,(canonicalize-specializers specializers) 
			:body ',body
			:declarations ',declarations
			:environment nil)))))

(defmacro define-slot-reader-method (&environment env name (class-name slot-name))
  (check-type class-name symbol)
  (check-type slot-name symbol)
  `(eval-when (:compile-toplevel)
     (let* ((gf (movitz-ensure-generic-function (movitz::translate-program ',name :cl :muerte.cl)
					    :lambda-list '(object)))
	    (class (movitz-find-class (movitz::translate-program ',class-name :cl :muerte.cl)))
	    (slot-definition (find (movitz::translate-program ',slot-name :cl :muerte.cl)
				   (std-slot-value class 'direct-slots)
				   :key 'slot-definition-name)))
       (assert slot-definition ()
	 "Can't find slot ~S in ~S" ',slot-name ',class-name)
       (ensure-method gf
		      :method-class 'standard-reader-method
		      :slot-definition slot-definition
		      :lambda-list '(object)
		      :qualifiers nil
		      :specializers (list class)
		      :body '(slot-value object ',slot-name)
		      :declarations nil
		      :environment ,env))))

(defmacro define-slot-writer-method (&environment env name (class-name slot-name))
  (check-type class-name symbol)
  (check-type slot-name symbol)
  `(eval-when (:compile-toplevel)
     (let* ((gf (movitz-ensure-generic-function (movitz::translate-program ',name :cl :muerte.cl)
					     :lambda-list '(value object)))
	    (class (movitz-find-class (movitz::translate-program ',class-name :cl :muerte.cl)))
	    (slot-definition (find (movitz::translate-program ',slot-name :cl :muerte.cl)
				   (std-slot-value class 'direct-slots)
				   :key 'slot-definition-name)))
       (assert slot-definition ()
	 "Can't find slot ~S in ~S" ',slot-name ',class-name)
       (ensure-method gf
		      :method-class 'standard-writer-method
		      :slot-definition slot-definition
		      :lambda-list '(value object)
		      :qualifiers nil
		      :specializers (list (movitz-find-class t) class)
		      :body '(setf (slot-value object ',slot-name) value)
		      :declarations nil
		      :environment ,env))))

(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

;;;


;;; std-instance


(defun std-instance-class (instance)
  (std-instance-class instance))

(defun (setf std-instance-class) (value instance)
  (std-instance-writer class value instance))

(defun std-instance-slots (instance)
  (std-instance-slots instance))

(defun (setf std-instance-slots) (value instance)
  (std-instance-writer slots value instance))


(defun allocate-std-instance (class slots)
  (let ((instance (inline-malloc #.(bt:sizeof 'movitz:movitz-std-instance) :tag :other)))
    (setf (memref instance #.(bt:slot-offset 'movitz:movitz-struct 'movitz:type)
		  0 :unsigned-byte8)
      #.(movitz:tag :std-instance))
    (setf-movitz-accessor (instance movitz-std-instance dummy) nil)
    (setf (std-instance-class instance) class
	  (std-instance-slots instance) slots)
    instance))

(defun std-allocate-instance (class)
  (allocate-std-instance class
			 (allocate-slot-storage (count-if 'instance-slot-p (class-slots class))
						(load-global-constant unbound-value))))

(defun allocate-slot-storage (size initial-value)
  (make-array size :initial-element initial-value))

(defun standard-instance-access (instance location)
  ;; (warn "slots: ~S" (std-instance-slots instance))
  (standard-instance-access instance location))

(defun (setf standard-instance-access) (value instance location)
  (setf (svref (std-instance-slots instance) location) value))

;;; std-gf-instance

(defun std-gf-instance-class (instance)
  (check-type instance standard-gf-instance)
  (movitz-accessor instance movitz-funobj-standard-gf standard-gf-class))

(defun std-gf-instance-slots (instance)
  (check-type instance standard-gf-instance)
  (movitz-accessor instance movitz-funobj-standard-gf standard-gf-slots))

(defun std-gf-num-required-arguments (instance)
  (check-type instance standard-gf-instance)
  (movitz-accessor instance movitz-funobj-standard-gf num-required-arguments))

(define-compiler-macro std-gf-num-required-arguments (instance)
  `(movitz-accessor ,instance movitz-funobj-standard-gf num-required-arguments))

(defun std-gf-classes-to-emf-table (instance)
  (check-type instance standard-gf-instance)
  (movitz-accessor instance movitz-funobj-standard-gf classes-to-emf-table))

(define-compiler-macro std-gf-classes-to-emf-table (instance)
  `(movitz-accessor ,instance movitz-funobj-standard-gf classes-to-emf-table))

(defun (setf std-gf-classes-to-emf-table) (value instance)
  (check-type instance standard-gf-instance)
  (setf-movitz-accessor (instance movitz-funobj-standard-gf classes-to-emf-table) value))

(defun std-gf-eql-specializer-table (instance)
  (check-type instance standard-gf-instance)
  (movitz-accessor instance movitz-funobj-standard-gf eql-specializer-table))

(defun (setf std-gf-eql-specializer-table) (value instance)
  (check-type instance standard-gf-instance)
  (setf-movitz-accessor (instance movitz-funobj-standard-gf eql-specializer-table) value))

(defun set-funcallable-instance-function (funcallable-instance function)
  "This function is called to set or to change the function of a funcallable instance.
After set-funcallable-instance-function is called, any subsequent calls to
funcallable-instance will run the new function."
  (check-type funcallable-instance standard-gf-instance)
  (check-type function function)
  (setf-movitz-accessor (funcallable-instance movitz-funobj-standard-gf standard-gf-function)
		     function)
  (values))

(defun instance-slot-p (slot)
  (eq (slot-definition-allocation slot) :instance))

;;;

;;;

(defun find-class (symbol &optional (errorp t))
  (let ((class (gethash symbol *class-table*)))
    (if (and (null class) errorp)
	(error "No class named ~S." symbol)
      class)))

(defun (setf find-class) (class class-name)
  (check-type class (or null class))
  (case class-name
    ((t) (setf (%run-time-context-slot 'the-class-t) class))
    (null (setf (%run-time-context-slot 'the-class-null) class))
    (symbol (setf (%run-time-context-slot 'the-class-symbol) class))
    (fixnum (setf (%run-time-context-slot 'the-class-fixnum) class))
    (cons (setf (%run-time-context-slot 'the-class-cons) class)))
  (if class
      (setf (gethash class-name *class-table*) class)
    (remhash class-name *class-table*))
  class)

(defun class-of (object)
  (class-of object))			; compiler-macro

#+ignore
(defun class-of (object)
  (typecase object
    (std-instance
     (movitz-accessor object movitz-std-instance class))
    (standard-gf-instance
     (movitz-accessor object movitz-funobj-standard-gf standard-gf-class))
    (null
     (find-class 'null))
    (cons
     (find-class 'cons))
    (symbol
     (find-class 'symbol))
    (fixnum
     (find-class 'fixnum))
    (vector
     (find-class 'vector))
    (compiled-function
     (find-class 'function))
    (hash-table
     (find-class 'hash-table))
    (package
     (find-class 'package))
    (structure-object
     (find-class 'structure-object))
    (t (warn "Don't know the class of ~Z!" object)
       (find-class t))))

(defun subclassp (c1 c2)
  (not (null (find c2 (class-precedence-list c1)))))

;;;
;;;
;;;
;;;
;;;
;;;
;;; Generic function stuff


;;; Several tedious functions for analyzing lambda lists

(defun gf-required-arglist (gf)
  (let ((plist (analyze-lambda-list (generic-function-lambda-list gf))))
    (getf plist :required-args)))

(defun analyze-lambda-list (lambda-list)
  (labels ((make-keyword (symbol)
	     (intern (symbol-name symbol)
		     (find-package 'keyword)))
	   (get-keyword-from-arg (arg)
	     (if (listp arg)
		 (if (listp (car arg))
		     (caar arg)
		   (make-keyword (car arg)))
	       (make-keyword arg))))
    (let ((keys ())			; Just the keywords
	  (key-args ())			; Keywords argument specs
	  (required-names ())		; Just the variable names
	  (required-args ())		; Variable names & specializers
	  (specializers ())		; Just the specializers
	  (rest-var nil)
	  (optionals ())
	  (auxs ())
	  (allow-other-keys nil)
	  (state :parsing-required))
      (dolist (arg lambda-list)
	(if (member arg lambda-list-keywords)
	    (ecase arg
	      (&optional
	       (setq state :parsing-optional))
	      (&rest
	       (setq state :parsing-rest))
	      (&key
	       (setq state :parsing-key))
	      (&allow-other-keys
	       (setq allow-other-keys 't))
	      (&aux
	       (setq state :parsing-aux)))
	  (case state
	    (:parsing-required 
	     (push-on-end arg required-args)
	     (if (listp arg)
		 (progn (push-on-end (car arg) required-names)
			(push-on-end (cadr arg) specializers))
	       (progn (push-on-end arg required-names)
		      (push-on-end 't specializers))))
	    (:parsing-optional (push-on-end arg optionals))
	    (:parsing-rest (setq rest-var arg))
	    (:parsing-key
	     (push-on-end (get-keyword-from-arg arg) keys)
	     (push-on-end arg key-args))
	    (:parsing-aux (push-on-end arg auxs)))))
      (list  :required-names required-names
	     :required-args required-args
	     :specializers specializers
	     :rest-var rest-var
	     :keywords keys
	     :key-args key-args
	     :auxiliary-args auxs
	     :optional-args optionals
	     :allow-other-keys allow-other-keys))))

;;;
;;;

;;; Method dispatch

(defvar *sml-context* nil)

(defun slow-method-lookup (gf args classes)
  "Find the emfun and specializers for applying gf to args."
  (declare (dynamic-extent args classes))
  (check-stack-limit)			; This is an infinite recursion hot-spot.
  ;; args and classes are stack-allocated by std-discriminating-function.
  (multiple-value-bind (applicable-methods using-class-of-ok)
      (compute-applicable-methods-using-classes gf (map-into classes #'class-of args))
    (cond
     ((not using-class-of-ok)
      (slow-method-lookup-with-eql-specializers gf args))
     ((null applicable-methods)
      (apply 'no-applicable-method gf args))      
     (t (let ((emfun-form (compute-effective-method gf (generic-function-method-combination gf)
						    applicable-methods)))
	  (if (not emfun-form)
	      (error "nil emfun-form?")	; not sure what this means yet..
	    (let ((emfun (compile-effective-method-form gf emfun-form classes args)))
	      (check-type emfun function)
	      (values emfun classes)
	      #+ignore ((pushnew (cons (if (not (cdr classes))
					   (car classes)
					 (copy-list classes))
				       emfun)
				 (std-gf-classes-to-emf-table gf)
				 :test 'equal)
			(apply emfun args)))))))))

(defun slow-method-lookup-with-eql-specializers (gf args)
  "Find the emfun and specializers for applying gf to args,
knowing there are EQL specialized methods."
  (let ((applicable-methods (compute-applicable-methods gf args)))
    (when (null applicable-methods)
      (apply 'no-applicable-method gf args))
    (let* ((new-es-size (reduce #'max applicable-methods
				:initial-value 0
				:key (lambda (method)
				       (1+ (or (position-if #'eql-specializer-p
							    (method-specializers method))
					       -1)))))
	   ;; An es-table is a list of hash-tables, where each hash-table corresponds
	   ;; to that position in the gf's (required) argument-list.
	   (old-es-table (std-gf-eql-specializer-table gf))
	   ;; Make sure es-table is as long as the rightmost eql-specialized argument.
	   (old-es-size (length old-es-table))
	   (new-es-table (cond
			  ((< old-es-size new-es-size)
			   (nconc old-es-table (make-list (- new-es-size old-es-size))))
			  ((< new-es-size old-es-size)
			   (subseq old-es-table 0 new-es-size))
			  (t old-es-table))))
      ;; In gf's eql-specializer-table, fill in all the eql-specializers
      ;; that applies to these applicable methods.
      (dolist (method applicable-methods)
	(do* ((s (method-specializers method) (cdr s))
	      (specializer (car s) (car s))
	      (es new-es-table (cdr es)))
	    ((null s))
	  (when (eql-specializer-p specializer)
	    (unless (hash-table-p (car es))
	      (setf (car es) (make-hash-table :test 'eql :size 11)))
	    (setf (gethash (eql-specializer-object specializer)
			   (car es))
	      specializer))))
      (setf (std-gf-eql-specializer-table gf) new-es-table)
      ;; Compute the list of specializers that these args map into.
      ;; This is the exact same process as that which happens in the
      ;; (fast) discriminating function. This list is the key used
      ;; for the gf's classes-to-emf-table.
      (let ((active-specializers (make-list (std-gf-num-required-arguments gf))))
	(do ((ac active-specializers (cdr ac))
	     (es new-es-table (cdr es))
	     (a args (cdr a)))
	    ((endp ac))
	  (setf (car ac)
	    (or (and (car es) (gethash (car a) (car es)))
		(class-of (car a)))))
	;; Compute the effective function, cache it, and finally call it.
	(let ((emfun-form (compute-effective-method gf (generic-function-method-combination gf)
						    applicable-methods)))
	  (if (not emfun-form)
	      (error "nil emfun-form?")
	    (let ((emfun (compile-effective-method-form gf emfun-form active-specializers args)))
	      (check-type emfun function)
	      (values emfun active-specializers)
	      #+ignore ((pushnew (cons (if (not (cdr active-specializers))
					   (car active-specializers)
					 (copy-list active-specializers))
				       emfun)
				 (std-gf-classes-to-emf-table gf)
				 :test 'equal)
			(apply emfun args)))))))))

(defun discriminating-function-max-step2 (gf args &rest args-copy)
  "This is a way of manually stack-allocating args-copy."
  (declare (dynamic-extent args-copy))
  (let* ((specializers
	  ;; The prog form below is (hopefully) equivalent to:
	  ;;   ;; chop off args-copy to be same length as (gf-required-arglist gf)..
	  ;;   (setf (cdr (nthcdr (1- (gf-num-required-arguments gf)) args-copy)) nil)
	  ;;   (map-into args-copy #'specializer-of args-copy))
	  (prog ((i (std-gf-num-required-arguments gf))
		 (es (std-gf-eql-specializer-table gf)) ; runs along eql-specializer-table
		 (ac args-copy))	; runs along args-copy
	    (when es
	      (go loop-with-eql-specializers))
	   loop-without-eqls		; no eql-specializing
	    (unless ac
	      (go too-few-arguments))
	    (setf (car ac) (class-of (car ac)))
	    (when (= 0 (decf i))
	      (setf (cdr ac) nil)
	      (return args-copy))
	    (setf ac (cdr ac))
	    (go loop-without-eqls)
	   loop-with-eql-specializers
	    (unless ac
	      (go too-few-arguments))
	    (setf (car ac)
	      (let ((a (car ac))
		    (es-hash (car es)))
		(or (and es-hash (gethash a es-hash))
		    (class-of a))))
	    (when (= 0 (decf i))
	      (setf (cdr ac) nil)
	      (return args-copy))
	    (setf ac (cdr ac))
	    (if (setf es (cdr es))
		(go loop-with-eql-specializers)
	      (go loop-without-eqls))
	   too-few-arguments
	    (error "~D Generic function ~S requires ~D arguments but received ~D arguments: ~S."
		   i gf (std-gf-num-required-arguments gf) (length args) args)
	    (format t "~D Generic function ~S requires ~D arguments but received ~D arguments: ~S."
		    i gf (std-gf-num-required-arguments gf) (length args) args)))
	 (emfun-cons (assoc specializers (std-gf-classes-to-emf-table gf) :test 'equal)
		     #+old-cmop (gethash specializers (std-gf-classes-to-emf-table gf))))
    (if emfun-cons
	(apply (cdr emfun-cons) args)
      (multiple-value-bind (emfun specializers)
	  (slow-method-lookup gf args specializers)
	(push (cons (copy-list specializers)
		    emfun)
	      (std-gf-classes-to-emf-table gf))
	(apply emfun args)))))

(defun discriminating-function-max (&edx gf &rest args)
  "The most general GF dispatcher."
  (numargs-case
   #+ignore
   (1 (&edx gf arg0)
      (let* ((es-table (car (std-gf-eql-specializer-table gf)))
	     (specializer (or (and es-table (gethash arg0 es-table))
			      (class-of arg0)))
	     (emfun (assoc specializer (std-gf-classes-to-emf-table gf))))
	(if emfun
	    (funcall (cdr emfun) arg0)
	  (let ((args (list arg0)))
	    (multiple-value-bind (emfun active-specializers)
		(slow-method-lookup gf args (list specializer))
	      (assert (= 1 (length active-specializers)))
	      (push (cons (car active-specializers)
			  emfun)
		    (std-gf-classes-to-emf-table gf))
	      (funcall emfun arg0))))))
   #+ignore
   (2 (&edx gf arg0 arg1)		; outdated
      (let* ((es-tables (std-gf-eql-specializer-table gf))
	     (es-table0 (pop es-tables))
	     (es-table1 (pop es-tables))
	     (specializer0 (or (and es-table0 (gethash arg0 es-table0))
			       (class-of arg0)))
	     (specializer1 (or (and es-table1 (gethash arg1 es-table1))
			       (class-of arg1)))
	     (emfun (gethash-doubleton (std-gf-classes-to-emf-table gf) specializer0 specializer1)))
	(if emfun
	    (funcall emfun arg0 arg1)
	  (slow-method-lookup gf (list arg0 arg1) (list specializer0 specializer1)))))
   (t (&edx gf &rest args)
      (declare (dynamic-extent args))
      (apply 'discriminating-function-max-step2 gf args args))))

(defun discriminating-function-no-eqls (&edx gf &rest args)
  "Dispatcher for GF's with no EQL-specialized methods."
  (declare (dynamic-extent args))
  (apply 'discriminating-function-max-step2 gf args args))

(defun cached-lookup-failed-map1 (gf &rest arg0-class-optionals)
  (declare (dynamic-extent arg0-class-optionals))
  (let ((arg0-list arg0-class-optionals)
	(arg0-class-list (cdr arg0-class-optionals))
	(optional-args (cddr arg0-class-optionals)))
    ;; In case the gf has more than one required arguments, in which
    ;; case we know there are no method specializing on them (see
    ;; initial-discriminating-function), we must append some arguments
    ;; so that slow-method-lookup will work.
    (setf (cdr arg0-list)
      (make-list (1- (std-gf-num-required-arguments gf))
		 :initial-element nil))
    (setf (cdr arg0-class-list)
      (make-list (1- (std-gf-num-required-arguments gf))
		 :initial-element (find-class t)))
    (multiple-value-bind (emfun active-specializers)
	(slow-method-lookup gf arg0-list arg0-class-list)
      ;; (assert (null (cdr active-specializers)))
      (push (cons (car active-specializers)
		  emfun)
	    (std-gf-classes-to-emf-table gf))
      (when (< 5 (length (std-gf-classes-to-emf-table gf)))
	(warn "method cache size for ~S: ~D"
	      gf (length (std-gf-classes-to-emf-table gf))))
      (apply emfun (car arg0-list) optional-args))))

(defun cached-lookup-failed-map10 (gf &rest arg01-class01-optionals)
  (declare (dynamic-extent arg01-class01-optionals))
  (let ((arg01-list arg01-class01-optionals)
	(class01-list (cddr arg01-class01-optionals))
	(optional-args (cddddr arg01-class01-optionals)))
    ;; In case the gf has more than two required arguments, in which
    ;; case we know there are no method specializing on them (see
    ;; initial-discriminating-function), we must append some arguments
    ;; so that slow-method-lookup will work.
    (setf (cddr arg01-list)
      (make-list (- (std-gf-num-required-arguments gf) 2)
		 :initial-element nil))
    (setf (cddr class01-list)
      (make-list (- (std-gf-num-required-arguments gf) 2)
		 :initial-element (find-class t)))
    (multiple-value-bind (emfun active-specializers)
	(slow-method-lookup gf arg01-list class01-list)
      (push (cons (cadr active-specializers)
		  emfun)
	    (std-gf-classes-to-emf-table gf))
      (when (< 4 (length (std-gf-classes-to-emf-table gf)))
	(warn "method cache size for ~S: ~D"
	      gf (length (std-gf-classes-to-emf-table gf))))
      (apply emfun (car arg01-list) (cadr arg01-list) optional-args))))

(defun cached-lookup-failed-map11 (gf &rest args)
  (declare (dynamic-extent args))
  (multiple-value-bind (emfun active-specializers)
      (slow-method-lookup gf args (mapcar #'class-of args))
    (push (list* (first active-specializers)
		 (second active-specializers)
		 emfun)
	  (std-gf-classes-to-emf-table gf))
    (when (< 4 (length (std-gf-classes-to-emf-table gf)))
      (warn "method cache size for ~S: ~D"
	    gf (length (std-gf-classes-to-emf-table gf))))
    (apply emfun args)))

(defun cached-lookup-failed-map101 (gf &rest args)
  (declare (dynamic-extent args))
  (multiple-value-bind (emfun active-specializers)
      (slow-method-lookup gf args (mapcar #'class-of args))
    (push (list* (first active-specializers)
		 (third active-specializers)
		 emfun)
	  (std-gf-classes-to-emf-table gf))
    (when (< 4 (length (std-gf-classes-to-emf-table gf)))
      (warn "method cache size for ~S: ~D"
	    gf (length (std-gf-classes-to-emf-table gf))))
    (apply emfun args)))

(defun cached-lookup-failed-map111 (gf &rest args)
  (declare (dynamic-extent args))
  (multiple-value-bind (emfun active-specializers)
      (slow-method-lookup gf args (mapcar #'class-of args))
    (push (list* (first active-specializers)
		 (second active-specializers)
		 (third active-specializers)
		 emfun)
	  (std-gf-classes-to-emf-table gf))
    (when (< 4 (length (std-gf-classes-to-emf-table gf)))
      (warn "method cache size for ~S: ~D"
	    gf (length (std-gf-classes-to-emf-table gf))))
    (apply emfun args)))

(defun cached-lookup-failed-map1111 (gf &rest args)
  (declare (dynamic-extent args))
  (multiple-value-bind (emfun active-specializers)
      (slow-method-lookup gf args (mapcar #'class-of args))
    (push (list* (first active-specializers)
		 (second active-specializers)
		 (third active-specializers)
		 (fourth active-specializers)
		 emfun)
	  (std-gf-classes-to-emf-table gf))
    (when (< 4 (length (std-gf-classes-to-emf-table gf)))
      (warn "method cache size for ~S: ~D"
	    gf (length (std-gf-classes-to-emf-table gf))))
    (apply emfun args)))

(defun discriminating-function-map1-no-eqls (&edx gf arg0 &rest optional-args)
  (numargs-case
   (1 (&edx gf arg0)
      (let ((class (class-of arg0)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map1 gf arg0 class))
	  (when (eq class (car entry))
	    (return (funcall%unsafe (cdr entry) arg0))))))
   (2 (&edx gf arg0 optional1)
      (let ((class (class-of arg0)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map1 gf arg0 class optional1))
	  (when (eq class (car entry))
	    (return (funcall%unsafe (cdr entry) arg0 optional1))))))
   (3 (&edx gf arg0 optional1 optional2)
      (let ((class (class-of arg0)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map1 gf arg0 class optional1 optional2))
	  (when (eq class (car entry))
	    (return (funcall%unsafe (cdr entry) arg0 optional1 optional2))))))
   (t (&edx gf arg0 &rest optional-args)
      (declare (dynamic-extent optional-args))
      (let ((class (class-of arg0)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (apply 'cached-lookup-failed-map1 gf arg0 class optional-args))
	  (when (eq class (car entry))
	    (return (apply (cdr entry) arg0 optional-args))))))))

(defun discriminating-function-map1-with-eqls (&edx gf arg0 &rest optional-args)
  (numargs-case
   (1 (&edx gf arg0)
      (let* ((es-table (car (std-gf-eql-specializer-table gf)))
	     (specializer (or (and es-table (gethash arg0 es-table))
			      (class-of arg0))))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map1 gf arg0 specializer))
	  (when (eq specializer (car entry))
	    (return (funcall%unsafe (cdr entry) arg0))))))
   (2 (&edx gf arg0 optional1)
      (let* ((es-table (car (std-gf-eql-specializer-table gf)))
	     (specializer (or (and es-table (gethash arg0 es-table))
			      (class-of arg0))))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map1 gf arg0 specializer optional1))
	  (when (eq specializer (car entry))
	    (return (funcall%unsafe (cdr entry) arg0 optional1))))))
   (t (&edx gf arg0 &rest optional-args)
      (declare (dynamic-extent optional-args))
      (let* ((es-table (car (std-gf-eql-specializer-table gf)))
	     (specializer (or (and es-table (gethash arg0 es-table))
			      (class-of arg0))))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (apply #'cached-lookup-failed-map1 gf arg0 specializer optional-args))
	  (when (eq specializer (car entry))
	    (return (apply (cdr entry) arg0 optional-args))))))))      

(defun discriminating-function-map10-no-eqls (&edx gf arg0 arg1 &rest optional-args)
  "map10 means dispatch on arg1 only."
  (numargs-case
   (2 (&edx gf arg0 arg1)
      (let ((class (class-of arg1)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map10 gf arg0 arg1 (class-of arg0) class))
	  (when (eq class (car entry))
	    (return (funcall%unsafe (cdr entry) arg0 arg1))))))
   (3 (&edx gf arg0 arg1 optional2)
      (let ((class (class-of arg1)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map10 gf arg0 arg1 (class-of arg0) class optional2))
	  (when (eq class (car entry))
	    (return (funcall%unsafe (cdr entry) arg0 arg1 optional2))))))
   (t (&edx gf arg0 arg1 &rest optional-args)
      (declare (dynamic-extent optional-args))
      (let ((class (class-of arg1)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (apply 'cached-lookup-failed-map10
			 gf arg0 arg1 (class-of arg0) class optional-args))
	  (when (eq class (car entry))
	    (return (apply (cdr entry) arg0 arg1 optional-args))))))))

(defun discriminating-function-map11-no-eqls (&edx gf arg0 arg1 &rest optional-args)
  "map11 means dispatch on arg0 and arg1."
  (numargs-case
   (2 (&edx gf arg0 arg1)
      (let ((class0 (class-of arg0))
	    (class1 (class-of arg1)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map11 gf arg0 arg1))
	  (let ((e entry))
	    (when (and (eq class0 (pop e))
		       (eq class1 (pop e)))
	      (return (funcall%unsafe e arg0 arg1)))))))
   (3 (&edx gf arg0 arg1 optional2)
      (let ((class0 (class-of arg0))
	    (class1 (class-of arg1)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map11 gf arg0 arg1 optional2))
	  (let ((e entry))
	    (when (and (eq class0 (pop e))
		       (eq class1 (pop e)))
	      (return (funcall%unsafe e arg0 arg1 optional2)))))))
   (t (&edx gf arg0 arg1 &rest optional-args)
      (declare (dynamic-extent optional-args))
      (let ((class0 (class-of arg0))
	    (class1 (class-of arg1)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (apply 'cached-lookup-failed-map11 gf arg0 arg1 optional-args))
	  (let ((e entry))
	    (when (and (eq class0 (pop e))
		       (eq class1 (pop e)))
	      (return (apply e arg0 arg1 optional-args)))))))))

(defun discriminating-function-map101-no-eqls (&edx gf arg0 arg1 &rest optional-args)
  "map101 means dispatch on arg0 and arg2."
  (numargs-case
   (3 (&edx gf arg0 arg1 arg2)
      (let ((class0 (class-of arg0))
	    (class2 (class-of arg2)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (cached-lookup-failed-map101 gf arg0 arg1 arg2))
	  (let ((e entry))
	    (when (and (eq class0 (pop e))
		       (eq class2 (pop e)))
	      (return (funcall%unsafe e arg0 arg1 arg2)))))))
   (t (&edx gf arg0 arg1 arg2 &rest optional-args)
      (declare (dynamic-extent optional-args))
      (let ((class0 (class-of arg0))
	    (class2 (class-of arg2)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (apply 'cached-lookup-failed-map101 gf arg0 arg1 arg2 optional-args))
	  (let ((e entry))
	    (when (and (eq class0 (pop e))
		       (eq class2 (pop e)))
	      (return (apply e arg0 arg1 arg2 optional-args)))))))))

(defun discriminating-function-map111 (&edx gf arg0 arg1 arg2 &rest optional-args)
  (declare (dynamic-extent optional-args))
  (let ((es-table (std-gf-eql-specializer-table gf)))
    (macrolet ((specializer-of (arg)
		 `(let ((es (pop es-table)))
		    (or (and es (gethash ,arg es))
			(class-of ,arg)))))
      (let ((specializer0 (specializer-of arg0))
	    (specializer1 (specializer-of arg1))
	    (specializer2 (specializer-of arg2)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (apply 'cached-lookup-failed-map111 gf arg0 arg1 arg2 optional-args))
	  (let ((e entry))
	    (when (and (eq specializer0 (pop e))
		       (eq specializer1 (pop e))
		       (eq specializer2 (pop e)))
	      (return (apply e arg0 arg1 arg2 optional-args)))))))))

(defun discriminating-function-map1111 (&edx gf arg0 arg1 arg2 arg3 &rest optional-args)
  (declare (dynamic-extent optional-args))
  (let ((es-table (std-gf-eql-specializer-table gf)))
    (macrolet ((specializer-of (arg)
		 `(let ((es (pop es-table)))
		    (or (and es (gethash ,arg es))
			(class-of ,arg)))))
      (let ((specializer0 (specializer-of arg0))
	    (specializer1 (specializer-of arg1))
	    (specializer2 (specializer-of arg2))
	    (specializer3 (specializer-of arg3)))
	(dolist (entry (std-gf-classes-to-emf-table gf)
		  (apply 'cached-lookup-failed-map1111 gf arg0 arg1 arg2 arg3 optional-args))
	  (let ((e entry))
	    (when (and (eq specializer0 (pop e))
		       (eq specializer1 (pop e))
		       (eq specializer2 (pop e))
		       (eq specializer3 (pop e)))
	      (return (apply e arg0 arg1 arg2 arg3 optional-args)))))))))

(defvar *forward-generic-function* nil
  "Used to allow :no-clos-fallback functions see which gf it was forwarded from.")

(defun initial-discriminating-function (&edx gf &rest args)
  (declare (dynamic-extent args))
  ;; (warn "initial-df: ~S" (funobj-name gf))
  (check-stack-limit)			; This is a infinite recursion hot-spot.
  (cond
   ((not (get 'clos-bootstrap 'have-bootstrapped))
    (let ((x (std-gf-classes-to-emf-table gf)))
      (cond
       ((not x)
	(warn "Auto-bootstrapping CLOS.")
	(clos-bootstrap)
	(unless (get 'clos-bootstrap 'have-bootstrapped)
	  (error "Giving up auto-bootstrapping of CLOS."))
	(warn "Auto-bootstrapping CLOS completed.")
	(apply gf args))
       #+ignore
       ((and (functionp x)
	     (= #.(bt:enum-value 'movitz:movitz-funobj-type :method-function)))
	(apply x args))			; This GF is a :no-clos-fallback, so call it.
       (t (check-type x function)
	  (let ((*forward-generic-function* gf))
	    (apply x args))))))		; This GF is a :no-clos-fallback, so call it.
   (t (let ((eqls-p (some (lambda (method)
			    (some #'eql-specializer-p (method-specializers method)))
			  (generic-function-methods gf)))
	    (specializer-bitmap
	     (if (< 20 (std-gf-num-required-arguments gf))
		 -1
	       (reduce #'logior (generic-function-methods gf)
		       :key (lambda (method)
			      (do ((result 0)
				   (i 1 (ash i 1))
				   (m (method-specializers method) (cdr m)))
				  ((or (endp m) (>= i (truncate most-positive-fixnum 2)))
				   result)
				(unless (eq (car m) (find-class t))
				  (incf result i))))))))
	(assert (or (< 1 (std-gf-num-required-arguments gf))
		    (>= 1 specializer-bitmap))
	    () "Weird map ~b for ~S." specializer-bitmap gf)
	(setf (std-gf-classes-to-emf-table gf) nil)
	(set-funcallable-instance-function
	 gf
	 (cond
	  ((= 0 specializer-bitmap)
	   ;; No method that specializes anything..
	   (assert (= 1 (length (generic-function-methods gf))))
	   (method-function (first (generic-function-methods gf))))
	  ((and (not eqls-p)
		(= #b1 specializer-bitmap))
	   #'discriminating-function-map1-no-eqls)
	  ((= #b1 specializer-bitmap)
	   #'discriminating-function-map1-with-eqls)
	  ((and (not eqls-p)
		(= #b10 specializer-bitmap))
	   #'discriminating-function-map10-no-eqls)
	  ((and (not eqls-p)
		(= #b11 specializer-bitmap))
	   #'discriminating-function-map11-no-eqls)
	  ((and (not eqls-p)
		(= specializer-bitmap (logand #b101 specializer-bitmap)))
	   #'discriminating-function-map101-no-eqls)
	  ((and (<= 3 (std-gf-num-required-arguments gf))
		(= specializer-bitmap (logand #b111 specializer-bitmap)))
	   #'discriminating-function-map111)
	  ((and (<= 4 (std-gf-num-required-arguments gf))
		(= specializer-bitmap (logand #b1111 specializer-bitmap)))
	   #'discriminating-function-map1111)
	  (t (warn "Defaulting map ~b for ~S~@[ with eql-specializers~]."
		   specializer-bitmap gf eqls-p)
	     (if eqls-p
		 #'discriminating-function-max
	       #'discriminating-function-no-eqls))))
	(apply gf args)))))

(defun no-unspecialized-fallback (&rest ignore)
  (declare (ignore ignore))
  (error "A :no-clos-fallback generic function for which there
is no unspecialized method was called."))

;;;

(defun primary-method-p (method)
  (null (method-qualifiers method)))
(defun before-method-p (method)
  (equal '(:before) (method-qualifiers method)))
(defun after-method-p (method)
  (equal '(:after) (method-qualifiers method)))
(defun around-method-p (method)
  (equal '(:around) (method-qualifiers method)))


(defmacro define-effective-slot-reader (name location)
  `(defun ,name (instance)
     (with-inline-assembly (:returns :multiple-values)
       (:compile-form (:result-mode :eax) instance)
;;;       (:leal (:eax -2) :ecx)
;;;       (:testb 7 :cl)
;;;       (:jnz '(:sub-program () (:int 68)))
       (:movl (:eax ,(bt:slot-offset 'movitz::movitz-std-instance 'movitz::slots))
	      :eax)
       (:movl (:eax ,(+ (bt:slot-offset 'movitz::movitz-vector 'movitz::data)
			(* location 4)))
	      :eax)
       (:cmpl :eax ,(movitz::make-indirect-reference :edi (movitz::global-constant-offset 'unbound-value)))
       (:je '(:sub-program (unbound)
	      (:compile-form (:result-mode :multiple-values) (slot-unbound-trampoline instance ,location))
	      (:jmp 'done)))
       (:clc)
       done)))

(defparameter *standard-effective-slot-readers*
    (vector 'standard-effective-slot-reader%0
	    'standard-effective-slot-reader%1
	    'standard-effective-slot-reader%2
	    'standard-effective-slot-reader%3
	    'standard-effective-slot-reader%4
	    'standard-effective-slot-reader%5
	    'standard-effective-slot-reader%6
	    'standard-effective-slot-reader%7)
  "The element at position i is a standard-reader
for a slot at position i.")

(define-effective-slot-reader standard-effective-slot-reader%0 0)
(define-effective-slot-reader standard-effective-slot-reader%1 1)
(define-effective-slot-reader standard-effective-slot-reader%2 2)
(define-effective-slot-reader standard-effective-slot-reader%3 3)
(define-effective-slot-reader standard-effective-slot-reader%4 4)
(define-effective-slot-reader standard-effective-slot-reader%5 5)
(define-effective-slot-reader standard-effective-slot-reader%6 6)
(define-effective-slot-reader standard-effective-slot-reader%7 7)

(defun compute-effective-slot-reader (class slot-definition)
  (let* ((slot-name (slot-definition-name slot-definition))
	 (slot (find-slot class slot-name)))
    (assert slot (slot-name)
      "No slot named ~S in class ~S." slot-name class)
    (let ((slot-location (slot-definition-location slot)))
      (check-type slot-location (integer 0 *))
      (etypecase class
	(standard-class
	 (if (and (< slot-location (length *standard-effective-slot-readers*))
		  (svref *standard-effective-slot-readers* slot-location))
	     (symbol-function (svref *standard-effective-slot-readers* slot-location))
	   (lambda (instance)
	     (let ((x (standard-instance-access instance slot-location)))
	       (if (not (eq x (load-global-constant unbound-value)))
		   x
		 (slot-unbound-trampoline instance slot-location))))))
	(funcallable-standard-class
	 (lambda (instance)
	   (let ((x (svref (std-gf-instance-slots instance) slot-location)))
	     (if (not (eq x (load-global-constant unbound-value)))
		 x
	       (slot-unbound-trampoline instance slot-location)))))))))

(defun compute-effective-slot-writer (class slot-definition)
  (let* ((slot-name (slot-definition-name slot-definition))
	 (slot (find-slot class slot-name)))
    (assert slot (slot-name)
      "No slot named ~S in class ~S." slot-name class)
    (let ((slot-location (slot-definition-location slot)))
      (assert slot-location)
      (etypecase class
	(standard-class
	 (lambda (value instance)
	   (setf (standard-instance-access instance slot-location)
	     value)))
	(funcallable-standard-class
	 (lambda (value instance)
	   (setf (svref (std-gf-instance-slots instance) slot-location)
	     value)))))))

(defun make-emfun (method next-emf)
  "Make an effective method function from method that will have
next-emf as its target for call-next-method."
  (let* ((f (method-function method))
	 (p (do ((l (funobj-num-constants f))
		 (i 0 (1+ i)))
		((>= i l) nil)
	      (when (eq 'proto-next-emf	(funobj-constant-ref f i))
		(return i)))))
    (if (or (not p) (null next-emf))
	f
      (let ((new-f (copy-funobj f)))
	(setf (funobj-constant-ref new-f p) next-emf)
	new-f))))

(defun compute-primary-emfun (methods)
  (if (null methods)
      nil
    (make-emfun (car methods) (compute-primary-emfun (cdr methods)))))

;;;

(defclass symbol (t) () (:metaclass built-in-class))
(defclass sequence (t) () (:metaclass built-in-class))
(defclass array (t) () (:metaclass built-in-class))
(defclass character (t) () (:metaclass built-in-class))
;; (defclass hash-table (t) () (:metaclass built-in-class))
;;;(defclass package (t) () (:metaclass built-in-class))
;;;(defclass pathname (t) () (:metaclass built-in-class))
;;;(defclass readtable (t) () (:metaclass built-in-class))
(defclass list (sequence) () (:metaclass built-in-class))
(defclass null (symbol list) () (:metaclass built-in-class))
(defclass cons (list) () (:metaclass built-in-class))
(defclass vector (array sequence) () (:metaclass built-in-class))
(defclass bit-vector (vector) () (:metaclass built-in-class))
(defclass string (vector) () (:metaclass built-in-class))
(defclass simple-array (array) () (:metaclass built-in-class))
(defclass simple-vector (vector simple-array) () (:metaclass built-in-class))
(defclass restart () () (:metaclass built-in-class))
(defclass basic-restart (restart) () (:metaclass built-in-class))

(defclass number (t) () (:metaclass built-in-class))
(defclass real (number) () (:metaclass built-in-class))
(defclass rational (real) () (:metaclass built-in-class))
(defclass integer (rational) () (:metaclass built-in-class))
(defclass fixnum (integer) () (:metaclass built-in-class))
(defclass ratio (rational) () (:metaclass built-in-class))
(defclass float (real) () (:metaclass built-in-class))
(defclass complex (number) () (:metaclass built-in-class))

(defclass run-time-context (t)
  ()
  (:metaclass built-in-class)
  (:size #.(bt:sizeof 'movitz::movitz-constant-block))
  (:slot-map #.(movitz::slot-map 'movitz::movitz-constant-block
			       (cl:+ (bt:slot-offset 'movitz::movitz-constant-block
						     'movitz::constant-block-start)
				     0))))

(defclass stream () ())

;;;

(defclass method-combination (metaobject)
  ((type
    :initarg :type)
   (options
    :initform nil
    :initarg :options)))

(defclass standard-method-combination (method-combination) ())

(define-compile-time-variable *the-standard-method-combination*
    (movitz-make-instance 'standard-method-combination
		       :type 'standard))

;;; Funcallables

(defclass funcallable-standard-class (std-slotted-class) ())

(defclass function (t) () (:metaclass built-in-class))
(defclass funcallable-standard-object (standard-object function) ())
(defclass generic-function (metaobject funcallable-standard-object) ())
(defclass standard-generic-function (generic-function)
  (#+ignore
   (name
    :initarg :name)			; :accessor generic-function-name
   #+ignore
   (lambda-list				; :accessor generic-function-lambda-list
    :initarg :lambda-list)
   (methods
    :initform ())			; :accessor generic-function-methods)
   (method-class			; :accessor generic-function-method-class
    :initarg :method-class)
   (method-combination
    :initform *the-standard-method-combination*
    :initarg :method-combination))
  (:metaclass funcallable-standard-class))

(defclass method (metaobject) ())
(defclass standard-method (method standard-object)
  ((qualifiers
    :initarg :qualifiers)		; :accessor method-qualifiers
   (specializers
    :initarg :specializers)		; :accessor method-specializers
   (generic-function
    :initform nil)			; :accessor method-generic-function
   (function)				; :accessor method-function
   (optional-arguments-p)))

(defclass standard-accessor-method (standard-method)
  ((slot-definition
    :initarg :slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())
(defclass standard-writer-method (standard-accessor-method) ())


;;; Now we can define gfs and methods

(define-slot-reader-method accessor-method-slot-definition
    (standard-accessor-method slot-definition))
(define-slot-reader-method generic-function-methods
    (standard-generic-function methods))
(define-slot-reader-method generic-function-method-combination
    (standard-generic-function method-combination))
(define-slot-reader-method class-slots (std-slotted-class effective-slots))
(define-slot-reader-method class-name (class name))
(define-slot-reader-method class-plist (class plist))
(define-slot-reader-method class-direct-subclasses (class direct-subclasses))
(define-slot-reader-method class-precedence-list (class class-precedence-list))
(define-slot-reader-method class-prototype-value (class prototype))
(define-slot-writer-method (setf class-prototype-value) (class prototype))

(defmethod class-slots ((class class)) nil)
;; (defmethod class-slots ((class std-slotted-class)) nil)

(define-slot-reader-method method-optional-arguments-p (standard-method optional-arguments-p))
(define-slot-reader-method method-function (standard-method function))
(define-slot-reader-method method-specializers (standard-method specializers))
(define-slot-reader-method method-qualifiers (standard-method qualifiers))
(define-slot-reader-method method-generic-function (standard-method generic-function))

(defmethod method-lambda-list ((method standard-method))
  (funobj-lambda-list (method-function method)))

;;;(define-slot-reader-method generic-function-name (standard-generic-function name))
;;;(define-slot-reader-method generic-function-lambda-list (standard-generic-function lambda-list))

;;;;

(defclass structure-class (class)
  ((slots
    :initarg :slots
    :accessor structure-slots)))

(defclass structure-object (t) () (:metaclass structure-class))

(defun make-structure (class &rest init-args)
  (declare (dynamic-extent init-args))
  (let ((class (if (symbolp class) (find-class class nil) class)))
    (check-type class structure-class)
    (let* ((slots (structure-slots class))
	   (num-slots (length slots))
	   (struct (inline-malloc (+ #.(bt:sizeof 'movitz::movitz-struct)
				     (* 4 num-slots)
				     (if (evenp num-slots) 0 1)))))
      (setf (memref struct #.(bt:slot-offset 'movitz::movitz-struct 'movitz::name)
		    0 :lisp)
	(class-name class))
      (setf (memref struct #.(bt:slot-offset 'movitz::movitz-struct 'movitz::type)
		    0 :unsigned-byte8)
	#.(movitz::tag :defstruct))
      (setf (memref struct #.(bt:slot-offset 'movitz::movitz-struct 'movitz::length)
		    0 :unsigned-byte16)
	num-slots)
      (dotimes (i num-slots)
	(setf (structure-ref struct i) nil))
      (do ((p init-args (cddr p)))
	  ((endp p))
	(let ((slot-position (position (car p) slots :key #'fifth)))
	  (assert slot-position ()
	    "Illegal init-arg ~S for ~S." (car p) class)
	  (setf (structure-ref struct slot-position) (cadr p))))
      struct)))

;;;;


(defmethod generic-function-name ((gf standard-generic-function))
  (funobj-name gf))

(defmethod generic-function-lambda-list ((gf standard-generic-function))
  (funobj-lambda-list gf))

;;; Finalization

(defmethod class-prototype ((class class))
  (let ((x (slot-value class 'prototype)))
    (assert x () "The class ~S is not finalized." class)
    x))

(defmethod class-finalized-p ((class class))
  (not (null (class-prototype-value class))))

(defmethod finalize-inheritance ((class standard-class))
  (unless (class-prototype-value class)
    (setf (class-prototype-value class)
      (std-allocate-instance class))))

;;; Instance creation and initialization

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (dynamic-extent initargs) (ignore initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (std-allocate-instance class))

(defun compute-defaulted-initargs (class initargs)
  (dolist (class (class-precedence-list class))
    (let ((d (getf (class-plist class) :default-initargs-function)))
      (when d
	(let ((fun (car d))
	      (indicators (cdr d)))
	  (dolist (indicator indicators)
	    (unless (do ((p initargs (cddr p)))
			((endp p) nil)
		      (when (eq (car p) indicator)
			(return t)))
	      (push (funcall fun indicator) initargs)
	      (push indicator initargs)))))))
  initargs)

(defmethod make-instance ((class standard-class) &rest initargs)
  (declare (dynamic-extent initargs))
  (let ((defaulted-initargs (compute-defaulted-initargs class initargs)))
    (apply 'initialize-instance
	   (apply 'allocate-instance class defaulted-initargs)
	   defaulted-initargs)))

(defmethod make-instance ((class symbol) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply 'make-instance (find-class class) initargs))

(defgeneric initialize-instance (instance &rest initargs))
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply 'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &rest initargs))
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply 'shared-initialize instance () initargs))

(defgeneric shared-initialize (instance slot-names &rest all-keys))
(defmethod shared-initialize ((instance standard-object) slot-names &rest all-keys)
  (declare (dynamic-extent all-keys))
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
	  (get-properties all-keys (slot-definition-initargs slot))
	(declare (ignore init-key))
	(if foundp
	    (setf (slot-value instance slot-name) init-value)
	  (when (and (not (slot-boundp instance slot-name))
		     (not (null (slot-definition-initfunction slot)))
		     (or (eq slot-names t)
			 (member slot-name slot-names)))
	    (let ((initfunction (slot-definition-initfunction slot)))
	      (setf (slot-value instance slot-name)
		(etypecase initfunction
		  (cons (cadr initfunction)) ; '(quote <obj>)
		  (function (funcall initfunction))))))))))
  instance)


;;; Slot access

(defvar *standard-slot-value-using-class* nil)
(defvar *standard-setf-slot-value-using-class* nil)
(defvar *standard-gf-slot-value-using-class* nil)
(defvar *standard-gf-setf-slot-value-using-class* nil)

(define-slot-reader-method slot-definition-name
    (slot-definition name))
(define-slot-reader-method slot-definition-location
    (standard-effective-slot-definition location))
(define-slot-reader-method slot-definition-allocation
    (standard-slot-definition allocation))
(define-slot-reader-method slot-definition-initargs
    (standard-slot-definition initargs))
(define-slot-reader-method slot-definition-initfunction
    (standard-slot-definition initfunction))
(define-slot-reader-method slot-definition-initform
    (standard-slot-definition initform))

(defun find-slot (class slot-name)
  (dolist (slot (if (eq class *the-class-standard-class*)
		    *the-slots-of-standard-class*
		  (class-slots class)) #+ignore (error "The slot ~S doesn't exist in ~S." slot-name class))
    (when (eql slot-name (slot-definition-name slot))
      (return slot))))

(defun std-slot-value (instance slot-name)
  (let* ((location (slot-definition-location (find-slot (std-instance-class instance) slot-name)))
         (slots (std-instance-slots instance))
         (val (svref slots location)))
    (if (eq (load-global-constant unbound-value) val)
        (error "The slot ~S is unbound in the object ~S."
               slot-name instance)
      val)))

(defun std-gf-slot-value (instance slot-name)
  (let ((slot (find-slot (std-gf-instance-class instance) slot-name)))
    (assert slot)
    (let* ((location (slot-definition-location slot))
	   (slots (std-gf-instance-slots instance))
	   (val (svref slots location)))
      (if (eq (load-global-constant unbound-value) val)
	  (error "The slot ~S is unbound in the object ~S."
		 slot-name instance)
	val))))

(defun slot-value (object slot-name)
  (let* ((class (class-of object))
	 (slot (find-slot class slot-name)))
    (if (not slot)
	(values (slot-missing class object slot-name 'slot-value))
      (slot-value-using-class class object slot))))

(defmethod slot-value-using-class ((class standard-class) object (slot standard-effective-slot-definition))
  (let ((x (standard-instance-access object (slot-definition-location slot))))
    (if (eq x (load-global-constant unbound-value))
	(slot-unbound class object (slot-definition-name slot))
      x)))

(defmethod slot-value-using-class ((class funcallable-standard-class)
				   object
				   (slot standard-effective-slot-definition))
  (let* ((location (slot-definition-location slot))
         (slots (std-gf-instance-slots object))
         (val (svref slots location)))
    (if (eq (load-global-constant unbound-value) val)
	(slot-unbound class object (slot-definition-name slot))        
      val)))

(defun (setf slot-value) (new-value object slot-name)
  (let* ((class (class-of object))
	 (slot (find-slot class slot-name)))
    (if (not slot)
	(progn
	  (slot-missing class object slot-name 'setf new-value)
	  new-value)
      (setf (slot-value-using-class class object slot)
	new-value))))

(defmethod (setf slot-value-using-class) (new-value (class standard-class) object
					  (slot standard-effective-slot-definition))
  (setf (standard-instance-access object (slot-definition-location slot)) new-value))

(defmethod (setf slot-value-using-class) (new-value (class funcallable-standard-class) object
					  (slot standard-effective-slot-definition))
  (let ((location (slot-definition-location slot))
	(slots (std-gf-instance-slots object)))
    (setf (svref slots location) new-value)))

(defun slot-boundp (object slot-name)
  (let* ((class (class-of object))
	 (slot (find-slot class slot-name)))
    (if (not slot)
	(and (slot-missing class object slot-name 'slot-boundp) t)
      (slot-boundp-using-class class object slot))))

(defmethod slot-boundp-using-class ((class standard-class) object (slot standard-effective-slot-definition))
  (not (eq (load-global-constant unbound-value)
	   (standard-instance-access object (slot-definition-location slot)))))
  
(defmethod slot-boundp-using-class ((class funcallable-standard-class) object (slot standard-effective-slot-definition))
  (not (eq (load-global-constant unbound-value)
	   (svref (std-gf-instance-slots object) (slot-definition-location slot)))))

(defmethod slot-boundp-using-class ((class built-in-class) object slot)
  (error "Slot-boundp-using-class called on built-in-class ~S" class))

(defun slot-makunbound (object slot-name)
  (let* ((class (class-of object))
	 (slot (find-slot class slot-name)))
    (if (not slot)
	(progn (slot-missing class object slot-name 'slot-makunbound)
	       object)
      (slot-makunbound-using-class class object slot))))

(defmethod slot-makunbound-using-class ((class standard-class) object (slot standard-effective-slot-definition))
  (setf (standard-instance-access object (slot-definition-location slot))
    (load-global-constant unbound-value))
  object)
  
(defmethod slot-makunbound-using-class ((class funcallable-standard-class) object (slot standard-effective-slot-definition))
  (setf (svref (std-gf-instance-slots object) (slot-definition-location slot))
    (load-global-constant unbound-value))
  object)

(defmethod slot-makunbound-using-class ((class built-in-class) object slot)
  (error "Slot-makunbound-using-class called on built-in-class ~S" class))

(defun slot-exists-p (object slot-name)
  (if (not (typep object '(or std-instance standard-gf-instance)))
      nil
    (dolist (slot (class-slots (class-of object)))
      (when (eql slot-name (slot-definition-name slot))
	(return t)))))

;;; Specializers

(defun eql-specializer-p (specializer)
  (typep specializer 'eql-specializer))

(defun eql-specializer-object (specializer)
  (slot-value specializer 'object))

(defun specializer-class (specializer)
  "Return the class that corresponds to the specializer.
I.e. either the specializer itself if it's a class, or the
class of an eql-specializer's object."
  (if (eql-specializer-p specializer)
      (class-of (eql-specializer-object specializer))
    specializer))

(defun sub-specializer-p (c1 c2 carg)
  "Is class c1 more specific than class c2 with respect to class carg?
I.e. does c1 appear before c2 in c-arg's class-precedence-list?"
  (or (eql-specializer-p c1)
      (let ((cpl (class-precedence-list carg)))
	(not (null (find c2 (cdr (member c1 cpl))))))))

(defun specialized-p (object specializer)
  "Does object match specializer?"
  (if (eql-specializer-p specializer)
      (eql object (eql-specializer-object specializer))
    (typep object specializer)))


;;; compute-applicable-methods-using-classes

(defun std-compute-applicable-methods-using-classes (gf classes)
  #+ignore (warn "camuc of: ~S for classes ~S"
		 (funobj-name gf)
		 (mapcar (lambda (c)
			   (standard-instance-access c 0))
			 classes))
  (flet ((method-specific< (method1 method2)
	   (do ((cspec1 (method-specializers method1) (cdr cspec1))
		(cspec2 (method-specializers method2) (cdr cspec2))
		(cargs classes (cdr cargs)))
	       ((endp cspec1) nil)
	     (let ((spec1 (car cspec1))
		   (spec2 (car cspec2)))
	       (unless (eq spec1 spec2)
		 (return (sub-specializer-p spec1 spec2 (car cargs))))))))
    (declare (dynamic-extent method-specific<))
    (let ((applicable-methods nil))
      (block hopefully-no-eql-specializers
	(dolist (method (generic-function-methods gf))
	  (when (do ((class-cons classes (cdr class-cons))
		     (speci-cons (method-specializers method) (cdr speci-cons)))
		    ((endp speci-cons) t)
		  (assert class-cons (classes)
		    "Too few arguments/classes for ~W: ~W" gf classes)
		  (let ((class (car class-cons))
			(specializer (car speci-cons)))
		    (cond
		     ((not (eql-specializer-p specializer))
		      (unless (subclassp class specializer)
			(return nil)))
		     ((not (typep (eql-specializer-object specializer)
				  class))
		      (return nil))
		     (t (return-from hopefully-no-eql-specializers (values nil nil))))))
	    #+ignore (loop for class in classes
			 as specializer in (method-specializers method)
			 finally (return t)
			 do (cond
			     ((not (eql-specializer-p specializer))
			      (unless (subclassp class specializer)
				(return nil)))
			     ((not (typep (eql-specializer-object specializer)
					  class))
			      (return nil))
			     (t (return-from hopefully-no-eql-specializers (values nil nil)))))
	    (setf applicable-methods
	      (merge 'list (list method) applicable-methods #'method-specific<))))
	(values applicable-methods t)))))

(defmethod compute-applicable-methods-using-classes ((gf standard-generic-function) classes)
  (std-compute-applicable-methods-using-classes gf classes))

(defmethod compute-applicable-methods ((gf standard-generic-function) arguments)
  (flet ((method-specific< (method1 method2)
	   (do ((cspec1 (method-specializers method1) (cdr cspec1))
		(cspec2 (method-specializers method2) (cdr cspec2))
		(args arguments (cdr args)))
	       ((endp cspec1) nil)
	     (let ((spec1 (car cspec1))
		   (spec2 (car cspec2)))
	       (unless (eq spec1 spec2)
		 (return (sub-specializer-p spec1 spec2 (class-of (car args)))))))))
    (declare (dynamic-extent method-specific<))
    (let ((applicable-methods nil))
      (dolist (method (generic-function-methods gf))
	(when (do ((a arguments (cdr a))
		   (s (method-specializers method) (cdr s)))
		  ((endp s) t)
		(let ((argument (car a))
		      (specializer (car s)))
		  (unless (if (eql-specializer-p specializer)
			      (eql (eql-specializer-object specializer)
				   argument)
			    (typep argument specializer))
		    (return nil))))
	  #+ignore (loop for argument in arguments
		       as specializer in (method-specializers method)
		       always (if (eql-specializer-p specializer)
				  (eql (eql-specializer-object specializer)
				       argument)
				(typep argument specializer)))
	  (setf applicable-methods
	    (merge 'list (list method) applicable-methods #'method-specific<))))
      applicable-methods)))

;;; Effective methods

(defun std-compute-effective-method-function (gf methods)
  (declare (ignore gf))
  ;; (warn "comp-eff-mf for ~S" (funobj-name gf))
  (list 'standard-combine methods))

(defmethod compute-effective-method ((generic-function standard-generic-function)
				     (method-combination standard-method-combination)
				     methods)
  (std-compute-effective-method-function generic-function methods))

(defun gf-nonstandard-specialized-p (gf standard-method &rest args)
  "Is gf applied to args specialized to something other than standard-method?"
  (declare (dynamic-extent args))
  (dolist (method (generic-function-methods gf))
    (unless (or (eq method standard-method)
		(not (every #'specialized-p args (method-specializers method))))
      (return t))))

(defun std-compile-effective-method (methods specializers args)
  "Compute the effective method for methods given standard method-combination."
  (let ((primaries (remove-if-not 'primary-method-p methods))
	(around (find-if 'around-method-p methods)))
    (cond
     ((null primaries)
      (error "Can't compile effective method with no primary method."))
     (around 
      (let ((next-emfun (std-compile-effective-method (remove around methods) specializers)))
	(make-emfun around next-emfun)))
     (t (let ((befores (remove-if-not 'before-method-p methods))
	      (reverse-afters (reverse (remove-if-not 'after-method-p methods)))
	      (primary-method (car primaries)))
	  (cond
	   ((and (null befores)
		 (null reverse-afters))
	    (cond
	     ((and (typep primary-method 'standard-reader-method)
		   ;; May we shortcut this reader method?
		   (or (not *standard-slot-value-using-class*) ; still bootstrapping..
		       (not (let ((object (car args)))
			      (gf-nonstandard-specialized-p #'slot-value-using-class
							    (if (typep object 'funcallable-standard-class)
								*standard-gf-slot-value-using-class*
							      *standard-slot-value-using-class*)
							    (class-of object) object
							    (accessor-method-slot-definition primary-method))))))
	      (compute-effective-slot-reader (specializer-class (car specializers))
					     (accessor-method-slot-definition primary-method)))
	     ((and (typep primary-method 'standard-writer-method)
		   ;; May we shortcut this writer method?
		   (or (not *standard-setf-slot-value-using-class*) ; still bootstrapping..
		       (not (let ((value (car args))
				  (object (cadr args)))
			      (gf-nonstandard-specialized-p #'slot-value-using-class
							    (if (typep object 'funcallable-standard-class)
								*standard-gf-setf-slot-value-using-class*
							      *standard-setf-slot-value-using-class*)
							    value (class-of object) object
							    (accessor-method-slot-definition primary-method))))))
	      (compute-effective-slot-writer (specializer-class (cadr specializers))
					     (accessor-method-slot-definition primary-method)))
	     (t (compute-primary-emfun primaries))))
	   ((null reverse-afters)
	    (let ((emfun (compute-primary-emfun primaries))
		  (before-emfuns (mapcar #'method-function befores)))
	      (lambda (&rest args)
		(declare (dynamic-extent args))
		(dolist (before before-emfuns)
		  (apply before args))
		(apply emfun args))))
	   (t (let ((emfun (compute-primary-emfun primaries))
		    (before-emfuns (mapcar #'method-function befores))
		    (after-emfuns (mapcar #'method-function reverse-afters)))
		(lambda (&rest args)
		  (declare (dynamic-extent args))
		  (dolist (before before-emfuns)
		    (apply before args))
		  (multiple-value-prog1
		      (apply emfun args)
		    (dolist (after after-emfuns)
		      (apply after args))))))))))))

(defmethod compile-effective-method-form ((gf standard-generic-function) form specializers args)
  (cond
   ((and (listp form) (eq 'standard-combine (car form)))
    (std-compile-effective-method (cadr form) specializers args))
   (t (error "Don't know how to compile effective method-form ~S." form))))

;;;;

(defmethod find-method ((generic-function standard-generic-function)
			qualifiers specializers &optional errorp)
  (dolist (method (generic-function-methods generic-function)
	    (if errorp
		(error "No method for ~S is matching specializers ~S and qualifiers ~S."
		       generic-function specializers qualifiers)
	      nil))
    (when (and (equal specializers (method-specializers method))
	       (equal qualifiers (method-qualifiers method)))
      (return method))))

;;;;;

(defgeneric no-applicable-method (generic-function &rest function-arguments)
  (:documentation
   "The generic function no-applicable-method is called when a generic function is invoked and no method on
that generic function is applicable. The default method signals an error.

The generic function no-applicable-method is not intended to be called by programmers. Programmers may
write methods for it."))
   
(defmethod no-applicable-method ((generic-function t) &rest function-arguments)
  (declare (dynamic-extent function-arguments))
  (error "No applicable method for ~S with arguments ~S of classes ~S."
	 generic-function
	 function-arguments
	 (mapcar (lambda (c) (class-name (class-of c)))
		 function-arguments)))

(defmethod no-next-method ((generic-function standard-generic-function) (method standard-method) &rest args)
  (declare (dynamic-extent args)
	   (ignore args))
  (error "No next method for method ~S of ~S." method generic-function))

(defun slot-unbound-trampoline (instance slot-location)
  "Invoke slot-unbound for instance and slot-location."
  (let* ((class (class-of instance)))
    (dolist (slot (class-slots class) (error "No slot at ~D in ~S." slot-location class))
      (when (eql slot-location (slot-definition-location slot))
	(return (slot-unbound class instance (slot-definition-name slot)))))))

(defgeneric slot-unbound (class instance slot-name)
  (:documentation
   "The generic function slot-unbound is called when an unbound slot is read
in an instance whose metaclass is standard-class."))

(defmethod slot-unbound ((class t) instance slot-name)
  (error "The slot ~S is unbound in ~S." slot-name instance))

(defmethod slot-missing ((class t) object slot-name operation &optional new-value)
  (declare (ignore operation new-value)
	   (optimize (debug 3) (speed 0) (space 0)))
  (error "The slot named ~S is missing from the object ~S of class ~S."
	 slot-name object class))

;;;;

(defgeneric compute-discrimination-function (generic-function)
  (:documentation
   "This generic function is called to determine the discriminating function for a generic function."))

#+ignore
(defmethod compute-discrimination-function ((gf standard-generic-function))
  #'initial-discriminating-function)

;;;

(defmethod print-object ((object t) stream)
  (let ((*never-use-print-object* t))
    (write object :stream stream)))

(defmethod print-object ((object class) stream)
  (print-unreadable-object (object stream :identity nil)
    (format stream "~W ~W" (class-name (class-of object)) (class-name object)))
  object)

(defmethod print-object ((object standard-object) stream)
  (print-unreadable-object (object stream :identity t)
    (write (class-name (class-of object))
	   :stream stream))
  object)

(defmethod print-object ((object structure-object) stream)
  (let* ((class (class-of object))
	 (slots (mapcar #'car (slot-value class 'slots)))
	 (position 0))
    (format stream "#S(~S" (class-name class))
    (dolist (slot slots)
      (format stream " :~A ~S" slot (structure-ref object position))
      (incf position))
    (write-string ")" stream))
  object)

(defmethod print-object ((object method) stream)
  (flet ((write-specializer (specializer stream)
	   (if (eql-specializer-p specializer)
	       (format stream "(~S ~S)" 'eql (eql-specializer-object specializer))
	     (write (class-name specializer) :stream stream))))
    (print-unreadable-object (object stream :type t)
      (format stream " ~S~{ ~A~} ("
	      (generic-function-name (method-generic-function object))
	      (method-qualifiers object))
      (write-specializer (first (method-specializers object)) stream)
      (dolist (method (rest (method-specializers object)))
	(write-char #\space stream)
	(write-specializer method stream))
      (write-string ")" stream)))
  object)

(defmethod print-object ((x run-time-context) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream " ~S" (%run-time-context-slot 'name x)))
  x)

;;;


;;; BOOTSTRAPPING

;;;(defmethod slot-value-using-class ((class standard-effective-slot-definition) object slot-name)
;;;  (let ((location (get 'standard-effective-slot-definition slot-name

(defun bootstrap-slot-definition-access (slot slot-name)
  (let* ((class (class-of slot))
	 (class-name (dolist (c '(standard-effective-slot-definition
				  standard-direct-slot-definition
				  standard-class
				  standard-reader-method
				  standard-writer-method
				  (funcallable-standard-class . standard-class)
				  standard-generic-function)
			       (error "Unknown class: ~Z for ~S." class slot-name))
		       (when (eq class (find-class (if (consp c) (car c) c)))
			 (return (if (consp c) (cdr c) c)))))
	 (location (get class-name slot-name)))
    ;; (warn "access ~S of ~S at ~S" slot-name class-name location)
    (assert location)
    (let ((x (standard-instance-access slot location)))
      (if (eq x (load-global-constant unbound-value))
	  (error "The slot ~S is unbound in the ~S ~Z." slot-name class-name slot)
	x))))

(defun bootstrap-class-name (class)
  (standard-instance-access class 0))

(defvar *bsml-context* nil)

(defun bootstrap-slow-method-lookup (&rest args)
  (declare (dynamic-extent args))
  (let ((p (position args *bsml-context* :test 'equal)))
    (when p
      (let ((*never-use-print-object* t)
	    (*print-length* 4)
	    (*print-level* 2))
	(error "Recursive circle:~%:::~S~v{~&::~S~}" args nil *sml-context*))))
  (let ((*bsml-context* (cons args *bsml-context*)))
    (apply 'do-bootstrap-slow-method-lookup args)))

(defun do-bootstrap-slow-method-lookup (gf args specializers)
  (declare (dynamic-extent args classes))
  ;; args and classes are stack-allocated by std-discriminating-function.
  (multiple-value-bind (applicable-methods using-class-of-ok)
      (compute-applicable-methods-using-classes gf (map-into specializers #'class-of args))
    (cond
     ((not using-class-of-ok)
      (error "camuc failed during bootstrap."))
     ((null applicable-methods)
      (apply 'no-applicable-method gf args))      
     (t (let ((emfun (std-compile-effective-method applicable-methods specializers args)))
	  (if (not emfun)
	      (error "nil emfun-form?")	; not sure what this means yet..
	    (progn
	      (check-type emfun function)
	      (values emfun specializers)
	      #+ignore ((pushnew (cons (copy-list specializers)
				       emfun)
				 (std-gf-classes-to-emf-table gf)
				 :test 'equal)
			(apply emfun args)))))))))

(defun clos-bootstrap ()
  (when (get 'clos-bootstrap 'have-bootstrapped)
    (warn "CLOS was already bootstrapped: ~S"
	  (get 'clos-bootstrap 'have-bootstrapped)))
  (setf (get 'clos-bootstrap 'have-bootstrapped) :in-progress)
  #+ignore
  (setf (runtime-context-slot 'the-class-t) (gethash 't *class-table*)
	(runtime-context-slot 'the-class-null) (gethash 'null *class-table*)
	(runtime-context-slot 'the-class-symbol) (gethash 'symbol *class-table*)
	(runtime-context-slot 'the-class-cons) (gethash 'cons *class-table*))
  (let ((real-camuc #'compute-applicable-methods-using-classes)
	(real-class-slots #'class-slots)
	(real-class-precedence-list #'class-precedence-list)
	(real-method-specializers #'method-specializers)
	(real-method-qualifiers #'method-qualifiers)
	(real-method-function #'method-function)
	(real-sd-name #'slot-definition-name)
	(real-sd-location #'slot-definition-location)
	(real-gf-methods #'generic-function-methods)
	(real-gf-mc #'generic-function-method-combination)
	(real-amsd #'accessor-method-slot-definition))
    (with-alternative-fdefinitions
	((slow-method-lookup #'bootstrap-slow-method-lookup)
	 (slot-definition-name
	  (lambda (slot) (bootstrap-slot-definition-access slot 'name)))
	 (slot-definition-location
	  (lambda (slot) (bootstrap-slot-definition-access slot 'location)))
	 (class-slots
	  (lambda (class) (bootstrap-slot-definition-access class 'effective-slots)))
	 (class-precedence-list
	  (lambda (class)
	    (std-slot-value class 'class-precedence-list)))
	 (method-specializers
	  (lambda (m) (std-slot-value m 'specializers)))
	 (method-qualifiers
	  (lambda (m) (std-slot-value m 'qualifiers)))
	 (method-function
	  (lambda (m) (std-slot-value m 'function)))
	 (generic-function-methods
	  (lambda (gf) (std-gf-slot-value gf 'methods)))
	 (generic-function-method-combination
	  (lambda (gf) nil))
	 (accessor-method-slot-definition
	  (lambda (method)
	    (std-slot-value method 'slot-definition)))
	 (compute-applicable-methods-using-classes
	  (lambda (gf classes)
	    ;; (warn "camuc of: ~S" (funobj-name gf))
	    (with-alternative-fdefinitions
		((method-function
		  (lambda (method)
		    (std-slot-value method 'function)))
		 (method-specializers
		  (lambda (method)
		    (std-slot-value method 'specializers)))
		 )
	      (case (funobj-name gf)
		((compute-applicable-methods-using-classes)
		 (std-compute-applicable-methods-using-classes gf classes))
		(t (funcall real-camuc gf classes)))))))
      ;; (warn "real-camuc: ~S" real-camuc)
      ;; Just rock the boat enough that certain standard-readers gets cached.
      (let ((slot (car (funcall real-class-slots (find-class 'standard-class)))))
	(funcall real-sd-name slot)
	(funcall real-sd-location slot))
      (assert (eq *the-class-standard-class*
		  (find-class 'standard-class)))
      (assert (eq *the-class-standard-class*
		  (car (funcall real-class-precedence-list (find-class 'standard-class)))))
      (funcall real-class-slots (find-class 'standard-generic-function))
      (funcall real-class-slots (find-class 'standard-class))
      (let ((m (car (funcall real-gf-methods real-method-specializers))))
	(assert m)
	(funcall real-method-specializers m)
	(funcall real-method-qualifiers m)
	(funcall real-method-function m))
      (let ((m (car (funcall real-gf-methods #'make-instance))))
	(funcall real-method-specializers m)
	(funcall real-method-qualifiers m)
	(funcall real-method-function m))
      (funcall real-gf-mc #'make-instance)
      (let ((emfun-form
	     (compute-effective-method #'make-instance
				       *the-standard-method-combination*
				       (generic-function-methods #'make-instance))))
	(compile-effective-method-form #'make-instance
				       emfun-form
				       (list (find-class 'symbol))
				       (list 'symbol)))
      (funcall real-amsd (car (generic-function-methods real-amsd)))
      (funcall real-sd-name (car (slot-value (find-class 'class) 'direct-slots)))
      (setf *standard-slot-value-using-class*
	(find-method #'slot-value-using-class nil
		     (list (find-class 'standard-class)
			   (find-class t)
			   (find-class 'standard-effective-slot-definition))))
      (setf *standard-setf-slot-value-using-class*
	(find-method #'(setf slot-value-using-class) nil
		     (list  (find-class t)
			    (find-class 'standard-class)
			    (find-class t)
			    (find-class 'standard-effective-slot-definition))))
      (setf *standard-gf-slot-value-using-class*
	(find-method #'slot-value-using-class nil
		     (list (find-class 'funcallable-standard-class)
			   (find-class t)
			   (find-class 'standard-effective-slot-definition))))
      (setf *standard-gf-setf-slot-value-using-class*
	(find-method #'(setf slot-value-using-class) nil
		     (list  (find-class t)
			    (find-class 'funcallable-standard-class)
			    (find-class t)
			    (find-class 'standard-effective-slot-definition))))
      (assert *standard-slot-value-using-class*)
      (assert *standard-setf-slot-value-using-class*)
      (assert *standard-gf-slot-value-using-class*)
      (assert *standard-gf-setf-slot-value-using-class*)
      (when (eq muerte::*never-use-print-object* :after-clos-bootstrapped)
	(setf muerte::*never-use-print-object* nil))
      (setf (get 'clos-bootstrap 'have-bootstrapped) t)
      (values))))


