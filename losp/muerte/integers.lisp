;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      integers.lisp
;;;; Description:   Arithmetics.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Wed Nov  8 18:44:57 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(require :muerte/basic-macros)
(require :muerte/typep)
(provide :muerte/integers)

(in-package muerte)

(defconstant most-positive-fixnum #.movitz::+movitz-most-positive-fixnum+)
(defconstant most-negative-fixnum #.movitz::+movitz-most-negative-fixnum+)

(deftype positive-fixnum ()
  `(integer 0 ,movitz:+movitz-most-positive-fixnum+))

(deftype positive-bignum ()
  `(integer ,(1+ movitz:+movitz-most-positive-fixnum+) *))

(deftype negative-fixnum ()
  `(integer ,movitz:+movitz-most-negative-fixnum+ -1))

(defmacro number-double-dispatch ((x y) &rest clauses)
  `(let ((x ,x) (y ,y))
     (cond ,@(loop for ((x-type y-type) . then-body) in clauses
		 collect `((and (typep x ',x-type) (typep y ',y-type))
			   ,@then-body))
	   (t (error "Not numbers: ~S or ~S." x y)))))

(defun fixnump (x)
  (typep x 'fixnum))

(define-compiler-macro evenp (x)
  `(with-inline-assembly (:returns :boolean-zf=1)
     (:compile-form (:result-mode :eax) ,x)
     (:call-global-constant unbox-u32)
     (:testb 1 :cl)))

(defun evenp (x)
  (evenp x))

(define-compiler-macro oddp (x)
  `(with-inline-assembly (:returns :boolean-zf=0)
     (:compile-form (:result-mode :eax) ,x)
     (:call-global-constant unbox-u32)
     (:testb 1 :cl)))

(defun oddp (x)
  (oddp x))

;;; Types

(define-typep integer (x &optional (min '*) (max '*))
  (and (typep x 'integer)
       (or (eq min '*) (<= min x))
       (or (eq max '*) (<= x max))))

(deftype signed-byte (&optional (size '*))
  (cond
   ((eq size '*)
    'integer)
   ((typep size '(integer 1 *))
    (list 'integer
	  (- (ash 1 (1- size)))
	  (1- (ash 1 (1- size)))))
   (t (error "Illegal size for signed-byte."))))

(deftype unsigned-byte (&optional (size '*))
  (cond
   ((eq size '*)
    '(integer 0))
   ((typep size '(integer 1 *))
    (list 'integer 0 (1- (ash 1 size))))
   (t (error "Illegal size for unsigned-byte."))))

(define-simple-typep (bit bitp) (x)
  (or (eq x 0) (eq x 1)))

;;; Addition

(define-compiler-macro + (&whole form &rest operands &environment env)
  (case (length operands)
    (0 0)
    (1 (first operands))
    #+ignore (2 `(+%2op ,(first operands) ,(second operands)))
    (2 `(let ((x ,(first operands))
	      (y ,(second operands)))
	  (++%2op x y)))
    (t (let ((operands
	      (loop for operand in operands
		  if (movitz:movitz-constantp operand env)
		  sum (movitz:movitz-eval operand env)
		  into constant-term
		  else collect operand
		  into non-constant-operands
		  finally (return (if (zerop constant-term)
				      non-constant-operands
				    (cons constant-term non-constant-operands))))))
	 `(+ (+ ,(first operands) ,(second operands)) ,@(cddr operands))))))

(defun + (&rest terms)
  (declare (without-check-stack-limit))
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (macrolet
	  ((do-it ()
	     `(number-double-dispatch (x y)
		((fixnum fixnum)
		 (with-inline-assembly (:returns :eax)
		   (:compile-form (:result-mode :eax) x)
		   (:compile-form (:result-mode :ebx) y)
		   (:addl :ebx :eax)
		   (:jo '(:sub-program (fix-fix-overflow)
			  (:movl :eax :ecx)
			  (:jns 'fix-fix-negative)
			  (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
			  (:call-global-constant box-u32-ecx)
			  (:jmp 'fix-fix-ok)
			  fix-fix-negative
			  (:jz 'fix-double-negative)
			  (:negl :ecx)
			  (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
			  (:call-global-constant box-u32-ecx)
			  (:movl ,(dpb 1 (byte 16 16)
				   (movitz:tag :bignum #xff))
			   (:eax ,movitz:+other-type-offset+))
			  (:jmp 'fix-fix-ok)
			  fix-double-negative
			  (:compile-form (:result-mode :eax)
			   ,(* 2 movitz:+movitz-most-negative-fixnum+))
			  (:jmp 'fix-fix-ok)))
		  fix-fix-ok))
		((positive-bignum positive-fixnum)
		 (funcall '+ y x))
		((positive-fixnum positive-bignum)
		 (with-inline-assembly (:returns :eax :labels (retry-not-size1
							       not-size1
							       copy-bignum-loop
							       add-bignum-loop
							       add-bignum-done
							       no-expansion
							       pfix-pbig-done))
		   (:compile-two-forms (:eax :ebx) y x)
		   (:testl :ebx :ebx)
		   (:jz 'pfix-pbig-done)
		   (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)) :ecx)
		   (:cmpl ,movitz:+movitz-fixnum-factor+ :ecx)
		   (:jne 'not-size1)
		   (:compile-form (:result-mode :ecx) x)
		   (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:addl (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)) :ecx)
		   (:jc 'retry-not-size1)
		   (:call-global-constant box-u32-ecx)
		   (:jmp 'pfix-pbig-done)
		  retry-not-size1
		   (:compile-form (:result-mode :eax) y)
		   (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)) :ecx)
		  not-size1
		   (:declare-label-set retry-jumper (retry-not-size1))
		   (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		   (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
				      'retry-jumper)
				    (:edi (:edi-offset atomically-status))))
		   (:leal ((:ecx 1) ,(* 2 movitz:+movitz-fixnum-factor+))
			  :eax)		; Number of words
		   (:call-global-constant get-cons-pointer)
		   (:load-lexical (:lexical-binding y) :ebx) ; bignum
		   (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)) :ecx)
		   (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			  :edx)
		   (:movl 0 (:eax :edx ,movitz:+other-type-offset+)) ; MSB
		  copy-bignum-loop
		   (:subl ,movitz:+movitz-fixnum-factor+ :edx)
		   (:movl (:ebx :edx ,movitz:+other-type-offset+) :ecx)
		   (:movl :ecx (:eax :edx ,movitz:+other-type-offset+))
		   (:jnz 'copy-bignum-loop)

		   (:load-lexical (:lexical-binding x) :ecx)
		   (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:xorl :ebx :ebx)
		   (:addl :ecx (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		   (:jnc 'add-bignum-done)
		  add-bignum-loop
		   (:addl 4 :ebx)
		   (:addl 1 (:eax :ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		   (:jc 'add-bignum-loop)
		  add-bignum-done
		   (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			    :ecx)
		   (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			  :ecx)
		   (:cmpl 0 (:eax :ecx ,(+ -4 (bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))
		   (:je 'no-expansion)
		   (:addl #x40000 (:eax ,movitz:+other-type-offset+))
		   (:addl ,movitz:+movitz-fixnum-factor+ :ecx)
		  no-expansion
		   (:call-global-constant cons-commit)
		   (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				    (:edi (:edi-offset atomically-status))))
		   
		  pfix-pbig-done))
		((positive-bignum negative-fixnum)
		 (+ y x))
		((negative-fixnum positive-bignum)
		 (with-inline-assembly (:returns :eax :labels (retry-not-size1
							       not-size1
							       copy-bignum-loop
							       add-bignum-loop
							       add-bignum-done
							       no-expansion
							       pfix-pbig-done))
		   (:compile-two-forms (:eax :ebx) y x)
		   (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)) :ecx)
		   (:cmpl 4 :ecx)
		   (:jne 'not-size1)
		   (:compile-form (:result-mode :ecx) x)
		   (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:addl (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)) :ecx)
		   (:call-global-constant box-u32-ecx)
		   (:jmp 'pfix-pbig-done)
		  retry-not-size1
		   (:compile-form (:result-mode :eax) y)
		   (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)) :ecx)
		  not-size1
		   (:declare-label-set retry-jumper (retry-not-size1))
		   (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		   (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
				      'retry-jumper)
				    (:edi (:edi-offset atomically-status))))
		   (:leal ((:ecx 1) ,(* 1 movitz:+movitz-fixnum-factor+))
			  :eax)		; Number of words
		   (:call-global-constant get-cons-pointer)
		   (:load-lexical (:lexical-binding y) :ebx) ; bignum
		   (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)) :ecx)
		   (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			  :edx)
		  copy-bignum-loop
		   (:subl ,movitz:+movitz-fixnum-factor+ :edx)
		   (:movl (:ebx :edx ,movitz:+other-type-offset+) :ecx)
		   (:movl :ecx (:eax :edx ,movitz:+other-type-offset+))
		   (:jnz 'copy-bignum-loop)

		   (:load-lexical (:lexical-binding x) :ecx)
		   (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:xorl :ebx :ebx)	; counter
		   (:negl :ecx)
		   (:subl :ecx (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		   (:jnc 'add-bignum-done)
		  add-bignum-loop
		   (:addl 4 :ebx)
		   (:subl 1 (:eax :ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		   (:jc 'add-bignum-loop)
		  add-bignum-done
		   (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			    :ecx)
		   (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			  :ecx)		; result bignum word-size
		   (:cmpl 0 (:eax :ecx ,(+ -8 (bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))
		   (:jne 'no-expansion)
		   (:subl #x40000 (:eax ,movitz:+other-type-offset+))
		   (:subl ,movitz:+movitz-fixnum-factor+ :ecx)
		  no-expansion
		   (:call-global-constant cons-commit)
		   (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				    (:edi (:edi-offset atomically-status))))
		   
		  pfix-pbig-done))
		((positive-bignum positive-bignum)
		 (if (< (%bignum-bigits y) (%bignum-bigits x))
		     (+ y x)
		   ;; Assume x is smallest.
		   (with-inline-assembly (:returns :eax :labels (retry-not-size1
								 not-size1
								 copy-bignum-loop
								 add-bignum-loop
								 add-bignum-done
								 no-expansion
								 pfix-pbig-done))
		     (:compile-two-forms (:eax :ebx) y x)
		     (:testl :ebx :ebx)
		     (:jz 'pfix-pbig-done)
		     (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)) :ecx)
		     (:cmpl ,movitz:+movitz-fixnum-factor+ :ecx)
		     (:jne 'not-size1)
		     (:movl (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)) :ecx)
		     (:addl (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)) :ecx)
		     (:jc 'retry-not-size1)
		     (:call-global-constant box-u32-ecx)
		     (:jmp 'pfix-pbig-done)
		    retry-not-size1
		     (:compile-form (:result-mode :eax) y)
		     (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)) :ecx)
		    not-size1
		     (:declare-label-set retry-jumper (retry-not-size1))
		     (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		     (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
					'retry-jumper)
				      (:edi (:edi-offset atomically-status))))
		     (:leal ((:ecx 1) ,(* 2 movitz:+movitz-fixnum-factor+))
			    :eax)	; Number of words
		     (:call-global-constant get-cons-pointer)
		     (:load-lexical (:lexical-binding y) :ebx) ; bignum
		     (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)) :ecx)
		     (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			    :edx)
		     (:movl 0 (:eax :edx ,movitz:+other-type-offset+)) ; MSB
		    copy-bignum-loop
		     (:subl ,movitz:+movitz-fixnum-factor+ :edx)
		     (:movl (:ebx :edx ,movitz:+other-type-offset+) :ecx)
		     (:movl :ecx (:eax :edx ,movitz:+other-type-offset+))
		     (:jnz 'copy-bignum-loop)

		     (:load-lexical (:lexical-binding x) :ebx)
		     (:xorl :edx :edx)	; counter
		     (:xorl :ecx :ecx)	; Carry
		    add-bignum-loop
		     (:cmpw :dx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:jbe '(:sub-program (zero-padding-loop)
			     (:addl :ecx (:eax :edx ,(bt:slot-offset 'movitz::movitz-bignum
						      'movitz::bigit0)))
			     (:sbbl :ecx :ecx)
			     (:negl :ecx) ; ECX = Add's Carry.
			     (:addl 4 :edx)
			     (:cmpw :dx (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
			     (:jae 'zero-padding-loop)
			     (:jmp 'add-bignum-done)))
		     (:addl (:ebx :edx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))
			    :ecx)
		     (:jc '(:sub-program (term1-carry)
			    ;; The digit + carry carried over, ECX = 0
			    (:addl 1 :ecx)
			    (:addl 4 :edx)
			    (:cmpw :dx (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
			    (:jae 'add-bignum-loop)
			    (:jmp 'add-bignum-done)))
		     (:addl :ecx (:eax :edx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		     (:sbbl :ecx :ecx)
		     (:negl :ecx)	; ECX = Add's Carry.
		     (:addl 4 :edx)
		     (:cmpw :dx (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:jae 'add-bignum-loop)
		    add-bignum-done
		     (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			      :ecx)
		     (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			    :ecx)
		     (:cmpl 0 (:eax :ecx ,(+ -4 (bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))
		     (:je 'no-expansion)
		     (:addl #x40000 (:eax ,movitz:+other-type-offset+))
		     (:addl ,movitz:+movitz-fixnum-factor+ :ecx)
		    no-expansion
		     (:call-global-constant cons-commit)
		     (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				      (:edi (:edi-offset atomically-status))))
		   
		    pfix-pbig-done)
		   ))
		(((integer * -1) (integer 0 *))
		 (- y (- x)))
		(((integer 0 *) (integer * -1))
		 (- x (- y)))
		(((integer * -1) (integer * -1))
		 (+ (- x) (- y)))
		)))
	(do-it)))
   (t (&rest terms)
      (declare (dynamic-extent terms))
      (if (null terms)
	  0
	(reduce #'+ terms)))))

(defun 1+ (number)
  (+ 1 number))

(define-compiler-macro 1+ (number)
  `(+ 1 ,number))

(defun 1- (number)
  (+ -1 number))

(define-compiler-macro 1- (number)
  `(+ -1 ,number))

(define-modify-macro incf (&optional (delta-form 1)) +)

;;; Subtraction

(define-compiler-macro - (&whole form &rest operands &environment env)
  (case (length operands)
    (0 0)
    (1 (let ((x (first operands)))
	 (if (movitz:movitz-constantp x env)
	     (- (movitz:movitz-eval x env))
	   form)))
    (2 (let ((minuend (first operands))
	     (subtrahend (second operands)))
	 (cond
	  ((movitz:movitz-constantp subtrahend env)
	   `(+ ,minuend ,(- (movitz:movitz-eval subtrahend env))))
	  (t form))))
    (t `(- ,(first operands) (+ ,@(rest operands))))))

(defun - (minuend &rest subtrahends)
  (declare (dynamic-extent subtrahends))
  (numargs-case
   (1 (x)
      (macrolet
	  ((do-it ()
	     `(with-inline-assembly (:returns :eax)
		(:compile-form (:result-mode :eax) x)
		(:testb ,movitz:+movitz-fixnum-zmask+ :al)
		(:jnz '(:sub-program (not-fixnum)
			(:leal (:eax ,(- (movitz:tag :other))) :ecx)
			(:testb 7 :cl)
			(:jnz '(:sub-program (not-a-number)
				(:compile-form (:result-mode :ignore)
				 (error 'type-error :expected-type 'number :datum x))))
			(:movl (:eax ,movitz:+other-type-offset+) :ecx)
			(:cmpb ,(movitz:tag :bignum) :cl)
			(:jne 'not-a-number)
			(:cmpl ,(dpb 4 (byte 16 16) (movitz:tag :bignum 0)) :ecx)
			(:jne 'not-most-negative-fixnum)
			(:cmpl ,(- most-negative-fixnum)
			 (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
			(:jne 'not-most-negative-fixnum)
			(:movl ,(ldb (byte 32 0)
				 (* most-negative-fixnum movitz::+movitz-fixnum-factor+))
			 :eax)
			(:jmp 'fix-ok)
			not-most-negative-fixnum
			(:compile-form (:result-mode :eax)
			 (copy-bignum x))
			(:notb (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::sign)))
			(:jmp 'fix-ok)))
		(:negl :eax)
		(:jo '(:sub-program (fix-overflow)
		       (:compile-form (:result-mode :eax)
			,(1+ movitz:+movitz-most-positive-fixnum+))
		       (:jmp 'fix-ok)))
	       fix-ok
		)))
	(do-it)))
   (2 (minuend subtrahend)
      (macrolet
	  ((do-it ()
	     `(number-double-dispatch (minuend subtrahend)
		((t (eql 0))
		 minuend)
		(((eql 0) t)
		 (- subtrahend))
		((fixnum fixnum)
		 (with-inline-assembly (:returns :eax :side-effects nil)
		   (:compile-two-forms (:eax :ebx) minuend subtrahend)
		   (:subl :ebx :eax)
		   (:into)))
		((positive-bignum fixnum)
		 (+ (- subtrahend) minuend))
		((fixnum positive-bignum)
		 (- (+ (- minuend) subtrahend)))
		((positive-bignum positive-bignum)
		 (cond
		  ((= minuend subtrahend)
		   0)
		  ((< minuend subtrahend)
		   (let ((x (- subtrahend minuend)))
		     (when (typep x 'bignum)
		       (setf (memref x ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::sign)
				     0 :unsigned-byte8)
			 #xff))
		     x))
		  (t (%bignum-canonicalize
		      (with-inline-assembly (:returns :eax)
			(:compile-two-forms (:eax :ebx) (copy-bignum minuend) subtrahend)
			(:xorl :edx :edx) ; counter
			(:xorl :ecx :ecx) ; carry
		       sub-loop
			(:addl (:ebx :edx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))
			       :ecx)
			(:jc '(:sub-program (carry-overflow)
			       ;; Just propagate carry
			       (:addl 1 :ecx)
			       (:addl 4 :edx)
			       (:cmpw :dx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
			       (:jne 'sub-loop)
			       (:jmp 'bignum-sub-done)))
			(:subl :ecx
			       (:eax :edx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
			(:sbbl :ecx :ecx)
			(:negl :ecx)
			(:addl 4 :edx)
			(:cmpw :dx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
			(:jne 'sub-loop)
			(:subl :ecx
			       (:eax :edx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
			(:jc '(:sub-program (should-not-happen)
			       (:int 107)))
		       bignum-sub-done
			)))))
		(((integer 0 *) (integer * -1))
		 (+ minuend (- subtrahend)))
		(((integer * -1) (integer 0 *))
		 (- (+ (- minuend) subtrahend)))
		(((integer * -1) (integer * -1))
		 (+ minuend (- subtrahend)))
		)))
	(do-it)))
   (t (minuend &rest subtrahends)
      (declare (dynamic-extent subtrahends))
      (if subtrahends
	  (reduce #'- subtrahends :initial-value minuend)
	(- 0 minuend)))))
    
(define-modify-macro decf (&optional (delta-form 1)) -)

;;; Comparison

(define-primitive-function fast-compare-two-reals (n1 n2)
  "Compare two numbers (i.e. set EFLAGS accordingly)."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :nothing) ; unspecified
	    (:testb ,movitz::+movitz-fixnum-zmask+ :al)
	    (:jnz 'n1-not-fixnum)
	    (:testb ,movitz::+movitz-fixnum-zmask+ :bl)
	    (:jnz 'n2-not-fixnum-but-n1-is)
	    (:cmpl :ebx :eax)		; both were fixnum
	    (:ret)
	   n1-not-fixnum		; but we don't know about n2
	    (:testb ,movitz::+movitz-fixnum-zmask+ :bl)
	    (:jnz 'neither-is-fixnum)
	    ;; n2 is fixnum
	    (:locally (:jmp (:edi (:edi-offset fast-compare-real-fixnum))))
	   n2-not-fixnum-but-n1-is
	    (:locally (:jmp (:edi (:edi-offset fast-compare-fixnum-real))))
	   neither-is-fixnum
	    ;; Check that both numbers are bignums, and compare them.
	    (:leal (:eax ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jnz '(:sub-program (n1-not-bignum)
		    (:int 107)))
	    (:movl (:eax ,movitz:+other-type-offset+) :ecx)
	    (:cmpb ,(movitz:tag :bignum) :cl)
	    (:jne 'n1-not-bignum)

	    (:cmpl :eax :ebx)		; If they are EQ, they are certainly =
	    (:je '(:sub-program (n1-and-n2-are-eq)
		   (:ret)))

	    (:leal (:ebx ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jnz '(:sub-program (n2-not-bignum)
		    (:int 107)))
	    (:movl (:ebx ,movitz:+other-type-offset+) :ecx)
	    (:cmpb ,(movitz:tag :bignum) :cl)
	    (:jne 'n2-not-bignum)

	    (:cmpb :ch (:eax ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::sign)))
	    (:jne '(:sub-program (different-signs)
		    ;; Comparing the sign-bytes sets up EFLAGS correctly!
		    (:ret)))
	    (:testl #xff00 :ecx)
	    (:jnz 'compare-negatives)
	    ;; Both n1 and n2 are positive bignums.

	    (:shrl 16 :ecx)
	    (:cmpw :cx (:eax ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::length)))
	    (:jne '(:sub-program (positive-different-sizes)
		    (:ret)))

	    ;; Both n1 and n2 are positive bignums of the same size, namely ECX.
	    (:movl :ecx :edx)		; counter
	   positive-compare-loop
	    (:subl ,movitz:+movitz-fixnum-factor+ :edx)
	    (:jz 'positive-compare-lsb)
	    (:movl (:ebx :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
		   :ecx)
	    (:cmpl :ecx
		   (:eax :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
	    (:je 'positive-compare-loop)
	   positive-compare-lsb
	    ;; Now we have to make the compare act as unsigned, which is why
	    ;; we compare zero-extended 16-bit quantities.
	    (:movzxw (:ebx :edx ,(+ 2 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		     :ecx)		; First compare upper 16 bits.
	    (:locally (:movl :ecx (:edi (:edi-offset scratch0))))
	    (:movzxw (:eax :edx ,(+ 2 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		     :ecx)
	    (:locally (:cmpl (:edi (:edi-offset scratch0)) :ecx))
	    (:jne 'upper-16-decisive)
	    (:movzxw (:ebx :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
		     :ecx)		; Then compare lower 16 bits.
	    (:locally (:movl :ecx (:edi (:edi-offset scratch0))))
	    (:movzxw (:eax :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
		     :ecx)		; Then compare lower 16 bits.
	    (:locally (:cmpl (:edi (:edi-offset scratch0)) :ecx))
	   upper-16-decisive
	    (:ret)
	    
	   compare-negatives
	    ;; Moth n1 and n2 are negative bignums.

	    (:shrl 16 :ecx)
	    (:cmpw (:eax ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::length)) :cx)
	    (:jne '(:sub-program (negative-different-sizes)
		    (:ret)))

	    ;; Both n1 and n2 are negative bignums of the same size, namely ECX.
	    (:movl :ecx :edx)		; counter
	   negative-compare-loop
	    (:subl ,movitz:+movitz-fixnum-factor+ :edx)
	    (:jz 'negative-compare-lsb)
	    (:movl (:eax :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
		   :ecx)
	    (:cmpl :ecx
		   (:ebx :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
	    (:je 'negative-compare-loop)
	    (:ret)
	   negative-compare-lsb		; it's down to the LSB bigits.
	    ;; Now we have to make the compare act as unsigned, which is why
	    ;; we compare zero-extended 16-bit quantities.
	    (:movzxw (:ebx :edx ,(+ 2 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		     :ecx)		; First compare upper 16 bits.
	    (:locally (:movl :ecx (:edi (:edi-offset scratch0))))
	    (:movzxw (:eax :edx ,(+ 2 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		     :ecx)
	    (:locally (:cmpl :ecx (:edi (:edi-offset scratch0))))
	    (:jne 'negative-upper-16-decisive)
	    (:movzxw (:ebx :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
		     :ecx)		; Then compare lower 16 bits.
	    (:locally (:movl :ecx (:edi (:edi-offset scratch0))))
	    (:movzxw (:eax :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
		     :ecx)		; Then compare lower 16 bits.
	    (:locally (:cmpl :ecx (:edi (:edi-offset scratch0))))
	   negative-upper-16-decisive
	    (:ret))))
    (do-it)))

(define-primitive-function fast-eql (x y)
  "Compare EAX and EBX under EQL, result in ZF.
Preserve EAX and EBX."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :nothing) ; unspecified
	    (:cmpl :eax :ebx)		; EQ?
	    (:je 'done)
	    (:leal (:eax ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jne 'done)
	    (:leal (:ebx ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jne 'done)
	    (:movl (:eax ,movitz:+other-type-offset+) :ecx)
	    (:cmpb ,(movitz:tag :bignum) :cl)
	    (:jne 'done)
	    (:cmpl :ecx (:ebx ,movitz:+other-type-offset+))
	    (:jne 'done)
	    ;; Ok.. we have two bignums of identical sign and size.
	    (:shrl 16 :ecx)
	    (:movl :ecx :edx)		; counter
	   compare-loop
	    (:subl ,movitz:+movitz-fixnum-factor+ :edx)
	    (:jz 'done)
	    (:movl (:eax :edx ,(+ -4 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		   :ecx)
	    (:cmpl :ecx
		   (:ebx :edx ,(+ -4 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))))
	    (:je 'compare-loop)
	   done
	    (:ret))))
    (do-it)))

(define-primitive-function fast-compare-fixnum-real (n1 n2)
  "Compare (known) fixnum <n1> with real <n2>."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :nothing) ; unspecified
	    (:testb ,movitz::+movitz-fixnum-zmask+ :bl)
	    (:jnz 'n2-not-fixnum)
	    (:cmpl :ebx :eax)
	    (:ret)
	   n2-not-fixnum
	    (:leal (:ebx ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jnz '(:sub-program (not-integer)
		    (:int 107)
		    (:jmp 'not-integer)))
	    (:movl (:ebx ,movitz:+other-type-offset+) :ecx)
	    (:cmpw ,(movitz:tag :bignum 0) :cx)
	    (:jne 'not-plusbignum)
	    ;; compare eax with something bigger
	    (:cmpl #x10000000 :edi)
	    (:ret)
	   not-plusbignum
	    (:cmpw ,(movitz:tag :bignum #xff) :cx)
	    (:jne 'not-integer)
	    ;; compare ebx with something bigger
	    (:cmpl #x-10000000 :edi)
	    (:ret))))
    (do-it)))

(define-primitive-function fast-compare-real-fixnum (n1 n2)
  "Compare real <n1> with fixnum <n2>."
  (with-inline-assembly (:returns :nothing) ; unspecified
    (:testb #.movitz::+movitz-fixnum-zmask+ :al)
    (:jnz 'not-fixnum)
    (:cmpl :ebx :eax)
    (:ret)
   not-fixnum
    (:leal (:eax #.(cl:- (movitz:tag :other))) :ecx)
    (:testb 7 :cl)
    (:jnz '(:sub-program (not-integer)
	    (:int 107)
	    (:jmp 'not-integer)))
    (:movl (:eax #.movitz:+other-type-offset+) :ecx)
    (:cmpw #.(movitz:tag :bignum 0) :cx)
    (:jne 'not-plusbignum)
    ;; compare ebx with something bigger
    (:cmpl #x-10000000 :edi)
    (:ret)
   not-plusbignum
    (:cmpw #.(movitz:tag :bignum #xff) :cx)
    (:jne 'not-integer)
    ;; compare ebx with something bigger
    (:cmpl #x10000000 :edi)
    (:ret)))

;;;

(define-compiler-macro <=%3op (min x max &environment env)
  (cond
   ((and (movitz:movitz-constantp min env)
	 (movitz:movitz-constantp max env))
    (let ((min (movitz:movitz-eval min env))
	  (max (movitz:movitz-eval max env)))
      (check-type min fixnum)
      (check-type max fixnum)
      ;; (warn "~D -- ~D" min max)
      (cond
       ((movitz:movitz-constantp x env)
	(<= min (movitz:movitz-eval x env) max))
       ((< max min)
	nil)
       ((= max min)
	`(= ,x ,min))
       ((minusp min)
	`(let ((x ,x))
	   (and (<= ,min x) (<= x ,max))))
       ((= 0 min)
	`(with-inline-assembly (:returns :boolean-cf=1)
	   (:compile-form (:result-mode :eax) ,x)
	   (:testb ,movitz::+movitz-fixnum-zmask+ :al)
	   (:jnz '(:sub-program () (:int 107)))
	   (:cmpl ,(* (1+ max) movitz::+movitz-fixnum-factor+) :eax)))
       (t `(do-result-mode-case ()
	     (:booleans
	      (with-inline-assembly (:returns :boolean-zf=0)
		(:compile-form (:result-mode :eax) ,x)
		(:testb ,movitz::+movitz-fixnum-zmask+ :al)
		(:jnz '(:sub-program () (:int 107)))
		(:cmpl ,(* min movitz::+movitz-fixnum-factor+) :eax)
		(:sbbl :ecx :ecx)
		(:cmpl ,(* (1+ max) movitz::+movitz-fixnum-factor+) :eax)
		(:adcl 0 :ecx)))
	     (t (with-inline-assembly (:returns (:boolean-ecx 1 0))
		  (:compile-form (:result-mode :eax) ,x)
		  (:testb ,movitz::+movitz-fixnum-zmask+ :al)
		  (:jnz '(:sub-program () (:int 107)))
		  (:cmpl ,(* min movitz::+movitz-fixnum-factor+) :eax)
		  (:sbbl :ecx :ecx)
		  (:cmpl ,(* (1+ max) movitz::+movitz-fixnum-factor+) :eax)
		  (:adcl 0 :ecx))))))))
   #+ignore				; this is buggy.
   ((movitz:movitz-constantp min env)
    (let ((min (movitz:movitz-eval min env)))
      (check-type min fixnum)
      (cond
       ((minusp min)
	`(let ((x ,x))
	   (and (<= ,min x) (<= x ,max))))
       (t `(do-result-mode-case ()
	     (:booleans
	      (with-inline-assembly (:returns :boolean-zf=1)
		(:compile-two-forms (:eax :ebx) ,x ,max)
		(:movl :eax :ecx)
		(:orl :ebx :ecx)
		(:testb ,movitz::+movitz-fixnum-zmask+ :cl)
		(:jne '(:sub-program () (:int 107)))
		(:cmpl :eax :ebx)
		(:sbbl :ecx :ecx)
		,@(unless (= 0 min)
		    `((:subl ,(* min movitz::+movitz-fixnum-factor+) :ebx)))
		(:addl :ebx :ebx)
		(:adcl 0 :ecx)))
	     (t (with-inline-assembly (:returns (:boolean-ecx 0 1))
		  (:compile-two-forms (:eax :ebx) ,x ,max)
		  (:movl :eax :ecx)
		  (:orl :ebx :ecx)
		  (:testb ,movitz::+movitz-fixnum-zmask+ :cl)
		  (:jne '(:sub-program () (:int 107)))
		  (:cmpl :eax :ebx)	; if x>max, CF=1
		  (:sbbl :ecx :ecx)	; ecx = x>max ? -1 : 0
		  ,@(unless (= 0 min)
		      `((:subl ,(* min movitz::+movitz-fixnum-factor+) :ebx)))
		  (:addl :ebx :ebx)	; if x<min, CF=1
		  (:adcl 0 :ecx)	; 
		  (:andl 1 :ecx))))))))
   (t `(let ((x ,x))
	 (and (<= ,min x) (<= x ,max))))))
       

(defmacro define-number-relational (name 2op-name condition &key (defun-p t) 3op-name)
  `(progn
     ,(when condition
	`(define-compiler-macro ,2op-name (n1 n2)
	   (cond
	    ((movitz:movitz-constantp n1)
	     (let ((n1 (movitz::movitz-eval n1)))
	       (check-type n1 (signed-byte 30))
	       `(with-inline-assembly (:returns ,,condition :side-effects nil)
		  (:compile-two-forms (:eax :ebx) ,n1 ,n2)
		  (:call-global-constant fast-compare-fixnum-real))))
	    ((movitz:movitz-constantp n2)
	     (let ((n2 (movitz::movitz-eval n2)))
	       (check-type n2 (signed-byte 30))
	       `(with-inline-assembly (:returns ,,condition :side-effects nil)
		  (:compile-two-forms (:eax :ebx) ,n1 ,n2)
		  (:call-global-constant fast-compare-real-fixnum))))
	    (t `(with-inline-assembly (:returns ,,condition :side-effects nil)
		  (:compile-two-forms (:eax :ebx) ,n1 ,n2)
		  (:call-global-constant fast-compare-two-reals))))))

     (defun ,2op-name (n1 n2)
       (,2op-name n1 n2))

     (define-compiler-macro ,name (&whole form number &rest more-numbers)
       (case (length more-numbers)
	 (0 `(progn ,number t))
	 (1 `(,',2op-name ,number ,(first more-numbers)))
	 ,@(when 3op-name
	     `((2 `(,',3op-name ,number ,(first more-numbers) ,(second more-numbers)))))
	 (t #+ignore (when (= 2 (length more-numbers))
		       (warn "3op: ~S" form))
	  `(and (,',2op-name ,number ,(first more-numbers))
		  (,',name ,@more-numbers)))))

     ,(when defun-p
	`(defun ,name (number &rest more-numbers)
	   (declare (dynamic-extent more-numbers))
	   (cond
	    ((null more-numbers)
	     (check-type number fixnum)
	     t)
	    ((not (cdr more-numbers))
	     (,2op-name number (first more-numbers)))
	    (t (and (,2op-name number (first more-numbers))
		    (do ((p more-numbers (cdr p)))
			((not (cdr p)) t)
		      (unless (,2op-name (car p) (cadr p))
			(return nil))))))))))

(define-number-relational >= >=%2op :boolean-greater-equal)
(define-number-relational > >%2op :boolean-greater)
(define-number-relational < <%2op :boolean-less)
(define-number-relational <= <=%2op :boolean-less-equal :3op-name <=%3op)

;;; Unsigned

(define-compiler-macro below (&whole form x max &environment env)
  (let ((below-not-integer (gensym "below-not-integer-")))
    (if (movitz:movitz-constantp max env)
	`(with-inline-assembly (:returns :boolean-cf=1)
	   (:compile-form (:result-mode :eax) ,x)
	   (:testb ,movitz::+movitz-fixnum-zmask+ :al)
	   (:jnz '(:sub-program (,below-not-integer) (:int 107)))
	   (:cmpl ,(* (movitz:movitz-eval max env)
		      movitz::+movitz-fixnum-factor+)
		  :eax))
      `(with-inline-assembly (:returns :boolean-cf=1)
	 (:compile-two-forms (:eax :ebx) ,x ,max)
	 (:movl :eax :ecx)
	 (:orl :ebx :ecx)
	 (:testb ,movitz::+movitz-fixnum-zmask+ :cl)
	 (:jnz '(:sub-program (,below-not-integer) (:int 107)))
	 (:cmpl :ebx :eax)))))

(defun below (x max)
  "Is x between 0 and max?"
  (below x max))


;;; Equality

(define-compiler-macro =%2op (n1 n2 &environment env)
  (cond
   ((movitz:movitz-constantp n1 env)
    (let ((n1 (movitz::movitz-eval n1 env)))
      (etypecase n1
	((eql 0)
	 `(do-result-mode-case ()
	    (:booleans
	     (with-inline-assembly (:returns :boolean-zf=1 :side-effects nil)
	       (:compile-form (:result-mode :eax) ,n2)
	       (:testl :eax :eax)))
	    (t (with-inline-assembly (:returns :boolean-cf=1 :side-effects nil)
		 (:compile-form (:result-mode :eax) ,n2)
		 (:cmpl 1 :eax)))))
	((signed-byte 30)
	 `(with-inline-assembly (:returns :boolean-zf=1 :side-effects nil)
	    (:compile-two-forms (:eax :ebx) ,n1 ,n2)
	    (:call-global-constant fast-compare-fixnum-real))))))
   ((movitz:movitz-constantp n2 env)
    (let ((n2 (movitz::movitz-eval n2 env)))
      (check-type n2 (signed-byte 30))
      `(with-inline-assembly (:returns :boolean-zf=1 :side-effects nil)
	 (:compile-two-forms (:eax :ebx) ,n1 ,n2)
	 (:call-global-constant fast-compare-real-fixnum))))
   (t `(with-inline-assembly (:returns :boolean-zf=1 :side-effects nil)
	 (:compile-two-forms (:eax :ebx) ,n1 ,n2)
	 (:call-global-constant fast-compare-two-reals)))))

(define-number-relational = =%2op nil :defun-p nil)

(defun = (first-number &rest numbers)
  (declare (dynamic-extent numbers))
  (dolist (n numbers t)
    (unless (= first-number n)
      (return nil))))

(define-number-relational /= /=%2op :boolean-zf=0 :defun-p nil)

(defun /= (&rest numbers)
  (declare (dynamic-extent numbers))
  (do ((p (cdr numbers) (cdr p)))
      ((null p) t)
    (do ((v numbers (cdr v)))
	((eq p v))
      (when (= (car p) (car v))
	(return-from /= nil)))))

;;;

(defun zerop (number)
  (= 0 number))

(define-compiler-macro zerop (number)
  `(= 0 ,number))

(defun plusp (number)
  (> number 0))

(define-compiler-macro plusp (number)
  `(> ,number 0))

(defun minusp (number)
  (< number 0))

(define-compiler-macro minusp (number)
  `(< ,number 0))

(define-compiler-macro abs (x)
  `(let ((x ,x))
     (if (>= x 0) x (- x))))

(defun abs (x)
  (abs x))

(defun signum (x)
  (cond
   ((> x 0) 1)
   ((< x 0) -1)
   (t 0)))

;;;

(define-compiler-macro max (&whole form first-number &rest more-numbers)
  (case (length more-numbers)
    (0 first-number)
    (1 `(let ((x ,first-number)
	      (y ,(car more-numbers)))
	  (if (>= x y) x y)))
    ((2 3 4)
     `(max ,first-number (max ,@more-numbers)))
    (t form)))

(defun max (number1 &rest numbers)
  (numargs-case
   (2 (x y) (max x y))
   (t (number1 &rest numbers)
      (declare (dynamic-extent numbers))
      (let ((max number1))
	(dolist (x numbers max)
	  (when (> x max)
	    (setq max x)))))))

(define-compiler-macro min (&whole form first-number &rest more-numbers)
  (case (length more-numbers)
    (0 first-number)
    (1 `(let ((x ,first-number)
	      (y ,(car more-numbers)))
	  (if (<= x y) x y)))
    ((2 3 4)
     `(min ,first-number (min ,@more-numbers)))
    (t form)))

(defun min (number1 &rest numbers)
  (numargs-case
   (2 (x y) (min x y))
   (t (number1 &rest numbers)
      (declare (dynamic-extent numbers))
      (let ((min number1))
	(dolist (x numbers min)
	  (when (< x min)
	    (setq min x)))))))

;; shift 

(define-compiler-macro ash (&whole form integer count &environment env)
  (if (not (movitz:movitz-constantp count env))
      form
    (let ((count (movitz:movitz-eval count env)))
      (cond
       ((movitz:movitz-constantp integer env)
	(ash (movitz::movitz-eval integer env) count))
       ((= 0 count)
	integer)
       (t form
	  #+igore
	  (let ((load-integer `((:compile-form (:result-mode :register) ,integer)
				(:testb ,movitz::+movitz-fixnum-zmask+ (:result-register-low8))
				(:jnz '(:sub-program () (:int 107) (:jmp (:pc+ -4)))))))
	    (cond
	     ((<= 1 count 4)
	      `(with-inline-assembly (:returns :register :side-effects nil)
		 ,@load-integer
		 ,@(loop repeat count
		       append `((:addl (:result-register) (:result-register))
				(:into)))))
	     ((< 0 count #.(cl:1- movitz::+movitz-fixnum-bits+))
	      `(with-inline-assembly (:returns :register :side-effects nil :type integer)
		 ,@load-integer
		 (:cmpl ,(ash 1 (- (- 31 0) count))
			(:result-register))
		 (:jge '(:sub-program () (:int 4)))
		 (:cmpl ,(- (ash 1 (- (- 31 0) count)))
			(:result-register))
		 (:jl '(:sub-program () (:int 4)))
		 (:shll ,count (:result-register))))
	     ((= -1 count)
	      `(with-inline-assembly (:returns :register :side-effects nil :type integer)
		 ,@load-integer
		 (:andb #.(cl:logxor #xfe (cl:* 2 movitz::+movitz-fixnum-zmask+)) (:result-register-low8))
		 (:sarl 1 (:result-register))))
	     ((> 0 count #.(cl:- (cl:1- movitz::+movitz-fixnum-bits+)))
	      `(with-inline-assembly (:returns :register :side-effects nil :type integer)
		 ,@load-integer
		 (:andl ,(ldb (byte 32 0)
			      (ash movitz:+movitz-most-positive-fixnum+
				   (- movitz:+movitz-fixnum-shift+ count)))
			(:result-register))
		 (:sarl ,(- count) (:result-register))))
	     ((minusp count)
	      `(if (minusp ,integer) -1 0))
	     (t `(if (= 0 ,integer) 0 (with-inline-assembly (:returns :non-local-exit) (:int 4)))))))))))
  
(defun ash (integer count)
  (cond
   ((not (minusp count))
    (do () ((< count 16))
      (setf integer (no-macro-call * #x10000 integer))
      (decf count 16))
    (dotimes (i count integer)
      (setf integer (no-macro-call * 2 integer))))
   (t (dotimes (i (- count) integer)
	(setf integer (truncate integer 2))))))

;;;;

(defun integer-length (integer)
  "=> number-of-bits"
  (etypecase integer
    (fixnum
     (macrolet
	 ((do-it ()
	    `(with-inline-assembly (:returns :eax)
	       (:xorl :eax :eax)
	       (:compile-form (:result-mode :ecx) integer)
	       (:testl :ecx :ecx)
	       (:jns 'not-negative)
	       (:notl :ecx)
	      not-negative
	       (:bsrl :ecx :ecx)
	       (:jz 'zero)
	       (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)
		       ,(* -1 movitz:+movitz-fixnum-factor+))
		      :eax)
	      zero)))
       (do-it)))
    (positive-bignum
     (macrolet
	 ((do-it ()
	    `(with-inline-assembly (:returns :eax)
	       (:compile-form (:result-mode :ebx) integer)
	       (:movzxw (:ebx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::length))
			:ecx)
	       (:leal ((:ecx 1) ,(* -1 movitz:+movitz-fixnum-factor+))
		      :eax)		; bigits-1
	       (:bsrl (:ebx (:ecx 1) ,(+ -4 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		      :ecx)
	       (:shll 5 :eax)		; bits = bigits*32 + (bit-index+1)
	       (:leal ((:ecx ,movitz:+movitz-fixnum-factor+) :eax
							     ,movitz:+movitz-fixnum-factor+)
		      :eax))))
       (do-it)))))

;;; Multiplication

(define-compiler-macro * (&whole form &rest operands &environment env)
  (case (length operands)
    (0 0)
    (1 (first operands))
    (2 (let ((factor1 (first operands))
	     (factor2 (second operands)))
	 (cond
	  ((and (movitz:movitz-constantp factor1 env)
		(movitz:movitz-constantp factor2 env))
	   (* (movitz:movitz-eval factor1 env)
	      (movitz:movitz-eval factor2 env)))
	  ((movitz:movitz-constantp factor2 env)
	   `(* ,(movitz:movitz-eval factor2 env) ,factor1))
	  ((movitz:movitz-constantp factor1 env)
	   (let ((f1 (movitz:movitz-eval factor1 env)))
	     (check-type f1 integer)
	     (case f1
	       (0 `(progn ,factor2 0))
	       (1 factor2)
;;;	       (2 `(let ((x ,factor2)) (+ x x)))
	       (t `(no-macro-call * ,factor1 ,factor2)))))
	  (t `(no-macro-call * ,factor1 ,factor2)))))
    (t `(* (* ,(first operands) ,(second operands)) ,@(cddr operands)))))

(defun * (&rest factors)
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (macrolet
	  ((do-it ()
	     `(number-double-dispatch (x y)
		((fixnum fixnum)
		 (let (d0 d1)
		   (with-inline-assembly (:returns :eax)
		     (:compile-two-forms (:eax :ecx) x y)
		     (:sarl ,movitz::+movitz-fixnum-shift+ :ecx)
		     (:std)
		     (:imull :ecx :eax :edx)
		     (:jno 'fixnum-result) ; most likely/optimized path.
		     (:cmpl ,movitz::+movitz-fixnum-factor+ :edx)
		     (:jc 'u32-result)
		     (:cmpl #xfffffffc :edx)
		     (:ja 'u32-negative-result)
		     (:jne 'two-bigits)
		     (:testl :eax :eax)
		     (:jnz 'u32-negative-result)
		     ;; The result requires 2 bigits..
		    two-bigits
		     (:shll ,movitz::+movitz-fixnum-shift+ :edx) ; guaranteed won't overflow.
		     (:cld)
		     (:store-lexical (:lexical-binding d0) :eax :type fixnum)
		     (:store-lexical (:lexical-binding d1) :edx :type fixnum)
		     (:compile-form (:result-mode :eax)
				    (malloc-data-words 3))
		     (:movl ,(dpb (* 2 movitz:+movitz-fixnum-factor+)
				  (byte 16 16) (movitz:tag :bignum 0))
			    (:eax ,movitz:+other-type-offset+))
		     (:load-lexical (:lexical-binding d0) :ecx)
		     (:movl :ecx (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		     (:load-lexical (:lexical-binding d1) :ecx)
		     (:sarl ,movitz:+movitz-fixnum-shift+
			    :ecx)
		     (:shrdl ,movitz:+movitz-fixnum-shift+ :ecx
			     (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		     (:sarl ,movitz:+movitz-fixnum-shift+
			    :ecx)
		     (:movl :ecx (:eax ,(+ 4 (bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))
		     (:jns 'fixnum-done)
		     ;; if result was negative, we must negate bignum
		     (:notl (:eax ,(+ 4 (bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))
		     (:negl (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		     (:cmc)
		     (:adcl 0 (:eax ,(+ 4 (bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))
		     (:xorl #xff00 (:eax ,movitz:+other-type-offset+))
		     (:jmp 'fixnum-done)
		     
		    u32-result
		     (:movl :eax :ecx)
		     (:shrdl ,movitz::+movitz-fixnum-shift+ :edx :ecx)
		     (:movl :edi :edx)
		     (:cld)
		     (:call-global-constant box-u32-ecx)
		     (:jmp 'fixnum-done)
		     
		    u32-negative-result
		     (:movl :eax :ecx)
		     (:shrdl ,movitz::+movitz-fixnum-shift+ :edx :ecx)
		     (:movl :edi :edx)
		     (:cld)
		     (:negl :ecx)
		     (:call-global-constant box-u32-ecx)
		     (:xorl #xff00 (:eax ,movitz:+other-type-offset+))
		     (:jmp 'fixnum-done)

		    fixnum-result
		     (:movl :edi :edx)
		     (:cld)
		    fixnum-done)))
		(((eql 0) t) 0)
		(((eql 1) t) y)
		(((eql -1) t) (- y))
		((t fixnum) (* y x))
		((fixnum bignum)
		 (let (r)
		   (with-inline-assembly (:returns :eax)
		    retry
		     (:declare-label-set retry-jumper (retry))
		     (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		     (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
					'retry-jumper)
				      (:edi (:edi-offset atomically-status))))
			     
		     (:compile-form (:result-mode :eax) y)
		     (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			      :ecx)
		     (:leal ((:ecx 1) ,(* 2 movitz:+movitz-fixnum-factor+))
			    :eax)
		     (:call-global-constant get-cons-pointer) ; New bignum into EAX

		     (:load-lexical (:lexical-binding y) :ebx) ; bignum
		     (:movl (:ebx ,movitz:+other-type-offset+) :ecx)
		     (:movl :ecx (:eax ,movitz:+other-type-offset+))
		     (:store-lexical (:lexical-binding r) :eax :type bignum)

		     (:movl :eax :ebx)	; r into ebx
		     (:xorl :ecx :ecx)	; counter
		     (:xorl :edx :edx)	; initial carry
		     (:std)		; Make EAX, EDX, ESI non-GC-roots.
		     (:compile-form (:result-mode :esi) x)
		     (:sarl ,movitz:+movitz-fixnum-shift+ :esi)
		     (:jns 'multiply-loop)
		     (:negl :esi)	; can't overflow
		    multiply-loop
		     (:movl :edx (:ebx (:ecx 1) ; new
				       ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		     (:compile-form (:result-mode :ebx) y)
		     (:movl (:ebx (:ecx 1) ; old
				  ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))
			    :eax)
		     
		     (:mull :esi :eax :edx)
		     (:compile-form (:result-mode :ebx) r)
		     (:addl :eax
			    (:ebx (:ecx 1)
				  ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		     (:adcl 0 :edx)
		     (:addl 4 :ecx)
		     (:cmpw :cx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:ja 'multiply-loop)
		     (:testl :edx :edx)
		     (:jz 'no-carry-expansion)
		     (:movl :edx
			    (:ebx (:ecx 1)
				  ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		     (:addl 4 :ecx)
		     (:movw :cx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		    no-carry-expansion
		     (:movl (:ebp -4) :esi)
		     (:movl :ebx :eax)
		     (:movl :edi :edx)
		     (:cld)		; EAX, EDX, and ESI are GC roots again.
		     (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			    :ecx)
		     (:call-global-constant cons-commit)
		     (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				      (:edi (:edi-offset atomically-status))))
		     (:compile-form (:result-mode :ebx) x)
		     (:testl :ebx :ebx)
		     (:jns 'positive-result)
		     ;; Negate the resulting bignum
		     (:xorl #xff00 (:eax ,movitz:+other-type-offset+))
		    positive-result
		     )))
		((positive-bignum positive-bignum)
		 (if (< x y)
		     (* y x)
		   ;; X is the biggest factor.
		   (let ((r 0) (f 0))
		     (dotimes (half-bigit (* 2 (%bignum-bigits y)))
		       (setf r (+ r (ash (* (memref y -2 half-bigit :unsigned-byte16) x)
					  f)))
		       (incf f 16))
		     r))))))
	(do-it)))
   (t (&rest factors)
      (declare (dynamic-extent factors))
      (if (null factors)
	  1
	(reduce '* factors)))))

;;; Division

(defun truncate (number &optional (divisor 1))
  (numargs-case
   (1 (number)
      (values number 0))
   (t (number divisor)
      (number-double-dispatch (number divisor)
	((t (eql 1))
	 number)
	((fixnum fixnum)
	 (with-inline-assembly (:returns :multiple-values)
	   (:compile-form (:result-mode :eax) number)
	   (:compile-form (:result-mode :ebx) divisor)
	   (:std)
	   (:cdq :eax :edx)
	   (:idivl :ebx :eax :edx)
	   (:shll #.movitz::+movitz-fixnum-shift+ :eax)
	   (:cld)
	   (:movl :edx :ebx)
	   (:xorl :ecx :ecx)
	   (:movb 2 :cl)		; return values: qutient, remainder.
	   (:stc)))
	((positive-fixnum positive-bignum)
	 (values 0 number))
	((positive-bignum positive-fixnum)
	 (macrolet
	     ((do-it ()
		`(let (r n)
		   (with-inline-assembly (:returns :multiple-values)
		     (:compile-form (:result-mode :ebx) number)
		     (:cmpw ,movitz:+movitz-fixnum-factor+
			    (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:jne 'not-size1)
		     (:compile-form (:result-mode :ecx) divisor)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:std)
		     (:movl (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)) :eax)
		     (:xorl :edx :edx)
		     (:divl :ecx :eax :edx)
		     (:movl :eax :ecx)
		     (:shll ,movitz:+movitz-fixnum-shift+ :edx)
		     (:movl :edi :eax)
		     (:cld)
		     (:pushl :edx)
		     (:call-global-constant box-u32-ecx)
		     (:popl :ebx)
		     (:jmp 'done)
		    not-size1
		     (:compile-form (:result-mode :ebx) number)
		     (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			      :ecx)
	     
		     (:declare-label-set retry-jumper (not-size1))
		     (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		     (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
					'retry-jumper)
				      (:edi (:edi-offset atomically-status))))

		     (:leal ((:ecx 1) 4) :eax) ; Number of words
		     (:call-global-constant get-cons-pointer) ; New bignum into EAX
		     

		     (:store-lexical (:lexical-binding r) :eax :type bignum)
		     (:compile-form (:result-mode :ebx) number)
		     (:movl (:ebx #.movitz:+other-type-offset+) :ecx)
		     (:movl :ecx (:eax #.movitz:+other-type-offset+))
		     (:shrl 16 :ecx)
	     
		     (:xorl :edx :edx)	; edx=hi-digit=0
					; eax=lo-digit=msd(number)
		     (:std)
		     (:compile-form (:result-mode :esi) divisor)
		     (:shrl #.movitz:+movitz-fixnum-shift+ :esi)

		    divide-loop
		     (:load-lexical (:lexical-binding number) :ebx)
		     (:movl (:ebx #.(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)
				  -4 (:ecx 1))
			    :eax)
		     (:divl :esi :eax :edx)
		     (:load-lexical (:lexical-binding r) :ebx)
		     (:movl :eax (:ebx #.(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)
				       -4 (:ecx 1)))
		     (:subl 4 :ecx)
		     (:jnz 'divide-loop)
		     (:movl :edi :eax)	; safe value
		     (:leal ((:edx ,movitz:+movitz-fixnum-factor+)) :edx)
		     (:movl (:ebp -4) :esi)
		     (:cld)
		     (:movl :ebx :eax)
		     (:movl :edx :ebx)

		     (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			      :ecx)
		     (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			    :ecx)
		     (:cmpl 0 (:eax :ecx ,(+ -8 (bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))
		     (:jne 'no-more-shrinkage)
		     
		     (:subw 4 (:eax #.(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:subl ,movitz:+movitz-fixnum-factor+ :ecx)
		     (:cmpl ,(* 2 movitz:+movitz-fixnum-factor+) :ecx)
		     (:jne 'no-more-shrinkage)
		     (:cmpl ,movitz:+movitz-most-positive-fixnum+
			    (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		     (:jnc 'no-more-shrinkage)
		     (:movl (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))
			    :ecx)
		     (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
		     (:jmp 'fixnum-result) ; don't commit the bignum
		    no-more-shrinkage
		     (:call-global-constant cons-commit)
		    fixnum-result
		     (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				      (:edi (:edi-offset atomically-status))))	     
		    done
		     (:movl 2 :ecx)
		     (:stc)))))
	   (do-it)))
	((positive-bignum positive-bignum)
	 (cond
	  ((= number divisor) (values 1 0))
	  ((< number divisor) (values 0 number))
	  (t (let* ((msb-pos (1- (* 2 (%bignum-bigits divisor))))
		    (msb (memref divisor -2 msb-pos :unsigned-byte16)))
	       (when (= 0 msb)
		 (decf msb-pos)
		 (setf msb (memref divisor -2 msb-pos :unsigned-byte16))
		 (assert (plusp msb)))
	       (do ((msb+1 (1+ msb))
		    (q 0) (r number))
		   ((< r divisor) (values q r))
		 (let ((guess (truncate r msb+1)))
		   (dotimes (i msb-pos)
		     (setf guess (truncate guess #x10000)))
		   (if (= 0 guess)
		       (setf q (1+ q)
			     r (- r divisor))
		     (setf q (+ q guess)
			   r (- r (* divisor guess))))))))))
	(((integer * -1) (integer 0 *))
	 (multiple-value-bind (q r)
	     (truncate (- number) divisor)
	   (values (- q) (- r))))
	(((integer 0 *) (integer * -1))
	 (multiple-value-bind (q r)
	     (truncate (- number) divisor)
	   (values (- q) r)))
	(((integer * -1) (integer * -1))
	 (multiple-value-bind (q r)
	     (truncate (- number) divisor)
	   (values q (- r))))
	))))

(defun / (number &rest denominators)
  (declare (dynamic-extent denominators))
  (cond
   ((null denominators)
    (make-ratio 1 number))
   ((null (cdr denominators))
    (multiple-value-bind (q r)
	(truncate number (first denominators))
      (if (= 0 r)
	  q
	(error "Don't know how to divide ~S by ~S." number (first denominators)))))
   (t (reduce '/ denominators :initial-value number))))
	       
(defun round (number &optional (divisor 1))
  "Mathematical rounding."
  (multiple-value-bind (quotient remainder)
      (truncate number divisor)
    (let ((rem2 (* 2 remainder)))
      (case (+ (if (minusp number) #b10 0)
	       (if (minusp divisor) #b01 0))
	(#b00 (cond
	       ((= divisor rem2)
		(if (evenp quotient)
		    (values quotient remainder)
		  (values (1+ quotient) (- remainder divisor))))
	       ((< rem2 divisor)
		(values quotient remainder))
	       (t (values (1+ quotient) (- remainder divisor)))))
	(#b11 (cond
	       ((= divisor rem2)
		(if (evenp quotient)
		    (values quotient remainder)
		  (values (1+ quotient) (- remainder divisor))))
	       ((> rem2 divisor)
		(values quotient remainder))
	       (t (values (1+ quotient) (- remainder divisor)))))
	(#b10 (cond
	       ((= (- divisor) rem2)
		(if (evenp quotient)
		    (values quotient remainder)
		  (values (1- quotient) (- remainder))))
	       ((< rem2 divisor)
		(values quotient remainder))
	       (t (values (1+ quotient) (- remainder divisor)))))
	(#b01 (cond
	       ((= (- divisor) rem2)
		(if (evenp quotient)
		    (values quotient remainder)
		  (values (1- quotient) (- remainder))))
	       ((> rem2 divisor)
		(values quotient remainder))
	       (t (values (1- quotient) (- remainder)))))))))

(defun ceiling (number &optional (divisor 1))
  (case (+ (if (minusp number) #b10 0)
	   (if (minusp divisor) #b01 0))
    (#b00 (multiple-value-bind (q r)
	      (truncate (+ number divisor -1) divisor)
	    (values q (- r (1- divisor)))))
    (t (error "Don't know."))))

(defun rem (dividend divisor)
  (nth-value 1 (truncate dividend divisor)))

(defun mod (number divisor)
  "Returns second result of FLOOR."
  (let ((rem (rem number divisor)))
    (if (and (not (zerop rem))
             (if (minusp divisor)
                 (plusp number)
                 (minusp number)))
        (+ rem divisor)
        rem)))

;;; bytes

(defun byte (size position)
  (+ (* size #x400) position))

(define-compiler-macro byte (&whole form size position)
  (cond
   ((and (integerp size)
	 (integerp position))
    (+ (* size #x400) position))
   #+ignore
   ((integerp size)
    `(+ ,position ,(* size #x400)))
   (t form)))

(defun byte-size (bytespec)
  (truncate bytespec #x400))

(defun byte-position (bytespec)
  (rem bytespec #x400))

(defun logbitp (index integer)
  (check-type index positive-fixnum)
  (macrolet
      ((do-it ()
	 `(etypecase integer
	    (fixnum
	     (with-inline-assembly (:returns :boolean-cf=1)
	       (:compile-two-forms (:ecx :ebx) index integer)
	       (:shrl ,movitz::+movitz-fixnum-shift+ :ecx)
	       (:addl ,movitz::+movitz-fixnum-shift+ :ecx)
	       (:btl :ecx :ebx)))
	    (positive-bignum
	     (with-inline-assembly (:returns :boolean-cf=1)
	       (:compile-two-forms (:ecx :ebx) index integer)
	       (:shrl ,movitz::+movitz-fixnum-shift+ :ecx)
	       (:btl :ecx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))))))
    (do-it)))

(define-compiler-macro logand (&whole form &rest integers &environment env)
  (let ((constant-folded-integers (loop for x in integers
				      with folded-constant = -1
				      if (and (movitz:movitz-constantp x env)
					      (not (= -1 (movitz:movitz-eval x env))))
				      do (setf folded-constant
					   (logand folded-constant (movitz:movitz-eval x env)))
				      else collect x into non-constants
				      finally (return (if (= -1 folded-constant)
							  non-constants
							(cons folded-constant non-constants))))))
    (case (length constant-folded-integers)
      (0 0)
      (1 (first constant-folded-integers))
      (2 `(no-macro-call logand ,(first constant-folded-integers) ,(second constant-folded-integers)))
      (t `(logand (logand ,(first constant-folded-integers) ,(second constant-folded-integers))
		  ,@(cddr constant-folded-integers))))))

(defun logand (&rest integers)
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (macrolet
	  ((do-it ()
	     `(number-double-dispatch (x y)
		((fixnum fixnum)
		 (with-inline-assembly (:returns :eax)
		   (:compile-two-forms (:eax :ebx) x y)
		   (:andl :ebx :eax)))
		((positive-bignum positive-fixnum)
		 (with-inline-assembly (:returns :eax)
		   (:compile-form (:result-mode :eax) x)
		   (:call-global-constant unbox-u32)
		   (:compile-form (:result-mode :eax) y)
		   (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :ecx)
		   (:andl :ecx :eax)))
		((positive-fixnum positive-bignum)
		 (with-inline-assembly (:returns :eax)
		   (:compile-form (:result-mode :eax) y)
		   (:call-global-constant unbox-u32)
		   (:compile-form (:result-mode :eax) x)
		   (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :ecx)
		   (:andl :ecx :eax)))
		((positive-bignum positive-bignum)
		 (if (< (%bignum-bigits y) (%bignum-bigits x))
		     (logand y x)
		   (%bignum-canonicalize
		    (with-inline-assembly (:returns :eax)
		      (:compile-two-forms (:eax :ebx) (copy-bignum x) y)
		      (:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			       :ecx)
		      (:leal ((:ecx 1) -4) :edx)
		     pb-pb-and-loop
		      (:movl (:ebx :edx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))
			     :ecx)
		      (:andl :ecx
			     (:eax :edx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		      (:subl 4 :edx)
		      (:jnc 'pb-pb-and-loop)))))
		)))
	(do-it)))
   (t (&rest integers)
      (declare (dynamic-extent integers))
      (if (null integers)
	  -1
	(reduce #'logand integers)))))

(defun logandc1 (integer1 integer2)
  (macrolet
      ((do-it ()
	 `(number-double-dispatch (integer1 integer2)
	    ((t positive-fixnum)
	     (with-inline-assembly (:returns :eax :type fixnum)
	       (:compile-form (:result-mode :eax) integer1)
	       (:call-global-constant unbox-u32)
	       (:shll ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:compile-form (:result-mode :eax) integer2)
	       (:notl :ecx)
	       (:andl :ecx :eax)))
	    (((eql 0) t) integer2)
	    (((eql -1) t) 0)
	    ((positive-fixnum positive-bignum)
	     (%bignum-canonicalize
	      (with-inline-assembly (:returns :eax)
		(:compile-two-forms (:eax :ecx) (copy-bignum integer2) integer1)
		(:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		(:notl :ecx)
		(:andl :ecx
		       (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))))
	    ((positive-bignum positive-bignum)
	     (%bignum-canonicalize
	      (with-inline-assembly (:returns :eax)
		(:compile-two-forms (:eax :ebx) (copy-bignum integer2) integer1)
		(:movzxw (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			 :ecx)
		(:leal ((:ecx 1) -4) :edx)
	       pb-pb-andc1-loop
		(:movl (:ebx :edx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))
		       :ecx)
		(:notl :ecx)
		(:andl :ecx
		       (:eax :edx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0)))
		(:subl 4 :edx)
		(:jnc 'pb-pb-andc1-loop)))))))
    (do-it)))


(defun logandc2 (integer1 integer2)
  (logandc1 integer2 integer1))

(defun logior (&rest integers)
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (number-double-dispatch (x y)
	((fixnum fixnum)
	 (with-inline-assembly (:returns :eax)
	   (:compile-two-forms (:eax :ebx) x y)
	   (:orl :ebx :eax)))
	((positive-fixnum positive-bignum)
	 (macrolet
	     ((do-it ()
		`(let ((r (copy-bignum y)))
		   (with-inline-assembly (:returns :eax)
		     (:compile-two-forms (:eax :ecx) r x)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:orl :ecx (:eax ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))))))
	   (do-it)))
	((positive-bignum positive-fixnum)
	 (macrolet
	     ((do-it ()
		`(let ((r (copy-bignum x)))
		   (with-inline-assembly (:returns :eax)
		     (:compile-two-forms (:eax :ecx) r y)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:orl :ecx (:eax ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))))))
	   (do-it)))
	((positive-bignum positive-bignum)
	 (if (< (%bignum-bigits x) (%bignum-bigits y))
	     (logior y x)
	   (let ((r (copy-bignum x)))
	     (macrolet
		 ((do-it ()
		    `(with-inline-assembly (:returns :eax)
		       (:compile-two-forms (:eax :ebx) r y)
		       (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
				:ecx)
		       (:leal ((:ecx 1) ,(* -1 movitz:+movitz-fixnum-factor+))
			      :edx)	; EDX is loop counter
		      or-loop
		       (:movl (:ebx :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
			      :ecx)
		       (:orl :ecx
			     (:eax :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		       (:subl 4 :edx)
		       (:jnc 'or-loop))))
	       (do-it)))))))
   (t (&rest integers)
      (declare (dynamic-extent integers))
      (if (null integers)
	  0
	(reduce #'logior integers)))))

(define-compiler-macro logior (&whole form &rest integers &environment env)
  (let ((constant-folded-integers (loop for x in integers
				      with folded-constant = 0
				      if (and (movitz:movitz-constantp x env)
					      (not (zerop (movitz:movitz-eval x env))))
				      do (setf folded-constant
					   (logior folded-constant (movitz:movitz-eval x env)))
				      else collect x into non-constants
				      finally (return (if (= 0 folded-constant)
							  non-constants
							(cons folded-constant non-constants))))))
    (case (length constant-folded-integers)
      (0 0)
      (1 (first constant-folded-integers))
      (2 `(no-macro-call logior ,(first constant-folded-integers) ,(second constant-folded-integers)))
      (t `(logior (logior ,(first constant-folded-integers) ,(second constant-folded-integers))
		  ,@(cddr constant-folded-integers))))))

(defun logxor (&rest integers)
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (number-double-dispatch (x y)
	((fixnum fixnum)
	 (with-inline-assembly (:returns :eax)
	   (:compile-two-forms (:eax :ebx) x y)
	   (:xorl :ebx :eax)))
	(((eql 0) t) y)
	((t (eql 0)) x)
	((positive-fixnum positive-bignum)
	 (macrolet
	     ((do-it ()
		`(with-inline-assembly (:returns :eax)
		   (:compile-two-forms (:eax :ecx) (copy-bignum y) x)
		   (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:xorl :ecx
			  (:eax ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))))))
	   (do-it)))
	((positive-bignum positive-fixnum)
	 (macrolet
	     ((do-it ()
		`(with-inline-assembly (:returns :eax)
		   (:compile-two-forms (:eax :ecx) (copy-bignum x) y)
		   (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:xorl :ecx
			  (:eax ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))))))
	   (do-it)))
	((positive-bignum positive-bignum)
	 (if (< (%bignum-bigits x) (%bignum-bigits y))
	     (logxor y x)
	   (let ((r (copy-bignum x)))
	     (macrolet
		 ((do-it ()
		    `(%bignum-canonicalize
		      (with-inline-assembly (:returns :eax)
			(:compile-two-forms (:eax :ebx) r y)
			(:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
				 :ecx)
			(:leal ((:ecx 1),(* -1 movitz:+movitz-fixnum-factor+))
			       :edx)	; EDX is loop counter
		       xor-loop
			(:movl (:ebx :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
			       :ecx)
			(:xorl :ecx
			       (:eax :edx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
			(:subl 4 :edx)
			(:jnc 'xor-loop)
			))))
	       (do-it)))))))
   (t (&rest integers)
      (declare (dynamic-extent integers))
      (if (null integers)
	  0
	(reduce #'logxor integers)))))

(defun lognot (integer)
  (check-type integer fixnum)
  (with-inline-assembly (:returns :eax)
    (:compile-form (:result-mode :eax) integer)
    (:xorl #.(cl:- #xffffffff movitz::+movitz-fixnum-zmask+) :eax)))

(defun ldb%byte (size position integer)
  "This is LDB with explicit byte-size and position parameters."
  (check-type size positive-fixnum)
  (check-type position positive-fixnum)
  (etypecase integer
    (fixnum
     (macrolet
	 ((do-it ()
	    `(with-inline-assembly (:returns :eax)
	       (:compile-two-forms (:eax :ecx) integer position)
	       (:cmpl ,(* (1- movitz:+movitz-fixnum-bits+) movitz:+movitz-fixnum-factor+)
		      :ecx)
	       (:ja '(:sub-program (outside-fixnum)
		      (:addl #x80000000 :eax) ; sign into carry
		      (:sbbl :ecx :ecx)
		      (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
		      (:jmp 'mask-fixnum)))
	       (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:std)			; <================= STD
	       (:sarl :cl :eax)		; shift..
	       (:andl ,(logxor #xffffffff movitz:+movitz-fixnum-zmask+) :eax)
	       (:cld)			; =================> CLD
	      mask-fixnum
	       (:compile-form (:result-mode :ecx) size)
	       (:cmpl ,(* (1- movitz:+movitz-fixnum-bits+) movitz:+movitz-fixnum-factor+)
		      :ecx)
	       (:jna 'fixnum-result)
	       (:testl :eax :eax)
	       (:jns 'fixnum-done)
	       ;; We need to generate a bignum..
	       ;; ..filling in 1-bits since the integer is negative.
	       (:pushl :eax)		; This will become the LSB bigit.
	      retry-ones-expanded-bignum
	       (:declare-label-set retry-jumper-ones-expanded-bignum (retry-ones-expanded-bignum))
	       ;; Calculate word-size from bytespec-size.
	       (:compile-form (:result-mode :ecx) size)
	       (:subl ,movitz:+movitz-fixnum-factor+ :ecx) ; Subtract 1
	       (:shrl ,(+ 5 movitz:+movitz-fixnum-shift+) :ecx) ; Divide by 32
	       (:leal ((:ecx ,movitz:+movitz-fixnum-factor+) ; Add 1 for index->size..
		       ,(* 2 movitz:+movitz-fixnum-factor+)) ; ..and 1 for header.
		      :eax)
	       (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
	       (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
				  'retry-jumper-ones-expanded-bignum)
				(:edi (:edi-offset atomically-status))))
	       (:call-global-constant get-cons-pointer)
	       (:shll 16 :ecx)
	       (:addl ,(dpb 1 (byte 16 16) (movitz:tag :bignum 0)) :ecx) ; add 1 for index->size
	       (:movl :ecx (:eax ,movitz:+other-type-offset+))
	       (:shrl 16 :ecx)
	       (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)
		       ,(* 1 movitz:+movitz-fixnum-factor+)) ; add 1 for header.
		      :ecx)
	       (:call-global-constant cons-commit)
	       (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				(:edi (:edi-offset atomically-status))))
	       ;; Have fresh bignum in EAX, now fill it with ones.
	       (:xorl :ecx :ecx)	; counter
	      fill-ones-loop
	       (:movl #xffffffff
		      (:eax (:ecx 1) ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
	       (:addl 4 :ecx)
	       (:cmpw :cx (:eax ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::length)))
	       (:jne 'fill-ones-loop)
	       
	       (:popl :ecx)		; The LSB bigit.
	       (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:movl :ecx (:eax ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
	       (:movl :eax :ebx)
	       ;; Compute MSB bigit mask in EDX
	       (:compile-form (:result-mode :ecx) size)
	       (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:std)			; <================= STD
	       (:xorl :edx :edx)
	       (:andl 31 :ecx)
	       (:jz 'fixnum-mask-ok)
	       (:addl 1 :edx)
	       (:shll :cl :edx)
	      fixnum-mask-ok
	       (:subl 1 :edx)
	       (:movzxw (:ebx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::length))
			:ecx)
	       (:andl :edx		; And EDX with the MSB bigit.
		      (:ebx (:ecx 1) ,(+ -4 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))))
	       (:movl :edi :edx)
	       (:movl :edi :eax)
	       (:cld)			; =================> CLD
	       (:movl :ebx :eax)
	       (:jmp 'fixnum-done)
	       
	      fixnum-result
	       (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:movl ,movitz:+movitz-fixnum-factor+ :edx) ; generate fixnum mask in EDX
	       (:shll :cl :edx)
	       (:subl ,movitz:+movitz-fixnum-factor+ :edx)
	       (:andl :edx :eax)
	       (:jmp 'fixnum-done)
	      fixnum-done
	       )))
       (do-it)))
    (positive-bignum
     (cond
      ((<= size 32)
       ;; The result is likely to be a fixnum (or at least an u32), due to byte-size.
       (macrolet
	   ((do-it ()
	      `(with-inline-assembly (:returns :eax)
		 (:compile-form (:result-mode :ebx) integer)
		 (:compile-form (:result-mode :eax) position)
		 (:movl :eax :ecx)	; compute bigit-number in ecx
		 (:sarl 5 :ecx)
		 (:andl -4 :ecx)
		 (:addl 4 :ecx)
		 (:cmpl #x4000 :ecx)
		 (:jae 'position-outside-integer)
		 (:cmpw :cx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		 (:jc '(:sub-program (position-outside-integer)
			(:movsxb (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::sign)) :ecx)
			(:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
			(:jmp 'done-u32)))
		 (:std)
		 (:movl (:ebx (:ecx 1) ,(+ -4 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
			:eax)
		 (:movl 0 :edx)		; If position was in last bigit.. (don't touch EFLAGS)
		 (:je 'no-top-bigit)	; ..we must zero-extend rather than read top bigit.
		 (:movl (:ebx (:ecx 1) ,(+ 0 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
			:edx)		; Read top bigit into EDX
		no-top-bigit
		 (:testl #xff00 (:ebx ,movitz:+other-type-offset+))
		 (:jnz '(:sub-program (negative-bignum)
			 ;; We must negate the bigits..
			 (:break)
			 ))
		edx-eax-ok
		 ;; EDX:EAX now holds the number that must be shifted and masked.
		 (:compile-form (:result-mode :ecx) position)
		 (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		 (:shrdl :cl :edx :eax)	; Shifted value into EAX
		 (:compile-form (:result-mode :ecx) size)
		 (:xorl :edx :edx)	; Generate a mask in EDX
		 (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		 (:testl 31 :ecx)
		 (:jz 'mask-ok-u32)
		 (:addl 1 :edx)
		 (:shll :cl :edx)
		mask-ok-u32
		 (:subl 1 :edx)
		 (:andl :edx :eax)
		 (:movl :eax :ecx)	; For boxing..
		 (:movl :edi :eax)
		 (:movl :edi :edx)
		 (:cld)
		 ;; See if we can return same bignum..
		 (:cmpl ,(dpb movitz:+movitz-fixnum-factor+
			      (byte 16 16) (movitz:tag :bignum 0))
			(:ebx ,movitz:+other-type-offset+))			     
		 (:jne 'cant-return-same)
		 (:cmpl :ecx (:ebx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		 (:jne 'cant-return-same)
		 (:movl :ebx :eax)
		 (:jmp 'done-u32)
		cant-return-same
		 (:call-global-constant box-u32-ecx)
		done-u32
		 )))
	 (do-it)))
      (t (macrolet
	     ((do-it ()
		`(let ()
		   (with-inline-assembly (:returns :eax)
		     (:compile-form (:result-mode :ebx) integer)
		     (:compile-form (:result-mode :ecx) position)
		     (:shrl 5 :ecx) ; compute fixnum bigit-number in ecx
		     (:cmpl #x4000 :ecx)
		     (:jnc 'position-outside-integer)
		     (:cmpw :cx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:jbe '(:sub-program (position-outside-integer)
			     (:movsxb (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::sign)) :ecx)
			     (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
			     (:jmp 'done-u32)))
		     
		     (:compile-two-forms (:edx :ecx) position size)
		     (:movl :ecx :eax)	; keep size/fixnum in EAX.
		     (:addl :edx :ecx)
		     (:into)		; just to make sure
		     (:shrl 5 :ecx)	; compute msb bigit index/fixnum in ecx
		     (:addl 4 :ecx)
		     (:cmpw :cx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (je '(:sub-program (equal-size-maybe-return-same)
			   (:testl :edx :edx) ; Can only return same if (zerop position).
			   (:jnz 'adjust-size)
			   (:movl :eax :ecx) ; size/fixnum
			   (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
			   (:andl 31 :ecx)
			   (:jz 'yes-return-same)
			   (:std)	; <================
			   ;; we know EDX=0, now generate mask in EDX
			   (:addl 1 :edx)
			   (:shll :cl :edx)
			   (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			    :ecx)
			   (:cmpl :edx (:ebx (:ecx 1)
					,(+ -4 (bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))))
			   (:movl 0 :edx) ; Safe value, and correct if we need to go to adjust-size.
			   (:cld)	; =================>
			   (:jnc 'adjust-size) ; nope, we have to generate a new bignum.
			   yes-return-same
			   (:movl :ebx :eax) ; yep, we can return same bignum.
			   (:jmp 'ldb-done)))
		     (:jnc 'size-ok)
		     ;; We now know that (+ size position) is beyond the size of the bignum.
		     ;; So, if (zerop position), we can return the bignum as our result.
		     (:testl :edx :edx)
		     (:jz '(:sub-program ()
			    (:movl :ebx :eax) ; return the source bignum.
			    (:jmp 'ldb-done)))
		    adjust-size
		     ;; The bytespec is (partially) outside source-integer, so we make the
		     ;; size smaller before proceeding. new-size = (- source-int-length position)
		     (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			      :ecx)	; length of source-integer
		     (:shll 5 :ecx)	; fixnum bit-position
		     (:xorl :eax :eax)	; In case the new size is zero.
		     (:subl :edx :ecx)	; subtract position
		     (:js '(:sub-program (should-not-happen)
			    ;; new size should never be negative.
			    (:break)))
		     (:jz 'ldb-done)	; New size was zero, so the result of ldb is zero.
		     (:movl :ecx :eax)	; New size into EAX.
		    size-ok
		    retry
		     (:declare-label-set retry-jumper (retry))
		     (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		     (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
					'retry-jumper)
				      (:edi (:edi-offset atomically-status))))
		     ;; (new) Size is in EAX.
		     (:pushl :eax)	; Save for later
		     (:subl ,movitz:+movitz-fixnum-factor+ :eax)
		     (:andl ,(logxor #xffffffff
				     (mask-field (byte (+ 5 movitz:+movitz-fixnum-shift+) 0) -1))
			    :eax)
		     (:shrl 5 :eax)	; Divide (size-1) by 32 to get number of bigits-1
		     ;; Now add 1 for index->size, 1 for header, and 1 for tmp storage before shift.
		     (:addl ,(* 3 movitz:+movitz-fixnum-factor+) :eax)
		     (:pushl :eax)
		     (:call-global-constant get-cons-pointer)
		     ;; (:store-lexical (:lexical-binding r) :eax :type t)
		     (:popl :ecx)
		     (:subl ,(* 2 movitz:+movitz-fixnum-factor+) :ecx) ; for tmp storage and header.
		     (:shll 16 :ecx)
		     (:orl ,(movitz:tag :bignum 0) :ecx)
		     (:movl :ecx (:eax ,movitz:+other-type-offset+))
		     (:compile-form (:result-mode :ebx) integer)
		     
		     (:xchgl :eax :ebx)
		     ;; now: EAX = old integer, EBX = new result bignum
		     
		     ;; Edge case: When size(old)=size(new), the tail-tmp must be zero.
		     ;; We check here, setting the tail-tmp to a mask for and-ing below.
		     (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			      :ecx)	; length of source-integer
		     ;; Initialize tail-tmp to #xffffffff, meaning copy from source-integer.
		     (:movl #xffffffff
			    (:ebx (:ecx 1) ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		     (:cmpw :cx (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:jc '(:sub-program (result-too-big-shouldnt-happen)
			    (:break)))
		     (:jne 'tail-tmp-ok)
		     ;; Sizes was equal, so set tail-tmp to zero.
		     (:movl 0 (:ebx (:ecx 1) ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		    tail-tmp-ok
		     ;; Now copy the relevant part of the integer
		     (:std)
		     (:compile-form (:result-mode :ecx) position)
		     (:sarl ,(+ 5 movitz:+movitz-fixnum-shift+) :ecx) ; compute bigit-number in ecx
		     ;; We can use primitive pointers because we're both inside atomically and std.
		     (:leal (:eax (:ecx 4) ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
			    :eax)	; Use EAX as primitive pointer into source
		     (:xorl :ecx :ecx)	; counter
		    copy-integer
		     (:movl (:eax) :edx)
		     (:addl 4 :eax)
		     (:movl :edx (:ebx (:ecx 1) ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		     (:addl 4 :ecx)
		     (:cmpw :cx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:jne 'copy-integer)
		     ;; Copy one more than the length, namely the tmp at the end.
		     ;; Tail-tmp was initialized to a bit-mask above.
		     (:movl (:eax) :edx)
		     (:andl :edx (:ebx (:ecx 1) ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		     ;; Copy done, now shift
		     (:compile-form (:result-mode :ecx) position)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:andl 31 :ecx)
		     (:jz 'shift-done)	; if (zerop (mod position 32)), no shift needed.
		     (:xorl :edx :edx)	; counter
		    shift-loop
		     (:movl (:ebx (:edx 1) ,(+ 4 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
			    :eax)	; Next bigit into eax
		     (:shrdl :cl :eax	; Now shift bigit, with msbs from eax.
			     (:ebx (:edx 1) ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		     (:addl 4 :edx)
		     (:cmpw :dx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:jne 'shift-loop)
		    shift-done
		     ;; Now we must mask MSB bigit.
		     (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			      :edx)
		     (:popl :ecx)	; (new) bytespec size
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:andl 31 :ecx)
		     (:jz 'mask-done)
		     (:movl 1 :eax)	; Generate mask in EAX
		     (:shll :cl :eax)
		     (:subl 1 :eax)
		     (:andl :eax
			    (:ebx (:edx 1) ,(+ -4 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))))
		    mask-done
		     ;; (:movl :edi :edx)	; safe EDX
		     (:movl :edi :eax)	; safe EAX
		     (:cld)
		     ;; Now we must zero-truncate the result bignum in EBX.
		     (:movzxw (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length))
			      :ecx)
		    zero-truncate-loop
		     (:cmpl 0 (:ebx (:ecx 1)
				    ,(+ -4 (bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))))
		     (:jne 'zero-truncate-done)
		     (:subl 4 :ecx)
		     (:jnz 'zero-truncate-loop)
		     ;; Zero bigits means the entire result collapsed to zero.
		     (:xorl :eax :eax)
		     (:jmp 'return-fixnum) ; don't commit the bignum allocation.
		    zero-truncate-done
		     (:cmpl 4 :ecx)	; If result size is 1, the result might have..
		     (:jne 'complete-bignum-allocation) ; ..collapsed to a fixnum.
		     (:cmpl ,movitz:+movitz-most-positive-fixnum+
			    (:ebx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0)))
		     (:ja 'complete-bignum-allocation)
		     (:movl (:ebx ,(bt:slot-offset 'movitz:movitz-bignum 'movitz::bigit0))
			    :ecx)
		     (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
		     (:jmp 'return-fixnum)
		    complete-bignum-allocation
		     (:movw :cx (:ebx ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::length)))
		     (:movl :ebx :eax)
		     (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			    :ecx)
		     (:call-global-constant cons-commit)
		    return-fixnum
		     (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				      (:edi (:edi-offset atomically-status))))
		    ldb-done))))
	   (do-it)))))))
    

(define-compiler-macro ldb%byte (&whole form &environment env size position integer)
  "This is LDB with explicit byte-size and position parameters."
  (cond
   ((and (movitz:movitz-constantp size env)
	 (movitz:movitz-constantp position env)
	 (movitz:movitz-constantp integer env))
    (ldb (byte (movitz:movitz-eval size env)
	       (movitz:movitz-eval position env))
	 (movitz:movitz-eval integer env))) ; constant folding
   ((and (movitz:movitz-constantp size env)
	 (movitz:movitz-constantp position env))
    (let* ((size (movitz:movitz-eval size env))
	   (position (movitz:movitz-eval position env))
	   (result-type `(unsigned-byte ,size)))
      (cond
       ((or (minusp size) (minusp position))
	(error "Negative byte-spec for ldb."))
       ((= 0 size)
	`(progn ,integer 0))
       ((<= (+ size position) (- 31 movitz:+movitz-fixnum-shift+))
	`(with-inline-assembly (:returns :register
					 :type ,result-type)
	   (:compile-form (:result-mode :eax) ,integer)
	   (:call-global-constant unbox-u32)
	   (:andl ,(mask-field (byte size position) -1) :ecx)
	   ,@(unless (zerop position)
	       `((:shrl ,position :ecx)))
	   (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) (:result-register))))
       ((<= (+ size position) 32)
	`(with-inline-assembly-case (:type ,result-type)
	   (do-case (t :eax :labels (nix done))
	     (:compile-form (:result-mode :eax) ,integer)
	     ,@(cond
		((and (= 0 position) (= 32 size))
		 ;; If integer is a positive bignum with one bigit, return it.
		 `((:leal (:eax ,(- (movitz:tag :other))) :ecx)
		   (:testb 7 :cl)
		   (:jnz 'nix)
		   (:cmpl ,(dpb 4 (byte 16 16) (movitz:tag :bignum 0))
			  (:eax ,movitz:+other-type-offset+))
		   (:je 'done)))
		((and (= 0 position) (<= (- 32 movitz:+movitz-fixnum-shift+) size ))
		 `((:leal (:eax ,(- (movitz:tag :other))) :ecx)
		   (:testb 7 :cl)
		   (:jnz 'nix)
		   (:cmpl ,(dpb 4 (byte 16 16) (movitz:tag :bignum 0))
			  (:eax ,movitz:+other-type-offset+))
		   (:jne 'nix)
		   (:movl (:eax ,(bt:slot-offset 'movitz::movitz-bignum 'movitz::bigit0))
			  :ecx)
		   (:testl ,(logxor #xffffffff (mask-field (byte size 0) -1))
			   :ecx)
		   (:jz 'done)
		   (:andl ,(mask-field (byte size 0) -1)
			  :ecx)
		   (:call-global-constant box-u32-ecx)
		   (:jmp 'done))))
	    nix
	     (:call-global-constant unbox-u32)
	     ,@(unless (= 32 (- size position))
		 `((:andl ,(mask-field (byte size position) -1) :ecx)))
	     ,@(unless (zerop position)
		 `((:shrl ,position :ecx)))
	     (:call-global-constant box-u32-ecx)
	    done)))
       (t form))))
   (t form)))

(defun ldb (bytespec integer)
  (ldb%byte (byte-size bytespec) (byte-position bytespec) integer))

(define-compiler-macro ldb (&whole form &environment env bytespec integer)
  (let ((bytespec (movitz::movitz-macroexpand bytespec env)))
    (if (not (and (consp bytespec) (eq 'byte (car bytespec))))
	form
      `(ldb%byte ,(second bytespec) ,(third bytespec) ,integer))))

(define-setf-expander ldb (bytespec int &environment env)
  "Stolen from the Hyperspec example in the define-setf-expander entry."
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)	;Get setf expansion for int.
    (let ((btemp (gensym))		;Temp var for byte specifier.
	  (store (gensym))		;Temp var for byte to store.
	  (stemp (first stores)))	;Temp var for int to store.
      (if (cdr stores) (error "Can't expand this."))
      ;; Return the setf expansion for LDB as five values.
      (values (cons btemp temps)	;Temporary variables.
	      (cons bytespec vals)	;Value forms.
	      (list store)		;Store variables.
	      `(let ((,stemp (dpb ,store ,btemp ,access-form)))
		 ,store-form
		 ,store)		;Storing form.
	      `(ldb ,btemp ,access-form) ;Accessing form.
              ))))


(defun ldb-test (bytespec integer)
  (case (byte-size bytespec)
    (0 nil)
    (1 (logbitp (byte-position bytespec) integer))
    (t (/= 0 (ldb bytespec integer)))))

(defun logtest (integer-1 integer-2)
  "=> generalized-boolean"
  (not (= 0 (logand integer-1 integer-2))))

(defun dpb (newbyte bytespec integer)
  (logior (mask-field bytespec (ash newbyte (byte-position bytespec)))
	  (logandc2 integer (mask-field bytespec -1))))

(defun mask-field (bytespec integer)
  (ash (ldb bytespec integer) (byte-position bytespec)))

(defun deposit-field (newbyte bytespec integer)
  (logior (mask-field bytespec newbyte)
	  (logandc2 integer (mask-field bytespec -1))))

;;;

(defun plus-if (x y)
  (if (integerp x) (+ x y) x))

(defun minus-if (x y)
  (if (integerp x) (- x y) x))

(defun gcd (&rest numbers)
  (numargs-case
   (1 (u) u)
   (2 (u v)
      ;; Code borrowed from CMUCL.
      (do ((k 0 (1+ k))
	   (u (abs u) (truncate u 2))
	   (v (abs v) (truncate v 2)))
	  ((or (oddp u) (oddp v))
	   (do ((temp (if (oddp u)
			  (- v)
			(truncate u 2))
		      (truncate temp 2)))
	       (nil)
	     (when (oddp temp)
	       (if (plusp temp)
		   (setq u temp)
		 (setq v (- temp)))
	       (setq temp (- u v))
	       (when (zerop temp)
		 (return (ash u k))))))))
   (t (&rest numbers)
      (declare (dynamic-extent numbers))
      (do ((gcd (car numbers)
		(gcd gcd (car rest)))
	   (rest (cdr numbers) (cdr rest)))
	  ((null rest) gcd)))))

(defun floor (n &optional (divisor 1))
  "This is floor written in terms of truncate."
  (numargs-case
   (1 (n) n)
   (2 (n divisor)
      (multiple-value-bind (q r)
	  (truncate n divisor)
	(cond
	 ((<= 0 q)
	  (values q r))
	 ((= 0 r)
	  (values q 0))
	 (t (values (1- q) (+ r divisor))))))
   (t (n &optional (divisor 1))
      (floor n divisor))))

(defun isqrt (natural)
  "=> natural-root"
  (etypecase natural
    ((eql 0) 0)
    ((integer 1 *)
     (let ((r 1))
       (do ((next-r (truncate (+ r (truncate natural r)) 2)
		    (truncate (+ r (truncate natural r)) 2)))
	   ((typep (- next-r r) '(integer 0 1))
	    (let ((r+1 (1+ r)))
	      (if (<= (* r+1 r+1) natural)
		  r+1
		r)))
	 (setf r next-r))))))

(define-compiler-macro expt (&whole form base-number power-number &environment env)
  (if (not (and (movitz:movitz-constantp base-number env)
		(movitz:movitz-constantp power-number env)))
      form
    (expt (movitz:movitz-eval base-number env)
	  (movitz:movitz-eval power-number env))))
    

(defun expt (base-number power-number)
  "Take base-number to the power-number."
  (do ((i 0 (1+ i))
       (r 1 (* r base-number)))
      ((>= i power-number) r)))
  
