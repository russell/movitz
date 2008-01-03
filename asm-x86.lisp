;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2007 Frode V. Fjeld
;;;; 
;;;; Description:   x86 assembler for 32 and 64-bit.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(defpackage asm-x86
  (:use :common-lisp :asm))

(in-package asm-x86)

(defvar *symtab* nil)
(defvar *cpu-mode* :32-bit)
(defvar *pc* nil "Current program counter.")

(defvar *instruction-encoders*
  (make-hash-table :test 'eq))

(defun prefix-lookup (prefix-name)
  (cdr (or (assoc prefix-name
		  '((:operand-size-override . #x66)
		    (:address-size-override . #x67)
		    (:16-bit-operand . #x66)
		    (:16-bit-address . #x67)
		    (:lock . #xf0)
		    (:repne . #xf2)
		    (:repz . #xf3)
		    (:cs-override . #x2e)
		    (:ss-override . #x36)
		    (:ds-override . #x3e)
		    (:es-override . #x26)
		    (:fs-override . #x64)
		    (:gs-override . #x65)))
	   (error "There is no instruction prefix named ~S." prefix-name))))

(defun rex-encode (rexes &key rm)
  (let ((rex (logior (if (null rexes)
			 0
			 (+ #x40 (loop for rex in rexes
				    sum (ecase rex
					  (:rex.w #b1000)
					  (:rex.r #b0100)
					  (:rex.x #b0010)
					  (:rex.b #b0001)))))
		     (if (not rm)
			 0
			 (ldb (byte 1 3) rm)))))
    (unless (zerop rex)
      (list rex))))



(deftype octet ()
  '(unsigned-byte 8))

(deftype xint (size)
  `(or (unsigned-byte ,size)
       (signed-byte ,size)))

(deftype uint (size)
  `(unsigned-byte ,size))

(deftype sint (size)
  `(signed-byte ,size))

(defun integer-to-octets (i n)
  "Return list of n octets, encoding i in signed little endian."
  (loop for b from 0 below (* 8 n) by 8
     collect (ldb (byte 8 b) i)))

(defun encode-instruction (instruction &key
			   ((:symtab *symtab*) *symtab*)
			   ((:cpu-mode *cpu-mode*) *cpu-mode*))
  "Return list of octets,"
  (multiple-value-bind (prefixes rexes opcode mod reg rm scale index base displacement immediate operand-size address-size)
      (encode-to-parts instruction)
    (unless opcode
      (error "Unable to encode instruction ~S." instruction))
    (when (or (and (eq address-size :32-bit)
		   (eq *cpu-mode* :64-bit))
	      (and (eq address-size :16-bit)
		   (eq *cpu-mode* :32-bit))
              (and (eq address-size :64-bit)
		   (eq *cpu-mode* :32-bit))
	      (and (eq address-size :32-bit)
		   (eq *cpu-mode* :16-bit)))
      (pushnew :address-size-override
	       prefixes))
    (when (or (and (eq operand-size :16-bit)
		   (eq *cpu-mode* :64-bit))
	      (and (eq operand-size :16-bit)
		   (eq *cpu-mode* :32-bit))
	      (and (eq operand-size :32-bit)
		   (eq *cpu-mode* :16-bit)))
      (pushnew :operand-size-override
	       prefixes))
    (append (mapcar #'prefix-lookup (reverse prefixes))
	    (rex-encode rexes :rm rm)
            (when (< 8(integer-length opcode))
              (list (ldb (byte 8 8) opcode)))
	    (list (ldb (byte 8 0) opcode))
	    (when (or mod reg rm)
	      (assert (and mod reg rm) (mod reg rm)
		      "Either all or none of mod, reg, and rm must be defined. mod=~S, reg=~S, rm=~S." mod reg rm)
	      (check-type mod (unsigned-byte 2))
	      (list (logior (ash (ldb (byte 2 0) mod)
				 6)
			    (ash (ldb (byte 3 0) reg)
				 3)
			    (ash (ldb (byte 3 0) rm)
				 0))))
	    (when (or scale index base)
	      (assert (and scale index base) (scale index base)
		      "Either all or none of scale, index, and base must be defined. scale=~S, index=~S, base=~S." scale index base)
	      (check-type scale (unsigned-byte 2))
	      (check-type index (unsigned-byte 4))
	      (check-type base (unsigned-byte 4))
	      (list (logior (ash (ldb (byte 2 0) scale)
				 6)
			    (ash (ldb (byte 3 0) index)
				 3)
			    (ash (ldb (byte 3 0) base)
				 0))))
 	    displacement
	    immediate)))

(defmacro merge-encodings (form1 form2)
  `(multiple-value-bind (prefixes1 rexes1 opcode1 mod1 reg1 rm1 scale1 index1 base1 displacement1 immediate1 operand-size1 address-size1)
       ,form1
     (multiple-value-bind (prefixes2 rexes2 opcode2 mod2 reg2 rm2 scale2 index2 base2 displacement2 immediate2 operand-size2 address-size2)
	 ,form2
       (macrolet ((getone (a b name)
		    `(cond
		       ((and ,a ,b)
			(error "~A doubly defined: ~S and ~S." ',name ,a ,b))
		       (,a)
		       (,b))))
	 (encoded-values :prefixes (append prefixes1 prefixes2)
			 :rex (append (if (listp rexes1)
					  rexes1
					  (list rexes1))
				      (if (listp rexes2)
					  rexes2
					  (list rexes2)))
			 :opcode (getone opcode1 opcode2 opcode)
			 :mod (getone mod1 mod2 mod)
			 :reg (getone reg1 reg2 reg)
			 :rm (getone rm1 rm2 rm)
			 :scale (getone scale1 scale2 scale)
			 :index (getone index1 index2 index)
			 :base (getone base1 base2 base)
			 :displacement (getone displacement1 displacement2 displacement)
			 :immediate (getone immediate1 immediate2 immediate)
			 :operand-size (getone operand-size1 operand-size2 operand-size)
			 :address-size (getone address-size1 address-size2 address-size))))))



(defun encoded-values (&key prefixes prefix rex opcode mod reg rm scale index base displacement immediate operand-size address-size)
  (values (append (when prefix
		    (list prefix))
		  prefixes)
	  (if (keywordp rex)
	      (list rex)
	      rex)
	  opcode
	  mod reg rm
	  scale index base
	  displacement
	  immediate
	  operand-size
	  address-size))
 
(defun encode-to-parts (instruction)
  (multiple-value-bind (legacy-prefixes instruction)
      (if (listp (car instruction))
	  (values (car instruction)
		  (cdr instruction))
	  (values nil
		  instruction))
    (destructuring-bind (operator &rest operands)
	instruction
      (multiple-value-bind (prefixes prefix rex opcode mod reg rm scale index base displacement immediate operand-size address-size)
	  (apply (or (gethash operator *instruction-encoders*)
		     (error "Unknown instruction operator ~S in ~S." operator instruction))
		 operands)
	(values (append legacy-prefixes prefixes)
		prefix
		rex
		opcode
		mod
		reg
		rm
		scale
		index
		base
		displacement
		immediate
		operand-size
		address-size)))))

(defmacro define-operator (operator lambda-list &body body)
  (check-type operator keyword)
  (let ((defun-name (intern (format nil "~A-~A" 'instruction-encoder operator))))
    `(progn
       (defun ,defun-name ,lambda-list
         (let ((operator-mode nil)
               (default-rex nil))
           (declare (ignorable operator-mode default-rex))
           (block operator
             ,@body)))
       (setf (gethash ',operator *instruction-encoders*)
	     ',defun-name)
       ',operator)))

(defmacro define-operator/8 (operator lambda-list &body body)
  `(define-operator ,operator ,lambda-list
     (let ((operator-mode :8-bit)
	   (default-rex nil))
       (declare (ignorable operator-mode default-rex))
       (macrolet ((yield (&rest args)
		    `(encoded-result :operand-size 8 ,@args)))
	 ,@body))))

(defmacro define-operator/16 (operator lambda-list &body body)
  `(define-operator ,operator ,lambda-list
     (let ((operator-mode :16-bit)
	   (default-rex nil))
       (macrolet ((yield (&rest args)
		    `(encoded-result :operand-size operator-mode ,@args)))
	 ,@body))))

(defmacro define-operator/32 (operator lambda-list &body body)
  `(define-operator ,operator ,lambda-list
     (let ((operator-mode :32-bit)
	   (default-rex nil))
       (declare (ignorable operator-mode))
       (macrolet ((yield (&rest args)
		    `(encoded-result :operand-size operator-mode ,@args)))
	 ,@body))))

(defmacro define-operator/64 (operator lambda-list &body body)
  `(define-operator ,operator ,lambda-list
     (let ((operator-mode :64-bit)
	   (default-rex '(:rex.w)))
       (declare (ignorable operator-mode))
       (macrolet ((yield (&rest args)
		    `(encoded-result :operand-size operator-mode ,@args)))
	 ,@body))))

(defmacro define-operator/64* (operator lambda-list &body body)
  `(define-operator ,operator ,lambda-list
     (let ((operator-mode :64-bit)
	   (default-rex (case *cpu-mode*
			  (:64-bit nil)
			  (t '(:rex.w)))))
       (declare (ignorable operator-mode))
       ,@body)))

(defmacro define-operator* ((&key |16| |32| |64|) args &body body)
  (let ((body16 (subst '(xint 16) :int-16-32-64
                       (subst :dx :dx-edx-rdx
                              (subst :ax :ax-eax-rax body))))
        (body32 (subst '(xint 32) :int-16-32-64
                       (subst :edx :dx-edx-rdx
                              (subst :eax :ax-eax-rax body))))
        (body64 (subst '(sint 32) :int-16-32-64
                       (subst :rdx :dx-edx-rdx
                              (subst :rax :ax-eax-rax body)))))
    `(progn
       ,(when |16|
              `(define-operator/16 ,|16| ,args ,@body16))
       ,(when |32|
              `(define-operator/32 ,|32| ,args ,@body32))
       ,(when |64|
              `(define-operator/64 ,|64| ,args ,@body64)))))
       
(defun resolve (x)
  (etypecase x
    (integer
     x)
    (symbol-reference
     (let ((s (symbol-reference-symbol x)))
       (cdr (or (assoc s *symtab*)
		(error 'unresolved-symbol 
		       :symbol s)))))))

(defun resolve-and-encode (x type &key size)
  (encode-integer (cond
		    ((typep x type)
		     x)
		    ((integerp x)
		     (error "Immediate value #x~X out of range for ~S." x type))
		    ((assoc x *symtab*)
		     (let ((value (cdr (assoc x *symtab*))))
		       (assert (typep value type))
		       value))
		    (t (error "Unresolved symbol ~S (size ~S)." x size)))
		  type))

(defun resolve-pc-relative (operand)
  (typecase operand
    ((cons (eql :pc+))
     (reduce #'+ (cdr operand)
      :key #'resolve))
    (symbol-reference
     (- (resolve operand) *pc*))))

(defun encode-integer (i type)
  (assert (typep i type))
  (let ((bit-size (cadr type)))
    (loop for b upfrom 0 below bit-size by 8
       collect (ldb (byte 8 b) i))))	 

(defun parse-indirect-operand (operand)
  (assert (indirect-operand-p operand))
  (let (reg offsets reg2 reg-scale)
    (dolist (expr operand)
      (etypecase expr
	(register-operand
	 (if reg
	     (setf reg2 expr)
	     (setf reg expr)))
	((cons register-operand
	       (cons (member 1 2 4 8) null))
	 (when reg
	   (assert (not reg-scale))
	   (setf reg2 reg))
	 (setf reg (first expr)
	       reg-scale (second expr)))
	(immediate-operand
	 (push expr offsets))
	((cons (eql :+))
	 (dolist (term (cdr expr))
	   (push term offsets)))))
    (when (and (eq reg2 :esp)
	       (or (not reg-scale)
		   (eql 1 reg-scale)))
      (psetf reg reg2
	     reg2 reg))
    (values reg offsets reg2 (if (not reg)
				 nil
				 (or reg-scale 1)))))


(defun encode-reg/mem (operand mode)
  (check-type mode (member nil :8-bit :16-bit :32-bit :64-bit :mm :xmm))
  (if (and mode (keywordp operand))
      (encoded-values :mod #b11
		      :rm (or (position operand (ecase mode
						  (:8-bit  '(:al :cl :dl :bl :ah :ch :dh :bh))
						  (:16-bit '(:ax :cx :dx :bx :sp :bp :si :di))
						  (:32-bit '(:eax :ecx :edx :ebx :esp :ebp :esi :edi))
						  (:64-bit '(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :13 :r14 :r15))
						  (:mm '(:mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7))
						  (:xmm '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7))))
			      (error "Unknown ~D-bit register ~S." mode operand)))
      (multiple-value-bind (reg offsets reg2 reg-scale)
	  (parse-indirect-operand operand)
	(check-type reg-scale (member nil 1 2 4 8))
	(assert (or (not reg2)
		    (and reg reg2)))
	(assert (or (not reg-scale)
		    (and reg reg-scale)))
	(let ((offset (reduce #'+ offsets
			      :key #'resolve)))
	  (cond
            ((and (not reg)
                  (eq mode :16-bit)
                  (typep offset '(xint 16)))
             (encoded-values :mod #b00
                             :rm #b110
                             :address-size :16-bit
                             :displacement (encode-integer offset '(xint 16))))
	    ((and (not reg)
		  (typep offset '(xint 32)))
	     (encoded-values :mod #b00
			     :rm #b101
                             :address-size :32-bit
			     :displacement (encode-integer offset '(xint 32))))
	    ((and (eq reg :sp)
		  (not reg2)
		  (= 1 reg-scale))
	     (etypecase offset
	       ((eql 0)
		(encoded-values :mod #b00
				:rm #b100
				:scale 0
				:index #b100
				:base #b100
				:address-size :16-bit))
	       ((sint 8)
		(encoded-values :mod #b01
				:rm #b100
				:displacement (encode-integer offset '(sint 8))
				:scale 0
				:index #b100
				:base #b100
				:address-size :16-bit))
	       ((xint 32)
		(encoded-values :mod #b10
				:rm #b100
				:displacement (encode-integer offset '(xint 32))
				:scale 0
				:index #b100
				:base #b100
				:address-size :16-bit))))
	    ((and (eq reg :esp)
		  (= 1 reg-scale))
	     (let ((reg2-index (or (position reg2 '(:eax :ecx :edx :ebx nil :ebp :esi :edi))
				   (error "Unknown reg2 [F] ~S." reg2))))
	       (etypecase offset
		 ((eql 0)
		  (encoded-values :mod #b00
				  :rm #b100
				  :scale 0
				  :index reg2-index
				  :base #b100
				  :address-size :32-bit))
		 ((sint 8)
		  (encoded-values :mod #b01
				  :rm #b100
				  :displacement (encode-integer offset '(sint 8))
				  :scale 0
				  :index reg2-index
				  :base #b100
				  :address-size :32-bit))
		 ((xint 32)
		  (encoded-values :mod #b10
				  :rm #b100
				  :displacement (encode-integer offset '(xint 32))
				  :scale 0
				  :index reg2-index
				  :base #b100
				  :address-size :32-bit)))))
	    ((and (eq reg :rsp)
		  (not reg2)
		  (= 1 reg-scale))
	     (etypecase offset
	       ((eql 0)
		(encoded-values :mod #b00
				:rm #b100
				:scale 0
				:index #b100
				:base #b100
				:address-size :64-bit))
	       ((sint 8)
		(encoded-values :mod #b01
				:rm #b100
				:displacement (encode-integer offset '(sint 8))
				:scale 0
				:index #b100
				:base #b100
				:address-size :64-bit))
	       ((sint 32)
		(encoded-values :mod #b10
				:rm #b100
				:displacement (encode-integer offset '(sint 32))
				:scale 0
				:index #b100
				:base #b100
				:address-size :64-bit))))
	    (t (multiple-value-bind (register-index map address-size)
		   (let* ((map32 '(:eax :ecx :edx :ebx :esp :ebp :esi :edi))
			  (index32 (position reg map32))
			  (map64 '(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))
			  (index64 (unless index32
				     (position reg map64))))
		     (if index32
			 (values index32 map32 :32-bit)
			 (values index64 map64 :64-bit)))
		 (cond
		   ((and (not reg2)
			 register-index
			 (= 1 reg-scale)
			 (zerop offset))
		    (encoded-values :mod #b00
				    :rm register-index
				    :address-size address-size))
		   ((and (not reg2)
			 register-index
			 (= 1 reg-scale)
			 (typep offset '(sint 8)))
		    (encoded-values :mod #b01
				    :rm register-index
				    :displacement (encode-integer offset '(sint 8))
				    :address-size address-size))
		   ((and (not reg2)
			 register-index
			 (= 1 reg-scale)
			 (or (typep offset '(sint 32))
			     (and (eq :32-bit address-size)
				  (typep offset '(xint 32)))))
		    (encoded-values :mod #b10
				    :rm register-index
				    :displacement (encode-integer offset '(sint 32))
				    :address-size address-size))
                   ((and (not reg2)
                         register-index
                         (if (eq :64-bit *cpu-mode*)
                             (typep offset '(sint 32))
                             (typep offset '(xint 32)))
                         (not (= #b100 register-index)))
                    (encoded-values :rm #b100
                                    :mod #b00
                                    :index register-index
                                    :base #b101
                                    :scale (or (position reg-scale '(1 2 4 8))
                                               (error "Unknown register scale ~S." reg-scale))
                                    :displacement (encode-integer offset '(xint 32))))
		   ((and reg2
			 register-index
			 (zerop offset)
			 (not (= register-index #b100)))
		    (encoded-values :mod #b00
				    :rm #b100
				    :scale (or (position reg-scale '(1 2 4 8))
                                               (error "Unknown register scale ~S." reg-scale))
				    :index register-index
				    :base (or (position reg2 map)
					      (error "unknown reg2 [A] ~S" reg2))
				    :address-size address-size))
		   ((and reg2
			 register-index
			 (typep offset '(sint 8))
			 (not (= register-index #b100)))
		    (encoded-values :mod #b01
				    :rm #b100
				    :scale (position reg-scale '(1 2 4 8))
				    :index register-index
				    :base (or (position reg2 map)
					      (error "unknown reg2 [B] ~S" reg2))
				    :address-size address-size
				    :displacement (encode-integer offset '(sint 8))))
		   ((and reg2
			 register-index
			 (eq :32-bit address-size)
			 (typep offset '(sint 8))
			 (not (= register-index #b100)))
		    (encoded-values :mod #b01
				    :rm #b100
				    :scale (position reg-scale '(1 2 4 8))
				    :index register-index
				    :base (or (position reg2 map)
					      (error "unknown reg2 [C] ~S." reg2))
				    :address-size address-size
				    :displacement (encode-integer offset '(sint 8))))
		   ((and reg2
			 register-index
			 (eq :32-bit address-size)
			 (typep offset '(xint 32))
			 (not (= register-index #b100)))
		    (encoded-values :mod #b10
				    :rm #b100
				    :scale (position reg-scale '(1 2 4 8))
				    :index register-index
				    :base (or (position reg2 map)
					      (error "unknown reg2 [D] ~S." reg2))
				    :address-size address-size
				    :displacement (encode-integer offset '(xint 32))))
		   ((and reg2
			 register-index
			 (eq :64-bit address-size)
			 (typep offset '(sint 32))
			 (not (= register-index #b100)))
		    (encoded-values :mod #b01
				    :rm #b100
				    :scale (position reg-scale '(1 2 4 8))
				    :index register-index
				    :base (or (position reg2 map)
					      (error "unknown reg2 [E] ~S" reg2))
				    :address-size address-size
				    :displacement (encode-integer offset '(sint 32))))
		   (t (let ((rm16 (position-if (lambda (x)
						 (or (and (eq (car x) reg)
							  (eq (cdr x) reg2))
						     (and (eq (car x) reg2)
							  (eq (cdr x) reg))))
					       '((:bx . :si) (:bx . :di) (:bp . :si) (:bp . :di)
						 (:si) (:di) (:bp) (:bx)))))
			(cond
			  ((and rm16
				(zerop offset)
				(not (= #b110 rm16)))
			   (encoded-values :mod #b00
					   :rm rm16
					   :address-size :16-bit))
			  ((and rm16
				(typep offset '(sint 8)))
			   (encoded-values :mod #b01
					   :rm rm16
					   :address-size :16-bit
					   :displacement (encode-integer offset '(sint 8))))
			  ((and rm16
				(typep offset '(xint 16)))
			   (encoded-values :mod #b10
					   :rm rm16
					   :address-size :16-bit
					   :displacement (encode-integer offset '(xint 16))))
			  (t (error "Huh? reg: ~S, reg2: ~S, scale: ~S, offset: ~S" reg reg2 reg-scale offset))
			  )))))))))))
		    


(defmacro encoded-result (&rest args &key prefixes prefix rex opcode mod reg rm scale index base displacement immediate operand-size address-size)
  (declare (ignore prefixes prefix rex opcode mod reg rm scale index base displacement immediate operand-size address-size))
  `(return-from operator (encoded-values ,@args)))

(defmacro imm (imm-operand opcode imm-type &rest extras)
  `(when (immediate-p ,imm-operand)
     (let ((immediate (resolve ,imm-operand)))
       (when (typep immediate ',imm-type)
	 (encoded-result :opcode ,opcode
			 :immediate (encode-integer immediate ',imm-type)
                         :operand-size operator-mode
			 :rex default-rex
			 ,@extras)))))

(defmacro imm-modrm (op-imm op-modrm opcode digit type)
  `(when (immediate-p ,op-imm)
     (let ((immediate (resolve ,op-imm)))
       (when (typep immediate ',type)
	 (return-from operator
	   (merge-encodings (encoded-values :opcode ,opcode
					    :reg ,digit
					    :operand-size operator-mode
					    :rex default-rex
					    :immediate (encode-integer immediate ',type))
			    (encode-reg/mem ,op-modrm operator-mode)))))))

(defmacro pc-rel (opcode operand type &rest extras)
  `(let ((offset (resolve-pc-relative ,operand)))
     (when (typep offset ',type)
       (return-from operator
	 (encoded-values :opcode ,opcode
			 :displacement (encode-integer offset ',type)
                         ,@extras)))))

(defmacro modrm (operand opcode digit)
  `(when (typep ,operand '(or register-operand indirect-operand))
     (return-from operator
       (merge-encodings (encoded-values :opcode ,opcode
                                        :reg ,digit
                                        :operand-size operator-mode
                                        :rex default-rex)
                        (encode-reg/mem ,operand operator-mode)))))

(defmacro reg-modrm (op-reg op-modrm opcode &rest extras)
  `(let* ((reg-map (ecase operator-mode
		     (:8-bit '(:al :cl :dl :bl :ah :ch :dh :bh))
		     (:16-bit '(:ax :cx :dx :bx :sp :bp :si :di))
		     (:32-bit '(:eax :ecx :edx :ebx :esp :ebp :esi :edi))
		     (:64-bit '(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))
		     (:mm '(:mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7 :mm8))
		     (:xmm '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7))))
	  (reg-index (position ,op-reg reg-map)))
     (when reg-index
       (return-from operator
	 (merge-encodings (encoded-values :opcode ,opcode
					  :reg reg-index
					  :operand-size operator-mode
					  :rex default-rex
                                          ,@extras)
			  (encode-reg/mem ,op-modrm operator-mode))))))

(defmacro sreg-modrm (op-sreg op-modrm opcode)
  `(let* ((reg-map '(:es :cs :ss :ds :fs :gs))
	  (reg-index (position ,op-sreg reg-map)))
     (when reg-index
       (return-from operator
	 (merge-encodings (encoded-values :opcode ,opcode
					  :reg reg-index
					  :rex default-rex)
			  (encode-reg/mem ,op-modrm operator-mode))))))

(defmacro moffset (opcode op-offset type)
  `(when (indirect-operand-p ,op-offset)
     (multiple-value-bind (reg offsets reg2)
	 (parse-indirect-operand ,op-offset)
       (when (and (not reg)
		  (not reg2))
	 (return-from operator
	   (encoded-values :opcode ,opcode
			   :displacement (encode-integer (reduce #'+ offsets
								 :key #'resolve)
							 ',type)))))))

(defmacro opcode (opcode &rest extras)
  `(return-from operator
     (encoded-values :opcode ,opcode
                     ,@extras
                     :operand-size operator-mode)))

(defmacro opcode* (opcode &rest extras)
  `(return-from operator
     (encoded-values :opcode ,opcode
                     ,@extras)))

(defmacro opcode-reg (opcode op-reg)
  `(let* ((reg-map (ecase operator-mode
		     (:8-bit '(:al :cl :dl :bl :ah :ch :dh :bh))
		     (:16-bit '(:ax :cx :dx :bx :sp :bp :si :di))
		     (:32-bit '(:eax :ecx :edx :ebx :esp :ebp :esi :edi))
		     (:64-bit '(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))
		     (:mm '(:mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7 :mm8))
		     (:xmm '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7))))
	  (reg-index (position ,op-reg reg-map)))
     (when reg-index
       (return-from operator
	 (encoded-values :opcode (+ ,opcode (ldb (byte 3 0) reg-index))
			 :operand-size operator-mode
			 :rex (cond
				((>= reg-index 8)
				 (assert (eq :64-bit operator-mode))
				 '(:rex.w :rex.r))
				(t default-rex)))))))

(defmacro opcode-reg-imm (opcode op-reg op-imm type)
  `(when (immediate-p ,op-imm)
     (let ((immediate (resolve ,op-imm)))
       (when (typep immediate ',type)
	 (let* ((reg-map (ecase operator-mode
			   (:8-bit '(:al :cl :dl :bl :ah :ch :dh :bh))
			   (:16-bit '(:ax :cx :dx :bx :sp :bp :si :di))
			   (:32-bit '(:eax :ecx :edx :ebx :esp :ebp :esi :edi))
			   (:64-bit '(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))
			   (:mm '(:mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7 :mm8))
			   (:xmm '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7))))
		(reg-index (position ,op-reg reg-map)))
	   (when reg-index
	     (return-from operator
	       (encoded-values :opcode (+ ,opcode (ldb (byte 3 0) reg-index))
			       :operand-size operator-mode
			       :immediate (encode-integer immediate ',type)
			       :rex (cond
				      ((>= reg-index 8)
				       (assert (eq :64-bit operator-mode))
				       '(:rex.w :rex.r))
				      (t default-rex))))))))))


;;;;;;;;;;;;;;;;

(define-operator :nop ()
  (opcode #x90))

;;;;;;;;;;; ADC

(define-operator/8 :adcb (src dst)
  (when (eq dst :al)
    (imm src #x14 (xint 8)))
  (imm-modrm src dst #x80 2 (xint 8))
  (reg-modrm dst src #x12)
  (reg-modrm src dst #x10))

(define-operator* (:16 :adcw :32 :adcl :64 :adcr) (src dst)
  (imm-modrm src dst #x83 2 (sint 8))
  (when (eq dst :ax-eax-rax)
    (imm src #x15 :int-16-32-64))
  (imm-modrm src dst #x81 2 :int-16-32-64)
  (reg-modrm dst src #x13)
  (reg-modrm src dst #x11))

;;;;;;;;;;; ADD

(define-operator/8 :addb (src dst)
  (when (eq dst :al)
    (imm src #x04 (xint 8)))
  (imm-modrm src dst #x80 0 (xint 8))
  (reg-modrm dst src #x02)
  (reg-modrm src dst #x00))

(define-operator* (:16 :addw :32 :addl :64 :addr) (src dst)
  (imm-modrm src dst #x83 0 (sint 8))
  (when (eq dst :ax-eax-rax)
    (imm src #x05 :int-16-32-64))
  (imm-modrm src dst #x81 0 :int-16-32-64)
  (reg-modrm dst src #x03)
  (reg-modrm src dst #x01))

;;;;;;;;;;; AND

(define-operator/8 :andb (mask dst)
  (when (eq dst :al)
    (imm mask #x24 (xint 8)))
  (imm-modrm mask dst #x80 4 (xint 8))
  (reg-modrm dst mask #x22)
  (reg-modrm mask dst #x20))

(define-operator* (:16 :andw :32 :andl :64 :andr) (mask dst)
  (imm-modrm mask dst #x83 4 (sint 8))
  (when (eq dst :ax-eax-rax)
    (imm mask #x25 :int-16-32-64))
  (imm-modrm mask dst #x81 4 :int-16-32-64)
  (reg-modrm dst mask #x23)
  (reg-modrm mask dst #x21))

;;;;;;;;;;; BOUND, BSF, BSR, BSWAP

(define-operator* (:16 :boundw :32 :boundl) (bounds reg)
  (reg-modrm reg bounds #x62))

(define-operator* (:16 :bsfw :32 :bsfl :64 :bsfr) (src dst)
  (reg-modrm dst src #x0fbc))

(define-operator* (:16 :bsrw :32 :bsrl :64 :bsrr) (src dst)
  (reg-modrm dst src #x0fbd))

(define-operator* (:32 :bswapl :64 :bswapr) (dst)
  (opcode-reg #x0fc8 dst))

;;;;;;;;;;; BT, BTC, BTR, BTS

(define-operator* (:16 :btw :32 :btl :64 :btr) (bit src)
  (imm-modrm bit src #x0fba 4 (uint 8))
  (reg-modrm bit src #x0fa3))

(define-operator* (:16 :btcw :32 :btcl :64 :btcr) (bit src)
  (imm-modrm bit src #x0fba 7 (uint 8))
  (reg-modrm bit src #x0fbb))

(define-operator* (:16 :btrw :32 :btrl :64 :btrr) (bit src)
  (imm-modrm bit src #x0fba 6 (uint 8))
  (reg-modrm bit src #x0fb3))

(define-operator* (:16 :btsw :32 :btsl :64 :btsr) (bit src)
  (imm-modrm bit src #x0fba 5 (uint 8))
  (reg-modrm bit src #x0fab))

;;;;;;;;;;; CALL

(define-operator/16 :callw (dest)
  (pc-rel #xe8 dest (sint 16))
  (modrm dest #xff 2))

(define-operator/32 :call (dest)
  (pc-rel #xe8 dest (sint 32))
  (modrm dest #xff 2))

(define-operator/32 :callr (dest)
  (modrm dest #xff 2))

;;;;;;;;;;; CLC, CLD, CLI, CLTS, CMC

(define-operator :clc () (opcode #xf8))
(define-operator :cld () (opcode #xfc))
(define-operator :cli () (opcode #xfa))
(define-operator :clts () (opcode #x0f06))
(define-operator :cmc () (opcode #xf5))

;;;;;;;;;;; CMOVcc

(define-operator* (:16 :cmovaw :32 :cmova :64 :cmovar) (src dst)
  (reg-modrm dst src #x0f47)) ; Move if above, CF=0 and ZF=0.

(define-operator* (:16 :cmovaew :32 :cmovae :64 :cmovaer) (src dst)
  (reg-modrm dst src #x0f43)) ; Move if above or equal, CF=0.

(define-operator* (:16 :cmovbw :32 :cmovb :64 :cmovbr) (src dst)
  (reg-modrm dst src #x0f42)) ; Move if below, CF=1.

(define-operator* (:16 :cmovbew :32 :cmovbe :64 :cmovber) (src dst)
  (reg-modrm dst src #x0f46)) ; Move if below or  equal, CF=1 or ZF=1.

(define-operator* (:16 :cmovcw :32 :cmovc :64 :cmovcr) (src dst)
  (reg-modrm dst src #x0f42)) ; Move if carry, CF=1.

(define-operator* (:16 :cmovew :32 :cmove :64 :cmover) (src dst)
  (reg-modrm dst src #x0f44)) ; Move if equal, ZF=1.

(define-operator* (:16 :cmovgw :32 :cmovg :64 :cmovgr) (src dst)
  (reg-modrm dst src #x0f4f)) ; Move if greater, ZF=0 and SF=OF.

(define-operator* (:16 :cmovgew :32 :cmovge :64 :cmovger) (src dst)
  (reg-modrm dst src #x0f4d)) ; Move if greater or equal, SF=OF.

(define-operator* (:16 :cmovlw :32 :cmovl :64 :cmovlr) (src dst)
  (reg-modrm dst src #x0f4c))

(define-operator* (:16 :cmovlew :32 :cmovle :64 :cmovler) (src dst)
  (reg-modrm dst src #x0f4e)) ; Move if less or equal, ZF=1 or SF/=OF.

(define-operator* (:16 :cmovnaw :32 :cmovna :64 :cmovnar) (src dst)
  (reg-modrm dst src #x0f46)) ; Move if not above, CF=1 or ZF=1.

(define-operator* (:16 :cmovnaew :32 :cmovnae :64 :cmovnaer) (src dst)
  (reg-modrm dst src #x0f42)) ; Move if not above or equal, CF=1.

(define-operator* (:16 :cmovnbw :32 :cmovnb :64 :cmovnbr) (src dst)
  (reg-modrm dst src #x0f43)) ; Move if not below, CF=0.

(define-operator* (:16 :cmovnbew :32 :cmovnbe :64 :cmovnber) (src dst)
  (reg-modrm dst src #x0f47)) ; Move if not below or equal, CF=0 and ZF=0.

(define-operator* (:16 :cmovncw :32 :cmovnc :64 :cmovncr) (src dst)
  (reg-modrm dst src #x0f43)) ; Move if not carry, CF=0.

(define-operator* (:16 :cmovnew :32 :cmovne :64 :cmovner) (src dst)
  (reg-modrm dst src #x0f45)) ; Move if not equal, ZF=0.

(define-operator* (:16 :cmovngew :32 :cmovnge :64 :cmovnger) (src dst)
  (reg-modrm dst src #x0f4c)) ; Move if not greater or equal, SF/=OF.

(define-operator* (:16 :cmovnlw :32 :cmovnl :64 :cmovnlr) (src dst)
  (reg-modrm dst src #x0f4d)) ; Move if not less SF=OF.

(define-operator* (:16 :cmovnlew :32 :cmovnle :64 :cmovnler) (src dst)
  (reg-modrm dst src #x0f4f)) ; Move if not less or equal, ZF=0 and SF=OF.

(define-operator* (:16 :cmovnow :32 :cmovno :64 :cmovnor) (src dst)
  (reg-modrm dst src #x0f41)) ; Move if not overflow, OF=0.

(define-operator* (:16 :cmovnpw :32 :cmovnp :64 :cmovnpr) (src dst)
  (reg-modrm dst src #x0f4b)) ; Move if not parity, PF=0.

(define-operator* (:16 :cmovnsw :32 :cmovns :64 :cmovnsr) (src dst)
  (reg-modrm dst src #x0f49)) ; Move if not sign, SF=0.

(define-operator* (:16 :cmovnzw :32 :cmovnz :64 :cmovnzr) (src dst)
  (reg-modrm dst src #x0f45)) ; Move if not zero, ZF=0.

(define-operator* (:16 :cmovow :32 :cmovo :64 :cmovor) (src dst)
  (reg-modrm dst src #x0f40)) ; Move if overflow, OF=1.

(define-operator* (:16 :cmovpw :32 :cmovp :64 :cmovpr) (src dst)
  (reg-modrm dst src #x0f4a)) ; Move if parity, PF=1.

(define-operator* (:16 :cmovsw :32 :cmovs :64 :cmovsr) (src dst)
  (reg-modrm dst src #x0f48)) ; Move if sign, SF=1

(define-operator* (:16 :cmovzw :32 :cmovz :64 :cmovzr) (src dst)
  (reg-modrm dst src #x0f44)) ; Move if zero, ZF=1

;;;;;;;;;;; CMP

(define-operator/8 :cmpb (src dst)
  (when (eq dst :al)
    (imm src #x3c (xint 8)))
  (imm-modrm src dst #x80 7 (xint 8))
  (reg-modrm dst src #x3a)
  (reg-modrm src dst #x38))

(define-operator* (:16 :cmpw :32 :cmpl :64 :cmpr) (src dst)
  (imm-modrm src dst #x83 7 (sint 8))
  (when (eq dst :ax-eax-rax)
    (imm src #x3d :int-16-32-64))
  (imm-modrm src dst #x81 7 :int-16-32-64)
  (reg-modrm dst src #x3b)
  (reg-modrm src dst #x39))

;;;;;;;;;;; MOV

(define-operator/8 :movb (src dst)
  (when (eq src :al)
    (moffset #xa2 dst (uint 8)))
  (when (eq dst :al)
    (moffset #xa0 src (uint 8)))
  (opcode-reg-imm #xb0 dst src (xint 8))
  (imm-modrm src dst #xc6 0 (xint 8))
  (reg-modrm dst src #x8a)
  (reg-modrm src dst #x88))

(define-operator/16 :movw (src dst)
  (when (eq src :ax)
    (moffset #xa3 dst (uint 16)))
  (when (eq dst :ax)
    (moffset #xa0 src (uint 16)))
  (opcode-reg-imm #xb8 dst src (xint 16))
  (imm-modrm src dst #xc7 0 (xint 16))
  (sreg-modrm src dst #x8c)
  (sreg-modrm dst src #x8e)
  (reg-modrm dst src #x8b)
  (reg-modrm src dst #x89))

(define-operator/32 :movl (src dst)
  (when (eq src :eax)
    (moffset #xa3 dst (uint 32)))
  (when (eq dst :eax)
    (moffset #xa0 src (uint 32)))
  (opcode-reg-imm #xb8 dst src (xint 32))
  (imm-modrm src dst #xc7 0 (xint 32))
  (reg-modrm dst src #x8b)
  (reg-modrm src dst #x89))

;;;;;;;;;;; POP

(define-operator* (:16 :popw :32 :popl) (dst)
  (case dst
    (:ds (yield :opcode #x1f))
    (:es (yield :opcode #x07))
    (:ss (yield :opcode #x17))
    (:fs (yield :opcode #x0fa1))
    (:gs (yield :opcode #x0fa9)))
  (opcode-reg #x58 dst)
  (modrm dst #x8f 0))

(define-operator/64* :popr (dst)
  (opcode-reg #x58 dst)
  (modrm dst #x8f 0))

;;;;;;;;;;; CMPXCHG

(define-operator/8 :cmpxchgb (cmp-reg cmp-modrm al-dst)
  (when (eq al-dst :al)
    (reg-modrm cmp-reg cmp-modrm #x0fb0)))

(define-operator* (:16 :cmpxchgw :32 :cmpxchgl :64 :cmpxchgr) (cmp-reg cmp-modrm al-dst)
  (when (eq al-dst :ax-eax-rax)
    (reg-modrm cmp-reg cmp-modrm #x0fb1)))

;;;;;;;;;;; CMPXCHG8B, CMPXCHG16B

(define-operator/32 :cmpxchg8b (address)
  (modrm address #x0fc7 1))

(define-operator/64 :cmpxchg16b (address)
  (modrm address #x0fc7 1))

;;;;;;;;;;; CPUID

(define-operator :cpuid ()
  (opcode* #x0fa2))

;;;;;;;;;;; CWD, CDQ

(define-operator/16 :cwd (reg1 reg2)
  (when (and (eq reg1 :ax)
             (eq reg2 :dx))
    (opcode #x99)))

(define-operator/32 :cdq (reg1 reg2)
  (when (and (eq reg1 :eax)
             (eq reg2 :edx))
    (opcode #x99)))

(define-operator/64 :cqo (reg1 reg2)
  (when (and (eq reg1 :rax)
             (eq reg2 :rdx))
    (opcode #x99)))

;;;;;;;;;;; DEC

(define-operator/8 :decb (dst)
  (modrm dst #xfe 1))

(define-operator* (:16 :decw :32 :decl) (dst)
  (unless (eq *cpu-mode* :64-bit)
    (opcode-reg #x48 dst))
  (modrm dst #xff 1))

(define-operator* (:64 :decr) (dst)
  (modrm dst #xff 1))
    
;;;;;;;;;;; DIV

(define-operator/8 :divb (divisor dividend)
  (when (eq dividend :ax)
    (modrm divisor #xf6 6)))

(define-operator* (:16 :divw :32 :divl :64 :divr) (divisor dividend1 dividend2)
  (when (and (eq dividend1 :ax-eax-rax)
             (eq dividend2 :dx-edx-rdx))
    (modrm divisor #xf7 6)))

;;;;;;;;;;; HLT

(define-operator :halt ()
  (opcode #xf4))

;;;;;;;;;;; IDIV

(define-operator/8 :idivb (divisor dividend1 dividend2)
  (when (and (eq dividend1 :al)
             (eq dividend2 :ah))
    (modrm divisor #xf6 7)))

(define-operator* (:16 :idivw :32 :idivl :64 :idivr) (divisor dividend1 dividend2)
  (when (and (eq dividend1 :ax-eax-rax)
             (eq dividend2 :dx-edx-rdx))
    (modrm divisor #xf7 7)))

;;;;;;;;;;; IMUL

(define-operator/32 :imull (factor product1 &optional product2)
  (when (not product2)
    (reg-modrm product1 factor #x0faf))
  (when (and (eq product1 :eax)
             (eq product2 :edx))
    (modrm factor #xf7 5))
  (typecase factor
    ((sint 8)
     (reg-modrm product1 product2 #x6b
      :displacement (encode-integer factor '(sint 8))))
    ((sint 32)
     (reg-modrm product1 product2 #x69
      :displacement (encode-integer factor '(sint 32))))))

;;;;;;;;;;; IN

(define-operator/8 :inb (port dst)
  (when (eq :al dst)
    (typecase port
      ((eql :dx)
       (opcode #xec))
      ((uint 8)
       (imm port #xe4 (uint 8))))))

(define-operator/16 :inw (port dst)
  (when (eq :ax dst)
    (typecase port
      ((eql :dx)
       (opcode #xed))
      ((uint 8)
       (imm port #xe5 (uint 8))))))

(define-operator/32 :inl (port dst)
  (when (eq :eax dst)
    (typecase port
      ((eql :dx)
       (opcode #xed))
      ((uint 8)
       (imm port #xe5 (uint 8))))))

;;;;;;;;;;; INC

(define-operator/8 :incb (dst)
  (modrm dst #xfe 0))

(define-operator* (:16 :incw :32 :incl) (dst)
  (unless (eq *cpu-mode* :64-bit)
    (opcode-reg #x40 dst))
  (modrm dst #xff 0))

(define-operator* (:64 :incr) (dst)
  (modrm dst #xff 0))

;;;;;;;;;;; INT

(define-operator :break ()
  (opcode #xcc))

(define-operator :int (vector)
  (imm vector #xcd (uint 8)))

(define-operator :into ()
  (opcode #xce))

;;;;;;;;;;; INVLPG

(define-operator :invlpg (address)
  (modrm address #x0f01 7))

;;;;;;;;;;; IRET

(define-operator* (:16 :iret :32 :iretd :64 :iretq) ()
  (opcode #xcf :rex default-rex))

;;;;;;;;;;; Jcc

(defmacro define-jcc (name opcode1 &optional (opcode2 (+ #x0f10 opcode1)))
 `(define-operator ,name (dst)
    (pc-rel ,opcode1 dst (sint 8))
    (case *cpu-mode*
      ((:16-bit :32-bit)
       (pc-rel ,opcode2 dst (sint 16)
        :operand-size :16-bit)))
    (pc-rel ,opcode2 dst (sint 32)
     :operand-size (case *cpu-mode*
                     ((:16-bit :32-bit)
                      :32-bit)))))

(define-jcc :ja #x77)
(define-jcc :jae #x73)
(define-jcc :jb #x72)
(define-jcc :jbe #x76)
(define-jcc :jc #x72)
(define-jcc :jecx #xe3)
(define-jcc :je #x74)
(define-jcc :jg #x7f)
(define-jcc :jge #x7d)
(define-jcc :jl #x7c)
(define-jcc :jle #x7e)
(define-jcc :jna #x76)
(define-jcc :jnae #x72)
(define-jcc :jnb #x73)
(define-jcc :jnbe #x77)
(define-jcc :jnc #x73)
(define-jcc :jne #x75)
(define-jcc :jng #x7e)
(define-jcc :jnge #x7c)
(define-jcc :jnl #x7d)
(define-jcc :jnle #x7f)
(define-jcc :jno #x71)
(define-jcc :jnp #x7b)
(define-jcc :jns #x79)
(define-jcc :jnz #x75)
(define-jcc :jo #x70)
(define-jcc :jp #x7a)
(define-jcc :jpe #x7a)
(define-jcc :jpo #x7b)
(define-jcc :js #x78)
(define-jcc :jz #x74)
  
;;;;;;;;;;; JMP

(define-operator :jmp (dst)
  (pc-rel #xeb dst (sint 8))
  (pc-rel #xe9 dst (sint 32))
  (modrm dst #xff 4))

;;;;;;;;;;; LAHF, LAR

(define-operator :lahf ()
  (case *cpu-mode*
    ((:16-bit :32-bit)
     (opcode #x9f))))

(define-operator* (:16 :larw :32 :larl :64 :larr) (src dst)
  (reg-modrm dst src #x0f02))

;;;;;;;;;;; LEA

(define-operator* (:16 :leaw :32 :leal :64 :lear) (addr dst)
  (reg-modrm dst addr #x8d))

;;;;;;;;;;; LEAVE

(define-operator :leave ()
  (opcode #xc9))

;;;;;;;;;;; LGDT, LIDT

(define-operator* (:16 :lgdtw :32 :lgdtl :64 :lgdtr) (addr)
  (modrm addr #x0f01 2))

(define-operator* (:16 :lidtw :32 :lidtl :64 :lidtr) (addr)
  (modrm addr #x0f01 3))

;;;;;;;;;;; PUSH

(define-operator* (:16 :pushw :32 :pushl) (src)
  (case src
    (:cs (yield :opcode #x0e))
    (:ss (yield :opcode #x16))
    (:ds (yield :opcode #x1e))
    (:es (yield :opcode #x06))
    (:fs (yield :opcode #x0fa0))
    (:gs (yield :opcode #x0fa8)))
  (opcode-reg #x50 src)
  (imm src #x6a (sint 8))
  (imm src #x68 :int-16-32-64 :operand-size operator-mode)
  (modrm src #xff 6))

(define-operator/64* :pushr (src)
  (opcode-reg #x50 src)
  (imm src #x6a (sint 8))
  (imm src #x68 (sint 16) :operand-size :16-bit)
  (imm src #x68 (sint 32))
  (modrm src #xff 6))

;;;;;;;;;;; RET

(define-operator :ret ()
  (opcode #xc3))
