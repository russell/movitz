;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2003-2004, 
;;;;    Department of Computer Science, University of Tromso, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      los0-gc.lisp
;;;; Description:   A simple GC architecture for los0.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Sat Feb 21 17:48:32 2004
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(provide :los0-gc)

(in-package muerte.init)

(defconstant +space-size+ #xfffd)

(defun make-space (location)
  "Make a space vector at a fixed location."
  (assert (evenp location))
  (macrolet ((x (index)
	       `(memref location 0 ,index :unsigned-byte32)))
    (setf (x 1) (* #.movitz:+movitz-fixnum-factor+ +space-size+)
	  (x 0) #.(cl:dpb (bt:enum-value 'movitz:movitz-vector-element-type :u32)
			  (cl:byte 8 8)
			  (bt:enum-value 'movitz:other-type-byte :basic-vector))))
  (%word-offset location #.(movitz:tag :other)))


(defmacro space-fresh-pointer (space)
  `(memref ,space -6 2 :lisp))

;;;(defmacro space-last-cons (space)
;;;  "The location of the last cons-cell that will with in this space."
;;;  `(memref ,space -6 3 :lisp))

(defmacro space-other (space)
  `(memref ,space -6 3 :lisp))

(defun allocate-space (size &optional other-space)
  (let ((space (make-array size :element-type '(unsigned-byte 32))))
    (initialize-space space)
    (setf (space-other space) other-space)
    space))

(defun initialize-space (space)
  (setf (space-fresh-pointer space) 2
;;;	(space-last-cons space) (+ (object-location space)
;;;				   (array-dimension space 0)))
	)
  space)
				   

(defun allocate-duo-space (size)
  (let* ((space1 (allocate-space size))
	 (space2 (allocate-space size space1)))
    (setf (space-other space1) space2)
    space1))

(defun space-cons-pointer ()
  (aref (%run-time-context-slot 'nursery-space) 0))

(defun test ()
  (warn "install..")
  (install-los0-consing 4)
  (warn "nursery: ~Z, other: ~Z" 
	(%run-time-context-slot 'muerte::nursery-space)
	(space-other (%run-time-context-slot 'muerte::nursery-space)))
  (warn "first cons: ~Z" (funcall 'truncate #x100000000 3))
  (warn "second cons: ~Z" (funcall 'truncate #x100000000 3))
  (halt-cpu)
  (values))

(define-primitive-function los0-fast-cons ()
  "Allocate a cons cell from nursery-space."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :eax)
	   retry-cons
	    ;; Set up thread-atomical execution
	    (:locally (:movl ,(movitz::atomically-status-simple-pf 'fast-cons t)
			     (:edi (:edi-offset atomically-status))))
	    (:locally (:movl (:edi (:edi-offset nursery-space)) :edx))
	    (:movl (:edx 2) :ecx)
	    (:cmpl (:edx ,(bt:slot-offset 'movitz:movitz-basic-vector 'movitz::num-elements))
		   :ecx)
	    (:jae '(:sub-program (allocation-failed)
		    ;; Exit thread-atomical
		    (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
			       (:edi (:edi-offset atomically-status))))
		    (:int 113)
		    ;; This interrupt can be retried.
		    (:jmp 'retry-cons)))
	    (:movl :eax (:edx :ecx 2))
	    (:movl :ebx (:edx :ecx 6))
	    (:addl 8 :ecx)
	    (:movl :ecx (:edx 2))	; Commit allocation
	    ;; Exit thread-atomical
	    (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
			     (:edi (:edi-offset atomically-status))))
	    (:leal (:edx :ecx -5) :eax)
	    (:ret))))
    (do-it)))

(define-primitive-function muerte::get-cons-pointer ()
  "Return in EAX the next object location with space for EAX words, with tag 6.
Preserve ECX."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :multiple-values)
	   retry
	    (:locally (:cmpl 0 (:edi (:edi-offset atomically-status)))) ; Atomically?
	    (:je '(:sub-program ()
		   (:int 50)))		; This must be called inside atomically.
	    (:locally (:movl (:edi (:edi-offset nursery-space)) :edx))
	    (:movl (:edx 2) :ebx)
	    (:leal (:ebx :eax 4) :eax)
	    (:andl -8 :eax)
	    (:cmpl (:edx ,(bt:slot-offset 'movitz:movitz-basic-vector 'movitz::num-elements))
		   :eax)
	    (:ja '(:sub-program (probe-failed)
		   (:int 113)
		   (:jmp 'retry)))
	    (:movl :edi (:edx :ebx 8 ,movitz:+other-type-offset+))
	    (:leal (:edx :ebx 8) :eax)
	    (:ret))))
    (do-it)))

(define-primitive-function muerte::cons-commit ()
  "Commit allocation of ECX/fixnum words.
Preserve EAX and EBX."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :multiple-values)
	   retry
	    (:locally (:cmpl 0 (:edi (:edi-offset atomically-status)))) ; Atomically?
	    (:je '(:sub-program ()
		   (:int 50)))		; This must be called inside atomically.
	    (:addl ,movitz:+movitz-fixnum-factor+ :ecx)
	    (:locally (:movl (:edi (:edi-offset nursery-space)) :edx))
	    (:andl -8 :ecx)
	    (:addl (:edx 2) :ecx)
	    (:cmpl (:edx ,(bt:slot-offset 'movitz:movitz-basic-vector 'movitz::num-elements))
		   :ecx)
	    (:ja '(:sub-program (commit-failed)
		   (:int 113)
		   (:jmp 'retry)))
	    (:movl :ecx (:edx 2))
	    (:leal (:edx :ecx) :ecx)
	    (:ret))))
    (do-it)))
    
(define-primitive-function los0-box-u32-ecx ()
  "Make u32 in ECX into a fixnum or bignum."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :multiple-values)
	    (:cmpl ,movitz:+movitz-most-positive-fixnum+ :ecx)
	    (:ja 'not-fixnum)
	    (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
	    (:ret)
	   not-fixnum
	   retry-cons
	    (:locally (:movl ,(movitz::atomically-status-simple-pf 'box-u32-ecx t)
			     (:edi (:edi-offset atomically-status))))
	    (:locally (:movl (:edi (:edi-offset nursery-space)) :edx))
	    (:movl (:edx 2) :eax)
	    (:cmpl (:edx ,(bt:slot-offset 'movitz:movitz-basic-vector 'movitz::num-elements))
		   :eax)
	    (:jae '(:sub-program ()
		    (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
			       (:edi (:edi-offset atomically-status))))
		    (:int 113)		; This interrupt can be retried.
		    (:jmp 'retry-cons)))
	    (:movl ,(dpb 1 (byte 16 16) (movitz:tag :bignum 0))
		   (:edx :eax 2))
	    (:movl :ecx (:edx :eax 6))
	    (:addl 8 :eax)
	    (:movl :eax (:edx 2))	; Commit allocation
	    ;; Exit thread-atomical
	    (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
			     (:edi (:edi-offset atomically-status))))
	    (:leal (:edx :eax) :eax)
	    (:ret))))
    (do-it)))

(defun los0-malloc-clumps (clumps)
  (check-type clumps (integer 0 160000))
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :eax)
	   retry
	    (:compile-form (:result-mode :ebx) clumps)
	    (:declare-label-set retry-jumper (retry))
	    (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t)
			       'retry-jumper)
			     (:edi (:edi-offset atomically-status))))
	    (:locally (:movl (:edi (:edi-offset nursery-space)) :edx))
	    (:movl (:edx 2) :ecx)
	    (:leal ((:ebx 2) :ecx) :eax)
	    (:cmpl (:edx ,(bt:slot-offset 'movitz:movitz-basic-vector 'movitz::num-elements))
		   :eax)
	    (:ja '(:sub-program ()
		   (:int 113)))
	    (:movl :eax (:edx 2))
	    (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
			     (:edi (:edi-offset atomically-status))))
	    (:movl ,(movitz:tag :infant-object) (:edx :ecx 6))
	    (:leal (:edx :ecx 8) :eax)		
	    (:xorl :ecx :ecx)
	   init-loop			; Now init eax number of clumps.
	    (:movl :edi (:eax (:ecx 2) -6))
	    (:movl :edi (:eax (:ecx 2) -2))
	    (:addl 4 :ecx)
	    (:cmpl :ebx :ecx)
	    (:jb 'init-loop))))
    (do-it)))

(defun los0-malloc-data-clumps (clumps)
  (check-type clumps (integer 0 160000))
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :eax)
	   retry
	    (:compile-form (:result-mode :ebx) clumps)
	    (:declare-label-set retry-jumper (retry))
	    (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t)
			       'retry-jumper)
			     (:edi (:edi-offset atomically-status))))
	    (:locally (:movl (:edi (:edi-offset nursery-space)) :edx))
	    (:movl (:edx 2) :ecx)
	    (:leal ((:ebx 2) :ecx) :eax)
	    (:cmpl (:edx ,(bt:slot-offset 'movitz:movitz-basic-vector 'movitz::num-elements))
		   :eax)
	    (:ja '(:sub-program ()
		   (:int 113)))
	    (:movl :eax (:edx 2))
	    (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
			     (:edi (:edi-offset atomically-status))))

	    (:movl #.(movitz:tag :infant-object) (:edx :ecx 6))
	    (:leal (:edx :ecx 8) :eax))))
    (do-it)))

(defun install-los0-consing (&optional (space-kilobytes 2048))
  (let ((size (* space-kilobytes #x100)))
    (setf (%run-time-context-slot 'nursery-space)
      (allocate-duo-space size))
    (setf (exception-handler 113)
      (lambda (exception interrupt-frame)
	(declare (ignore exception interrupt-frame))
	(format t "~&;; Handling out-of-memory exception..")
	(stop-and-copy)))
    (let ((conser (symbol-value 'los0-fast-cons)))
      (check-type conser vector)
      (setf (%run-time-context-slot 'muerte::fast-cons)
	conser))
    (let ((conser (symbol-value 'los0-box-u32-ecx)))
      (check-type conser vector)
      (setf (%run-time-context-slot 'muerte::box-u32-ecx)
	conser))
    (let ((old-malloc (symbol-function 'muerte:malloc-clumps)))
      (setf (symbol-function 'muerte:malloc-clumps)
	(symbol-function 'los0-malloc-clumps))
      (setf (symbol-function 'los0-malloc-clumps)
	old-malloc))
    (let ((old-malloc-data (symbol-function 'muerte:malloc-data-clumps)))
      (setf (symbol-function 'muerte:malloc-data-clumps)
	(symbol-function 'los0-malloc-data-clumps))
      (setf (symbol-function 'los0-malloc-data-clumps)
	old-malloc-data))
    (values)))

(defun install-old-consing ()
  (let ((conser (symbol-value 'muerte::fast-cons)))
    (check-type conser vector)
    (setf (%run-time-context-slot 'muerte::fast-cons)
      conser))
  (let ((old-malloc (symbol-function 'muerte:malloc-clumps)))
    (setf (symbol-function 'muerte:malloc-clumps)
      (symbol-function 'los0-malloc-clumps))
    (setf (symbol-function 'los0-malloc-clumps)
      old-malloc))
  (let ((old-malloc-data (symbol-function 'muerte:malloc-data-clumps)))
    (setf (symbol-function 'muerte:malloc-data-clumps)
      (symbol-function 'los0-malloc-data-clumps))
    (setf (symbol-function 'los0-malloc-data-clumps)
      old-malloc-data))
  (values))

(defun object-in-space-p (space object)
  (check-type space vector-u32)
  (and (typep object 'pointer)
       (< (object-location space)
	  (object-location object)
	  (+ (object-location space)
	     (array-dimension space 0)))))

(defun tenure ()
  (install-old-consing)
  (install-los0-consing))

(defun report-nursery (x location)
  "Write a message if x is inside newspace."
  (when (object-in-space-p (%run-time-context-slot 'nursery-space) x)
    (format t "~&~Z: ~S: ~S from ~S" x (type-of x) x location))
  x)

(defun report-inactive-space (x location)
  "Check that x is not pointing into (what is presumably) oldspace."
  (when (object-in-space-p (space-other (%run-time-context-slot 'nursery-space)) x)
    (break "~Z: ~S: ~S from ~S" x (type-of x) x location))
  x)

(defun location-finder (find-location)
  (lambda (x location)
    (when (location-in-object-p x find-location)
      (break "The location ~S is in the object at ~Z referenced from location ~S."
	     find-location x location))
    x))

(defun kill-the-newborns ()
  (let* ((oldspace (%run-time-context-slot 'nursery-space))
	 (newspace (space-other oldspace)))
    (setf (%run-time-context-slot 'nursery-space) newspace)
    (flet ((zap-oldspace (x location)
	     (declare (ignore location))
	     (if (object-in-space-p oldspace x)
		 nil
	       x)))
      (map-heap-words #'zap-oldspace 0 (malloc-end))
      (map-stack-words #'zap-oldspace (current-stack-frame))
      (initialize-space oldspace)
      (values))))
  
(defun stop-and-copy (&optional evacuator)
  (let* ((space0 (%run-time-context-slot 'nursery-space))
	 (space1 (space-other space0)))
    (check-type space0 vector-u32)
    (check-type space1 vector-u32)
    (assert (eq space0 (space-other space1)))
    (multiple-value-bind (newspace oldspace)
	(if (< (space-fresh-pointer space0) ; Chose the emptiest space as newspace.
	       (space-fresh-pointer space1))
	    (values space0 space1)
	  (values space1 space0))
      ;; Ensure newspace is activated.
      (setf (%run-time-context-slot 'nursery-space) newspace)
      ;; Evacuate-oldspace is to be mapped over every potential pointer.
      (let ((evacuator
	     (or evacuator
		 (lambda (x location)
		   "If x is in oldspace, migrate it to newspace."
		   (declare (ignore location))
		   (if (not (object-in-space-p oldspace x))
		       x
		     (let ((forwarded-x (memref (object-location x) 0 1 :lisp)))
		       (if (object-in-space-p newspace forwarded-x)
			   forwarded-x
			 (let ((forward-x (shallow-copy x)))
			   (setf (memref (object-location x) 0 1 :lisp) forward-x)
			   forward-x))))))))
	;; Scavenge roots
	(map-heap-words evacuator 0 (+ (malloc-buffer-start)
				       (* 2 (malloc-cons-pointer))))
	(map-stack-words evacuator (current-stack-frame))
	;; Scan newspace, Cheney style.
	(loop with newspace-location = (+ 2 (object-location newspace))
	    with scan-pointer = 2
	    as fresh-pointer = (space-fresh-pointer newspace)
	    while (< scan-pointer fresh-pointer)
	    do (map-heap-words evacuator
			       (+ newspace-location scan-pointer)
			       (+ newspace-location (space-fresh-pointer newspace)))
	       (setf scan-pointer fresh-pointer))
	;; GC completed, oldspace is evacuated.
	(let ((old-size (truncate (- (space-fresh-pointer oldspace) 2) 2))
	      (new-size (truncate (- (space-fresh-pointer newspace) 2) 2)))
	  (format t "~&;; Old space: ~/muerte:pprint-clumps/, new space: ~
~/muerte:pprint-clumps/, freed: ~/muerte:pprint-clumps/.~%"
		  old-size new-size (- old-size new-size)))
	(initialize-space oldspace))))
  (values))
