;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2003-2005, 
;;;;    Department of Computer Science, University of Tromso, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      scavenge.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Mon Mar 29 14:54:08 2004
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(in-package #:muerte)

(provide :muerte/scavenge)

;; In this file and others, a "location" is a fixnum word used as a
;; memory pointer with 32-bit resolution. The location 0 is address
;; #x0, the location 1 is address #x4, 2 is #x8, 100 is #x190, 262144
;; is the 1MB address, and so on.
;;
;; Such "locations" can of course only be used in certain
;; circumstances, i.e. when you know there is no outside GC
;; etc. involved.

(defvar *scan*)				; debugging
(defvar *scan-last*)			; debugging
(defvar *map-header-vals-verbose* nil)

(defun map-lisp-vals (function start-location end-location)
  (with-funcallable (do-map function)
    (loop for location from start-location below end-location
	as object = (memref location 0)
	do (when (typep object 'pointer)
	     (let ((new-object (do-map object location)))
	       (unless (eq object new-object)
		 (setf (memref location 0) new-object)))))))

(defun map-header-vals (function start-location end-location)
  "Map function over each potential pointer word between
start-location and end-location."
  (macrolet ((scavenge-typep (x primary)
	       (let ((code (movitz:tag primary)))
		 `(= ,code (ldb (byte 8 0) ,x))))
	     (scavenge-wide-typep (x primary secondary)
	       (let ((code (dpb secondary
				(byte 8 8)
				(movitz:tag primary))))
		 `(= ,code ,x))))
    (do ((verbose *map-header-vals-verbose*)
	 (*scan-last* nil)		; Last scanned object, for debugging.
	 (scan start-location (1+ scan)))
	((>= scan end-location))
      (with-simple-restart (continue-map-header-vals
			    "Continue map-header-vals at location ~S." (1+ scan))
	(let ((x (memref scan 0 :type :unsigned-byte16))
	      (x2 (memref scan 1 :type :unsigned-byte16)))
	  (when verbose
	    (format *terminal-io* " [at ~S: ~S]" scan x))
	  (cond
	   ((let ((tag (ldb (byte 3 0) x)))
	      (or (= tag #.(movitz:tag :null))
		  (= tag #.(movitz:tag :even-fixnum))
		  (= tag #.(movitz:tag :odd-fixnum))
		  (scavenge-typep x :character))))
	   ((or (and (= 0 x2) (= 2 x))
		(and (= #xffff x2) (= #xfffe x))
		(and (= #x7fff x2) (= #xffff x))))
	   ((scavenge-typep x :illegal)
	    (error "Illegal word ~S at ~S." x scan))
	   ((scavenge-typep x :bignum)
	    (assert (evenp scan) ()
	      "Scanned bignum-header ~S at odd location #x~X." x scan)
	    ;; Just skip the bigits
	    (let* ((bigits (memref scan 0 :index 1 :type :unsigned-byte14))
		   (delta (logior bigits 1)))
	      (setf *scan-last* (%word-offset scan #.(movitz:tag :other)))
	      (incf scan delta)))
	   ((scavenge-typep x :defstruct)
	    (assert (evenp scan) ()
	      "Scanned struct-header ~S at odd location #x~X." x scan)
	    (setf *scan-last* (%word-offset scan #.(movitz:tag :other))))
	   ((scavenge-typep x :funobj)
	    (assert (evenp scan) ()
	      "Scanned funobj-header ~S at odd location #x~X." 
	      (memref scan 0 :type :unsigned-byte32) scan)
	    (setf *scan-last* (%word-offset scan #.(movitz:tag :other)))
	    ;; Process code-vector pointers specially..
	    (let* ((funobj (%word-offset scan #.(movitz:tag :other)))
		   (code-vector (funobj-code-vector funobj))
		   (num-jumpers (funobj-num-jumpers funobj)))
	      (check-type code-vector code-vector)
	      (map-header-vals function (+ scan 5) (+ scan 7)) ; scan funobj's lambda-list and name
	      (let ((new-code-vector (funcall function code-vector scan)))
		(check-type new-code-vector code-vector)
		(unless (eq code-vector new-code-vector)
		  (error "Code-vector migration is not implemented (~S)." funobj)
		  (setf (memref scan 0 :index -1) (%word-offset new-code-vector 2))
		  ;; Do more stuff here to update code-vectors and jumpers
		  ))
	      (incf scan (+ 7 num-jumpers)))) ; Don't scan the jumpers.
	   ((scavenge-typep x :infant-object)
	    (assert (evenp scan) ()
	      "Scanned infant ~S at odd location #x~X." x scan)
	    (error "Scanning an infant object ~Z at ~S (end ~S)." x scan end-location))
	   ((or (scavenge-wide-typep x :basic-vector
				     #.(bt:enum-value 'movitz:movitz-vector-element-type :u8))
		(scavenge-wide-typep x :basic-vector
				     #.(bt:enum-value 'movitz:movitz-vector-element-type :character))
		(scavenge-wide-typep x :basic-vector
				     #.(bt:enum-value 'movitz:movitz-vector-element-type :code)))
	    (assert (evenp scan) ()
	      "Scanned u8-vector-header ~S at odd location #x~X." x scan)
	    (let ((len (memref scan 0 :index 1 :type :lisp)))
	      (check-type len positive-fixnum)
	      (setf *scan-last* (%word-offset scan #.(movitz:tag :other)))
	      (incf scan (1+ (* 2 (truncate (+ 7 len) 8))))))
	   ((scavenge-wide-typep x :basic-vector #.(bt:enum-value 'movitz:movitz-vector-element-type :u16))
	    (assert (evenp scan) ()
	      "Scanned u16-vector-header ~S at odd location #x~X." x scan)
	    (let ((len (memref scan 0 :index 1)))
	      (check-type len positive-fixnum)
	      (setf *scan-last* (%word-offset scan #.(movitz:tag :other)))
	      (incf scan (1+ (* 2 (truncate (+ 3 len) 4))))))
	   ((scavenge-wide-typep x :basic-vector #.(bt:enum-value 'movitz:movitz-vector-element-type :u32))
	    (assert (evenp scan) ()
	      "Scanned u32-vector-header ~S at odd location #x~X." x scan)
	    (let ((len (memref scan 4)))
	      (assert (typep len 'positive-fixnum) ()
		"Scanned basic-vector at ~S with illegal length ~S." scan len)
	      (setf *scan-last* (%word-offset scan #.(movitz:tag :other)))
	      (incf scan (1+ (logand (1+ len) -2)))))
	   ((scavenge-typep x :basic-vector)
	    (if (scavenge-wide-typep x :basic-vector
				     #.(bt:enum-value 'movitz:movitz-vector-element-type :any-t))
		(setf *scan-last* (%word-offset scan #.(movitz:tag :other)))
	      (error "Scanned unknown basic-vector-header ~S at location #x~X." x scan)))
	   ((and (eq x 3) (eq x2 0))
	    (setf *scan-last* scan)
	    (incf scan)
	    (let ((delta (memref scan 0)))
	      (check-type delta positive-fixnum)
	      ;; (warn "at ~S skipping ~S to ~S." scan delta (+ scan delta))
	      (incf scan delta)))
	   (t ;; (typep x 'pointer)
	    (let ((old (memref scan 0)))
	      (unless (eq old (load-global-constant new-unbound-value))
		(let ((new (funcall function old scan)))
		  (when verbose
		    (format *terminal-io* " [~Z => ~Z]" old new))
		  (unless (eq old new)
		    (setf (memref scan 0) new)))))))))))
  (values))

(defun map-stack-vector (function stack start-frame &optional (map-region #'map-header-vals))
  "Map function over the potential pointer words of a stack, starting
at the start-stack-frame location."
  (assert (typep (stack-frame-funobj stack start-frame) 'function) (start-frame)
    "Cannot start map-stack-vector at a non-normal frame.")
  (assert (eq nil stack))
  (map-stack function
	     (stack-frame-uplink stack start-frame)
	     (+ start-frame 2)
	     (+ start-frame 1)
	     map-region))

(defun scavenge-find-pf (location)
  (loop for (slot-name type) in (slot-value (class-of (current-run-time-context)) 'slot-map)
      do (when (eq type 'code-vector-word)
	   (let ((code-vector (%run-time-context-slot slot-name)))
	     (when (location-in-object-p code-vector location)
	       (return code-vector))))))

(defun scavenge-find-code-vector (location casf-funobj esi &optional primitive-function-p edx)
  (flet ((match-funobj (funobj location)
	   (cond
	    ((not (typep funobj 'function))
	     nil)
	    ((let ((x (funobj-code-vector funobj)))
	       (and (location-in-object-p x location) x)))
	    ((let ((x (funobj-code-vector%1op funobj)))
	       (and (typep x 'vector)
		    (location-in-object-p x location)
		    x)))
	    ((let ((x (funobj-code-vector%2op funobj)))
	       (and (typep x 'vector)
		    (location-in-object-p x location)
		    x)))
	    ((let ((x (funobj-code-vector%3op funobj)))
	       (and (typep x 'vector)
		    (location-in-object-p x location)
		    x))))))
    (cond
     ((location-in-object-p (symbol-value 'ret-trampoline) location)
      (symbol-value 'ret-trampoline))
     ((location-in-object-p (%run-time-context-slot 'dynamic-jump-next) location)
      (%run-time-context-slot 'dynamic-jump-next))
     ((eq 0 casf-funobj)
      (let ((dit-code-vector (symbol-value 'default-interrupt-trampoline)))
	(cond
	 ((location-in-object-p dit-code-vector location)
	  dit-code-vector)
	 ((match-funobj esi location))
	 (t (break "DIT returns outside DIT??")))))
     ((match-funobj casf-funobj location))
     ((match-funobj esi location))      
     ((match-funobj edx location))
     ((not (typep casf-funobj 'function))
      (break "Unknown funobj/frame-type: ~S" casf-funobj))
     ((when primitive-function-p
	(scavenge-find-pf location)
	#+ignore
	(%find-code-vector location)))
     (t (with-simple-restart (continue "Try to perform a code-vector-search.")
	  (error "Unable to decode EIP #x~X funobj ~S, ESI ~S."
		 (* 4 location) casf-funobj esi))
	(or (%find-code-vector location)
	    (error "Code-vector-search for EIP #x~X also failed."
		   (* 4 location)))))))

(defun map-stack-value (function value frame)
  (if (not (typep value 'pointer))
      value
    (funcall function value frame)))

(defun map-stack (function frame frame-bottom eip-index map-region)
  "Scavenge the stack starting at location <frame> which ends at <frame-bottom>
and whose return instruction-pointer is at location eip-index."
  (with-funcallable (map-region)
    (loop
      ;; for frame = frame then (stack-frame-uplink frame)
      ;; as frame-end = frame-end then frame
	while (not (eq 0 frame))
	do (map-lisp-vals function (1- frame) frame)
	   (let ((frame-funobj (map-stack-value function (stack-frame-funobj nil frame) frame)))
	     (cond
	      ((eq 0 frame-funobj)
	       (return (map-stack-dit function frame frame-bottom eip-index map-region)))
	      ((not (typep frame-funobj 'function))
	       (error "Unknown stack-frame funobj ~S at ~S" frame-funobj frame))
	      (t (let* ((old-code-vector
			 (scavenge-find-code-vector (stack-frame-ref nil eip-index 0 :location)
						    frame-funobj nil nil)))
		   (map-instruction-pointer function eip-index old-code-vector))
		 (let ((raw-locals (funobj-frame-raw-locals frame-funobj)))
		   (if (= 0 raw-locals)
		       (map-region function frame-bottom frame)
		     (progn
		      (break "~D raw-locals for ~S?" raw-locals frame-funobj)
		      (map-region function (1- frame) frame)
		      (map-region function frame-bottom (- frame 1 raw-locals))))
		   (setf eip-index (+ frame 1)
			 frame-bottom (+ frame 2)
			 frame (stack-frame-uplink nil frame)))))))))

(defun test-stack ()
  (let ((z (current-stack-frame)))
    (map-stack (lambda (x y)
		 (format t "~&[~S]: ~S" y x)
		 x)
	       (stack-frame-uplink nil z) (+ z 2) (+ z 1)
	       #'map-header-vals)))

(defun map-stack-dit (function dit-frame frame-bottom eip-index map-region)
  "Scavenge the stack, starting at a DIT stack-frame." 
  (with-funcallable (map-region)
    (let* ((atomically
	    (dit-frame-ref nil dit-frame :atomically-continuation :unsigned-byte32))
	   (secondary-register-mode-p
	    (logbitp 10 (dit-frame-ref nil dit-frame :eflags :unsigned-byte32)))
	   (casf-frame
	    (dit-frame-casf nil dit-frame))
	   (casf-funobj (map-stack-value function (stack-frame-funobj nil casf-frame) casf-frame))
	   (casf-code-vector (map-stack-value function
					      (case casf-funobj
						(0 (symbol-value 'default-interrupt-trampoline))
						(t (funobj-code-vector casf-funobj)))
					      casf-frame)))
      ;; 1. Scavenge the dit-frame
      (cond
       ((and (not (= 0 atomically))
	     (= 0 (ldb (byte 2 0) atomically)))
	;; Interrupt occurred inside an (non-pf) atomically, so none of the
	;; GC-root registers are active.
	#+ignore (setf (dit-frame-ref nil dit-frame :eax) nil
		       (dit-frame-ref nil dit-frame :ebx) nil
		       (dit-frame-ref nil dit-frame :edx) nil
		       (dit-frame-ref nil dit-frame :esi) nil)
	(map-region function frame-bottom (+ dit-frame 1 (dit-frame-index :scratch1))))
       (secondary-register-mode-p
	;; EBX is also active
	(map-region function frame-bottom (+ dit-frame 1 (dit-frame-index :ebx))))
       (t ;; EDX and EAX too.
	(map-region function frame-bottom (+ dit-frame 1 (dit-frame-index :eax)))))
      ;; The DIT's return-address
      (let* ((interrupted-esi (dit-frame-ref nil dit-frame :esi))
	     (next-frame-bottom (+ dit-frame 1 (dit-frame-index :eflags)))
	     (next-eip-index (+ dit-frame (dit-frame-index :eip)))
	     (old-code-vector
	      (scavenge-find-code-vector (stack-frame-ref nil eip-index 0 :location)
					 0 interrupted-esi
					 nil))
	     (new-code-vector (map-instruction-pointer function eip-index old-code-vector)))
	;; (when atomically (we should be more clever about the stack..))
	(multiple-value-bind (x0-location x0-tag)
	    (stack-frame-ref nil next-frame-bottom 0 :signed-byte30+2)
	  (cond
	   ((and (or (eq x0-tag 1)	; 1 or 5?
		     (eq x0-tag 3)	; 3 or 7?
		     (and (oddp x0-location) (eq x0-tag 2))) ; 6?
		 (location-in-object-p casf-code-vector x0-location))
	    (when (= #xc3 (memref-int (stack-frame-ref nil next-eip-index 0 :unsigned-byte32)
				      :physicalp nil :type :unsigned-byte8))
	      (setf (stack-frame-ref nil next-eip-index 0 :code-vector)
		(symbol-value 'ret-trampoline)))
	    (let* ((old-x0-code-vector
		    (scavenge-find-code-vector (stack-frame-ref nil next-eip-index 0 :location)
					       casf-funobj interrupted-esi t
					       (unless secondary-register-mode-p
						 (dit-frame-ref nil dit-frame :edx)))))
	      (map-instruction-pointer function next-eip-index old-x0-code-vector))
	    (setf next-eip-index next-frame-bottom
		  next-frame-bottom (1+ next-frame-bottom)))
	   (t (multiple-value-bind (x1-location x1-tag)
		  (stack-frame-ref nil next-frame-bottom 1 :signed-byte30+2)
		(when (and (or (eq x1-tag 1) ; 1 or 5?
			       (eq x1-tag 3) ; 3 or 7?
			       (and (oddp x1-location) (eq x1-tag 2))) ; 6?
			   (location-in-object-p casf-code-vector x1-location))
		  (let* ((old-x1-code-vector
			  (scavenge-find-code-vector (stack-frame-ref nil next-eip-index 0 :location)
						     casf-funobj
						     (unless secondary-register-mode-p
						       interrupted-esi)
						     t)))
		    (map-instruction-pointer function next-eip-index old-x1-code-vector))
		  (setf next-eip-index (+ 1 next-frame-bottom)
			next-frame-bottom (+ 2 next-frame-bottom)))))))
	;; proceed
	(map-stack function casf-frame next-frame-bottom next-eip-index map-region)))))

(defun map-instruction-pointer (function location
				&optional (old-code-vector (memref location 0 :type :code-vector)))
  "Update the (raw) instruction-pointer at location,
assuming the pointer refers to old-code-vector."
  (check-type old-code-vector code-vector)
  (assert (location-in-object-p old-code-vector (memref location 0 :type :location)))
  (let ((new-code-vector (funcall function old-code-vector nil)))
    (when (not (eq old-code-vector new-code-vector))
      (break "Code-vector for stack instruction-pointer moved at location ~S" location))
    new-code-vector))


