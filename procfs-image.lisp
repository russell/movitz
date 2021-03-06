;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2001-2005, 
;;;;    Department of Computer Science, University of Tromso, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      procfs-image.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Fri Aug 24 11:39:37 2001
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(in-package movitz)

(defclass procfs-image (stream-image)
  ((pid
    :initarg :pid
    :reader image-pid)
   (procfs-connection
    :initarg :procfs
    :reader procfs-image-connection)))

(defmacro with-procfs-image ((pid
			      &key (procfs-var (gensym))
				   (image-var '*image*)
				   (offset #x811b000))
			     &body body)
  `(let ((pid ,pid))
     (procfs:with-procfs-attached (,procfs-var pid :direction :io)
       (let ((,image-var (make-instance 'procfs-image
			   :pid pid
			   :procfs ,procfs-var
			   :stream (procfs:procfs-connection-mem-stream ,procfs-var)
			   :offset ,offset)))
	 ,@body))))

(defclass bochs-image (procfs-image)
  ((register-set-address
    :initarg :register-set-address
    :reader bochs-image-register-set-address)
   (gdtr-address
    :initarg :gdtr-address
    :reader bochs-image-gdtr-address)
   (sregs-address
    :initarg :sregs-address
    :reader bochs-image-sregs-address)
   (start-address
    :initarg :start-address
    :initform #x100000
    :accessor image-start-address)
   ))

(defun read-alist-file (path)
  (with-open-file (stream path :direction :input)
    (loop for c = (read stream nil '#0=:eof)
	until (eq #0# c)
	when (consp c)
	collect c)))

(defun bochs-parameter (p path)
  (cdr (assoc p (read-alist-file path))))

(defmacro with-bochs-image ((&key (path #p"bochs-parameters")
				  (procfs-var (gensym))
				  (image-var '*image*))
			    &body body)
  `(let ((bt:*endian* :little-endian)
	 (pid (bochs-parameter :pid ,path)))
     (procfs:with-procfs-attached (,procfs-var pid :direction :io)
       (let ((,image-var (make-instance 'bochs-image
			   :ds-segment-base (if (boundp '*previous-image*)
						(image-ds-segment-base *previous-image*)
					      0)
			   :cs-segment-base (if (boundp '*previous-image*)
						(image-cs-segment-base *previous-image*)
					      0)
			   :pid pid
			   :procfs ,procfs-var
			   :stream (procfs:procfs-connection-mem-stream ,procfs-var)
			   :register-set-address (+ 0 (bochs-parameter :cpu ,path))
			   :offset (bochs-parameter :memory ,path)
			   :sregs-address (bochs-parameter :sregs ,path)
			   :gdtr-address (bochs-parameter :gdtr ,path))))
	 ,@body))))
  

(define-unsigned r32 4 :little-endian)

(define-binary-class bochs-registers ()
  ((eax :binary-type r32)
   (ecx :binary-type r32)
   (edx :binary-type r32)
   (ebx :binary-type r32)
   (esp :binary-type r32)
   (ebp :binary-type r32)
   (esi :binary-type r32)
   (edi :binary-type r32)
   (eip :binary-type r32)))

(defmethod image-register32 ((image bochs-image) register-name)
  (assert (file-position (image-stream image)
			 (bochs-image-register-set-address image)))
  (let ((register-set (read-binary 'bochs-registers (image-stream image))))
    (slot-value register-set (intern register-name :movitz))))

(defun register32 (register-name)
  (image-register32 *image* register-name))

(defmethod image-movitz-to-lisp-object ((image procfs-image) expr)
  (etypecase expr
    (cons (mapcar #'movitz-print expr))
    ((not movitz-object)
     expr)
    ((or movitz-null movitz-run-time-context) nil)
    (movitz-std-instance expr)
    (movitz-symbol
     (intern (movitz-print (movitz-symbol-name expr))))
    (movitz-string
     (map 'string #'identity
	  (movitz-vector-symbolic-data expr)))
    (movitz-fixnum
     (movitz-fixnum-value expr))
    (movitz-bignum
     (movitz-bignum-value expr))
    (movitz-basic-vector
     (map 'vector #'movitz-print (movitz-vector-symbolic-data expr)))
    (movitz-cons
     (cons (movitz-print (movitz-car expr))
	   (movitz-print (movitz-cdr expr))))
    (movitz-funobj
     expr)))


(defmethod report-gdtr ((image bochs-image))
  (assert (file-position (image-stream image)
			 (bochs-image-gdtr-address image)))
  (let* ((*endian* :little-endian)
	 (base (read-binary 'u32 (image-stream image)))
	 (limit (read-binary 'u16 (image-stream image))))
    (format t "~&GDTR: base #x~X, limit #x~X~%" base limit)
    (assert (zerop (mod base 4)) ()
      "Base is not aligned to 16 bytes.")
    (assert (zerop (mod (1+ limit) 8)) ()
      "Limit is not aligned to 8 bytes.")
    (setf (image-stream-position *image*) base)
    (dotimes (i (truncate (1+ limit) 8))
      (format t "Descriptor ~D: ~<~W~>~%" i (read-binary 'code-segment-descriptor (image-stream image))))
    (values)))
    
(defmethod report-segment-registers ((image bochs-image))
  (let* ((*endian* :little-endian))
    (format t "~&Segment registers: ")
    (loop for (reg . address) in (bochs-image-sregs-address image)
	do (assert (file-position (image-stream image) address))
	do (format t "~A: #x~X " reg (read-binary 'u16 (image-stream image)))
	finally (terpri)))
  (values))

(defun current-stack-frame ()
  (image-register32 *image* :ebp))

(defun previous-stack-frame (stack-frame)
  (get-word stack-frame))

(defun stack-frame-funobj (stack-frame)
  (when (and (plusp stack-frame)
	     (zerop (ldb (byte 2 0) stack-frame)))
    (let ((x (movitz-word (get-word (- stack-frame 4)))))
      (and (typep x 'movitz-funobj) x))))

(defun stack-frame-return-address (stack-frame)
  (when (zerop (ldb (byte 2 0) stack-frame))
    (get-word (- stack-frame -4))))

(defun interrupt-frame-index (name)
  (- 5 (position name
		 (symbol-value 'muerte::+dit-frame-map+))))

(defun debug-get-object (word spartan)
  (if spartan
      word
    (handler-case
	(let ((object (movitz-word word)))
	  (typecase object
	    ((or movitz-funobj movitz-struct movitz-std-instance)
	     object)
	    (t (movitz-print object))))
      (serious-condition (c) (list :word-error word c)))))

(defun backtrace (&key (reqs t) print-frames print-returns spartan)
  (format t "~&Backtracing from EIP = #x~X: "
	  (image-register32 *image* :eip))
  ;; (search-image-funobj (image-register32 *image* :eip))
  (format t "~&Current ESI: #x~X.~%"
	  (image-register32 *image* :esi))
  (let ((*print-length* 20))
    (loop for stack-frame = (current-stack-frame) then (previous-stack-frame stack-frame)
	while (plusp stack-frame)
	unless (zerop (mod stack-frame 4))
	do (format t "[frame #x~8,'0x]" stack-frame)
	   (loop-finish)
	do (let ((movitz-name (funobj-name (stack-frame-funobj stack-frame))))
	     (typecase movitz-name
	       (null
		;; (write-string "?")
		(let* ((eax (get-word (+ (* 4 (interrupt-frame-index :eax)) stack-frame)))
		       (ebx (get-word (+ (* 4 (interrupt-frame-index :ebx)) stack-frame)))
		       (ecx (get-word (+ (* 4 (interrupt-frame-index :ecx)) stack-frame)))
		       (edx (get-word (+ (* 4 (interrupt-frame-index :edx)) stack-frame)))
		       (edi (get-word (+ (* 4 (interrupt-frame-index :edi)) stack-frame)))
		       (eip (get-word (+ (* 4 (interrupt-frame-index :eip)) stack-frame)))
		       (esi (get-word (+ (* 4 (interrupt-frame-index :esi)) stack-frame)))
		       (exception (get-word (+ (* 4 (interrupt-frame-index :exception-vector))
					       stack-frame))))
		  (format t "#x~X {EAX: #x~X, EBX: #x~X, ECX: #x~X, EDX: #x~X, EDI: #x~X, ESI: #x~X, EIP: #x~X, exception ~D}"
			  (truncate stack-frame 4)
			  eax ebx ecx edx edi esi eip exception)))
	       (movitz-symbol
		(let ((name (movitz-print movitz-name)))
		  (when print-frames
		    (format t "~S " (truncate stack-frame 4)))
		  (when (string= name 'toplevel-function)
		    (loop-finish))
		  (when reqs
		    (format t "(~A ~S ~S)"
			    (symbol-name name)
			    (debug-get-object (get-word (+ stack-frame -8)) spartan)
			    (debug-get-object (get-word (+ stack-frame -12)) spartan)))
		  (when print-returns
		    (format t " (#x~X)" (stack-frame-return-address stack-frame)))))
	       (t (when print-frames
		    (format t "~S " (truncate stack-frame 4)))
		  (write (movitz-print movitz-name)))))
	do (format t "~& => ")))
  (values))

(defun stack-frame (image)
  (do-stack-frame (image-register32 image :ebp) 0))
    
(defun get-word (address &optional physicalp)
  (unless (zerop (ldb (byte 2 0) address))
    (warn "Non-aligned address to GET-WORD: #x~8,'0X." address))
  (setf (image-stream-position *image* physicalp) address)
  (values (read-binary 'word (image-stream *image*))))

(defun do-stack-frame (frame-address count)
  (warn "Frame ~D: #x~8,'0X" count frame-address)
  (when (< count 10)
    (do-stack-frame (get-word frame-address) (1+ count))))


(defun current-dynamic-context ()
  (slot-value (image-run-time-context *image*) 'dynamic-env))

(defun stack-ref-p (pointer)
  (let ((top #xa0000)
	(bottom (image-register32 *image* :esp)))
    (<= bottom pointer top)))

(defun stack-ref (pointer offset index type)
  (assert (stack-ref-p pointer) (pointer)
    "Stack pointer not in range: #x~X" pointer)
  (ecase type
    (:lisp
     (movitz-word (get-word (+ pointer offset (* 4 index)))))
    (:unsigned-byte32
     (values (get-word (+ pointer offset (* 4 index)))))))

(defun dynamic-context-uplink (dynamic-context)
  (stack-ref dynamic-context 12 0 :unsigned-byte32))

(defun dynamic-context-tag (dynamic-context)
  (stack-ref dynamic-context 4 0 :lisp))

(defun load-global-constant (slot-name)
  (slot-value (image-run-time-context *image*) slot-name))

(defun image-eq (x y)
  (eql (movitz-intern x) (movitz-intern y)))

(defun print-dynamic-context (&optional (initial-dynamic-context (current-dynamic-context)))
  (loop for dynamic-context = initial-dynamic-context
      then (dynamic-context-uplink dynamic-context)
      while (stack-ref-p dynamic-context)
      do (let ((tag (dynamic-context-tag dynamic-context)))
	   (cond
	    ((image-eq tag (load-global-constant 'unbound-value))
	     (format t "~&#x~X: name: ~A => value: ~A"
		     dynamic-context
		     (stack-ref dynamic-context 0 0 :lisp)
		     (stack-ref dynamic-context 8 0 :lisp)))
	    (t (format t "~&#x~X:  tag: #x~X [name #x~X, val #x~X]"
		       dynamic-context
		       (stack-ref dynamic-context 4 0 :unsigned-byte32)
		       (stack-ref dynamic-context 0 0 :unsigned-byte32)
		       (stack-ref dynamic-context 8 0 :unsigned-byte32)))))
      finally (format t "~&Last uplink: #x~X~%" dynamic-context)
	      (values)))


(defvar *previous-image*)

#+allegro
(top-level:alias ("bochs" 0) (&optional form)
  (let ((*previous-image* *image*))
    (with-bochs-image ()
      (let ((image *image*))
	(with-simple-restart (continue "Exit this bochs session [pid=~D]" (image-pid image))
	  (if form
	      (let ((x (eval form)))
		(format t "~&~W" x)
		x)
	    (invoke-debugger
	     (format nil "Established Bochs session [pid=~D]. ~S is ~S"
		     (image-pid image)
		     '*previous-image*
		     *previous-image*))))))))

#+allegro
(top-level:alias ("unbochs" 3) (&optional form)
  (let ((*image* *previous-image*)
	(image *image*))
    (with-simple-restart (continue "Exit this unbochs session")
      (if form
	  (let ((x (eval form)))
	    (format t "~&~W" x)
	    x)
	(invoke-debugger "Established connection to unBochs ~S" image)))))
