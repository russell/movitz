;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2003-2004, 
;;;;    Department of Computer Science, University of Tromso, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      ratios.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Jul 20 00:39:59 2004
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(require :muerte/basic-macros)
(require :muerte/arithmetic-macros)
(require :muerte/defstruct)
(provide :muerte/ratios)

(in-package muerte)

(defun %make-ratio (numerator denominator)
  (macrolet
      ((do-it ()
	 `(with-allocation-assembly (4 :fixed-size-p t
				       :object-register :eax)
	    (:load-lexical (:lexical-binding numerator) :ebx)
	    (:load-lexical (:lexical-binding denominator) :edx)
	    (:movl ,(movitz:tag :ratio) (:eax (:offset movitz-ratio type)))
	    (:movl :edi (:eax (:offset movitz-ratio dummy2)))
	    (:movl :ebx (:eax (:offset movitz-ratio numerator)))
	    (:movl :edx (:eax (:offset movitz-ratio denominator))))))
    (do-it)))

(defun make-ratio (numerator denominator)
  (check-type numerator integer)
  (check-type denominator (integer 1 *))
  (%make-ratio numerator denominator))

(defun ratio-p (x)
  (typep x 'ratio))

(defun ratio-numerator (x)
  (check-type x ratio)
  (%ratio-numerator x))

(defun ratio-denominator (x)
  (check-type x ratio)
  (%ratio-denominator x))

(defun make-rational (numerator denominator)
  (check-type numerator integer)
  (check-type denominator integer)
  (cond
   ((= 1 denominator)
    numerator)
   ((minusp denominator)
    (make-rational (- numerator) (- denominator)))
   ((= 0 denominator)
    (error 'division-by-zero))
   (t (let ((gcd (gcd numerator denominator)))
	(if (= denominator gcd)
	    (values (truncate numerator denominator))
	  (make-ratio (truncate numerator gcd)
		      (truncate denominator gcd)))))))

(defun numerator (x)
  (etypecase x
    (integer x)
    (ratio (%ratio-numerator x))))

(defun denominator (x)
  (etypecase x
    (integer 1)
    (ratio (%ratio-denominator x))))

;;; "Floats"

(defconstant most-negative-short-float most-negative-fixnum)
(defconstant most-negative-single-float most-negative-fixnum)
(defconstant most-negative-long-float most-negative-fixnum)
(defconstant most-negative-double-float most-negative-fixnum)

(defconstant least-positive-short-float 1/100000)
(defconstant least-positive-single-float 1/100000)
(defconstant least-positive-double-float 1/100000)
(defconstant least-positive-long-float 1/100000)

;;;

(defconstant pi #xea7632a/4aa1a8b)

(defvar long-float-epsilon 1/10000)

(defun float (x &optional proto)
  (declare (ignore proto))
  (check-type x float)
  x)

(defun float-radix (x)
  (if (integerp x)
      2
      (denominator x)))

(defun integer-decode-float (x)
  (if (integerp x)
      (if (minusp x)
	  (values x 0 -1)
	  (values x 0 1))
      (let ((n (numerator x)))
	(if (minusp x)
	    (values n -1 -1)
	    (values n -1 1)))))

(defun decode-float (x)
  (multiple-value-bind (n sign)
      (let ((n (numerator x)))
	(if (minusp n)
	    (values (- n) -1)
	    (values n 1)))
    (let* ((r (float-radix x))
	   (d (denominator x))
	   (e (if (= 1 d) 0 -1)))
      (do () ((< n 1)
	      (values n e sign))
	(setf n (/ n r))
	(incf e)))))

(defun cos (x)
  "http://mathworld.wolfram.com/Cosine.html"
  (do* ((rad (mod x 44/7))
        (n2 0 (+ n2 2))
        (sign 1 (- sign))
        (denominator 1 (* denominator (1- n2) n2))
        (term 1 (/ (expt rad n2)
                   denominator))
        (sum 1 (+ sum (* sign term))))
       ((<= term long-float-epsilon)
        sum)))

(defun sin (x)
  (cos (- x (/ pi 2))))
