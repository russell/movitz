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

(defun make-ratio (numerator denominator)
  (check-type numerator integer)
  (check-type denominator (integer 1 *))
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

(defconstant pi #xea7632a/4aa1a8b)
