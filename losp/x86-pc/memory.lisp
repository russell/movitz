;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2001-2002, 2004, 
;;;;    Department of Computer Science, University of Troms�, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      memory.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Thu Oct 11 16:32:11 2001
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(require :x86-pc/package)
(provide :x86-pc/memory)

(in-package muerte.x86-pc)

(defun memory-size ()
  (let ((kilobyte-memsize
	 (+ #x400
	    (prog1
		(dpb (progn
		       (setf (io-port #x70 :unsigned-byte8) #x18)
		       (io-port #x71 :unsigned-byte8))
		     (byte 8 8)
		     (progn
		       (setf (io-port #x70 :unsigned-byte8) #x17)
		       (io-port #x71 :unsigned-byte8)))))))
    (truncate kilobyte-memsize 1024)))
