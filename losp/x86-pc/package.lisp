;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2001-2004, 
;;;;    Department of Computer Science, University of Tromsø, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      package.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Oct  2 20:30:28 2001
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(require :lib/package)
(provide :x86-pc/package)

(defpackage muerte.x86-pc
  (:use muerte.cl muerte.lib muerte)
  (:export #:io-space-device
	   #:io-space
	   #:device-name
	   #:allocate-io-space
	   #:free-io-space
	   #:io-space-occupants
	   #:with-io-space-lock
	   #:make-io-space
	   #:reset-device
	   ))
