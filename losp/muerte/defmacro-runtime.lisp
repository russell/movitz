;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2008, Frode V. Fjeld
;;;; 
;;;; Filename:      defmacro-runtime.lisp
;;;; Author:        Frode Vatvedt Fjeld
;;;; Created at:    Wed Nov  8 18:44:57 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(in-package #:muerte)

(provide :muerte/defmacro-runtime)

(defmacro/cross-compilation defmacro (name lambda-list &body macro-body)
  `(progn
     (defmacro/run-time ,name ,lambda-list ,@macro-body)
     (defmacro/compile-time ,name ,lambda-list ,macro-body)
     ',name))
