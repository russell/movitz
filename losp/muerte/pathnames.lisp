;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2008, Frode V. Fjeld
;;;; 
;;;; Description:   Pathnames
;;;; Author:        Frode Vatvedt Fjeld
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(require :muerte/basic-macros)
(require :muerte/defstruct)

(in-package muerte)

(provide :muerte/pathnames)

(defstruct (pathname (:constructor make-pathname-object))
  name)
