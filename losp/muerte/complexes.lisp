;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2008, Frode V. Fjeld
;;;; 
;;;; Description:   Complex numbers
;;;; Author:        Frode Vatvedt Fjeld
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(require :muerte/basic-macros)
(require :muerte/defstruct)

(in-package muerte)

(provide :muerte/complexes)

(defstruct (complex (:constructor make-complex-number)
		    (:conc-name ""))
  realpart
  imagpart)
