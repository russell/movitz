;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2003-2004, 
;;;;    Department of Computer Science, University of Tromsoe, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      pci.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Sun Dec 14 22:33:42 2003
;;;;                
;;;; $Id$
;;;;                
;;;;------------------------------------------------------------------

(in-package muerte.x86-pc)

(provide :x86-pc/pci)

(defun pci-word (designator)
  "Map an integer or 4-character string to an (unsigned-byte 32)."
  (etypecase designator
    ((unsigned-byte 32)
     designator)
    ((signed-byte 32)
     (ldb (byte 32 0) designator))
    (string
     (loop for c across designator as i upfrom 0 by 8
	 summing (ash (char-code c) i)))))

(defun pci-string (integer)
  "Map a 32-bit value to a 4-character string."
  (check-type integer (or (signed-byte 32)
			  (unsigned-byte 32)))
  (let ((string (make-string 4)))
    (setf (char string 0) (code-char (ldb (byte 8 0) integer))
	  (char string 1) (code-char (ldb (byte 8 8) integer))
	  (char string 2) (code-char (ldb (byte 8 16) integer))
	  (char string 3) (code-char (ldb (byte 8 24) integer)))
    string))

(defun find-bios32-base ()
  (loop for bios32 from #xe0000 to #xffff0 by 16
      if (and (= (memref-int bios32) #x5f32335f)
	      (loop with checksum = 0
		  as i from 0 below (* 16 (memref-int bios32 :offset 9 :type :unsigned-byte8))
		  do (incf checksum
			   (memref-int bios32 :offset i :type :unsigned-byte8))
		  finally (return (= 0 (ldb (byte 8 0 ) checksum)))))
      return bios32))

(defvar *bios32-base* nil)
(defvar *pcibios-entry* nil)

(defun find-bios32-pci ()
  (let ((bios32-base (find-bios32-base)))
    (assert bios32-base "No bios32 found.")
    (multiple-value-bind (eax ebx ecx edx)
	(pci-far-call (memref-int bios32-base :offset 4)
		      :eax (pci-word "$PCI"))
      (declare (ignore ecx))
      (ecase (ldb (byte 8 0) eax)
	(#x80 (error "The PCI bios32 service isn't present."))
	(#x81 (error "The PCI bios32 service doesn't exist."))
	(#x00 (+ ebx edx))))))

(defun pci-bios-present ()
  (multiple-value-bind (eax ebx ecx edx cf)
      (pci-far-call (find-bios32-pci) :eax #xb101)
    (values (pci-string edx)
	    (ldb (byte 8 8) eax)	; AH: Present status
	    (ldb (byte 8 0) eax)	; AL: Hardware mechanism
	    (ldb (byte 8 8) ebx)	; BH: Interface Level Major Version
	    (ldb (byte 8 0) ebx)	; BL: Interface Level Minor Version
	    (ldb (byte 8 0) ecx))))	; CL: Number of last PCI bus in the system
		
(defun find-pci-device (vendor device &optional (index 0))
  (multiple-value-bind (eax ebx ecx edx cf)
      (pci-far-call (find-bios32-pci)
		    :eax #xb102
		    :ecx device
		    :edx vendor
		    :esi index)
    (unless cf
      (values (ldb (byte 8 8) ebx)	; Bus
	      (ldb (byte 5 3) ebx)	; Device
	      (ldb (byte 3 0) ebx)	; Function
	      (ecase (ldb (byte 8 8) eax)
		(#x00 :successful)
		(#x86 :device-not-found)
		(#x83 :bad-vendor-id))))))

(defun find-pci-class-code (class-code &optional (index 0))
  (multiple-value-bind (eax ebx ecx edx cf)
      (pci-far-call (find-bios32-pci)
		    :eax #xb103
		    :ecx class-code
		    :esi index)
    (declare (ignore ecx edx))
    (unless cf
      (values (ldb (byte 8 8) ebx)	; Bus
	      (ldb (byte 5 3) ebx)	; Device
	      (ldb (byte 3 0) ebx)	; Function
	      (ecase (ldb (byte 8 8) eax)
		(#x00 :successful)
		(#x86 :device-not-found))))))


(defun pci-far-call (address &key (cs 8) (eax 0) (ebx 0) (ecx 0) (edx 0) (esi 0))
  "Make a 'far call' to cs:address with the provided values for eax and ebx.
Returns the values of registers AL, EBX, ECX, and EDX, and status of CF.
 (NB: For now only the lower 30 bits of registers are actually returned.)
The stack discipline is broken during this call, so we disable interrupts
in a somewhat feeble attempt to avoid trouble."
  (check-type address (unsigned-byte 32))
  (without-interrupts
    (with-inline-assembly (:returns :multiple-values)
      ;; Enter atomically mode
      (:declare-label-set restart-pci-far-call (restart))
      (:locally (:pushl (:edi (:edi-offset :dynamic-env))))
      (:pushl 'restart-pci-far-call)
      (:locally (:pushl (:edi (:edi-offset :atomically-continuation))))
      (:pushl :ebp)
     restart
      (:movl (:esp) :ebp)
      (:locally (:movl :esp (:edi (:edi-offset :atomically-continuation))))

      (:load-lexical (:lexical-binding cs) :untagged-fixnum-ecx)
      (:pushl :ecx)			; Code segment
      (:load-lexical (:lexical-binding address) :untagged-fixnum-ecx)
      (:pushl :ecx)			; Code address
      (:load-lexical (:lexical-binding eax) :untagged-fixnum-ecx)
      (:pushl :ecx)			; push EAX
      (:load-lexical (:lexical-binding ebx) :untagged-fixnum-ecx)
      (:pushl :ecx)			; push EBX
      (:load-lexical (:lexical-binding edx) :untagged-fixnum-ecx)
      (:pushl :ecx)			; push EDX
      (:load-lexical (:lexical-binding esi) :untagged-fixnum-ecx)
      (:pushl :ecx)			; push ESI
      (:load-lexical (:lexical-binding ecx) :untagged-fixnum-ecx)
      (:popl :esi)
      (:popl :edx)
      (:popl :ebx)
      (:popl :eax)
      (:call-segment (:esp))
      (:leal (:esp 8) :esp)
      (:locally (:movl :edi (:edi (:edi-offset values) 8)))
      (:jnc 'cf=0)
      (:locally (:pushl (:edi (:edi-offset t-symbol))))
      (:locally (:popl (:edi (:edi-offset values) 8)))
     cf=0
      (:pushl :eax)
      (:pushl :ebx)
      (:pushl :edx)
      (:locally (:movl 3 (:edi (:edi-offset num-values))))
      (:call-local-pf box-u32-ecx)	; ECX
      (:locally (:movl :eax (:edi (:edi-offset values) 0)))
      (:popl :ecx)			; EDX
      (:call-local-pf box-u32-ecx)
      (:locally (:movl :eax (:edi (:edi-offset values) 4)))
      (:popl :ecx)			; EBX
      (:call-local-pf box-u32-ecx)
      (:locally (:movl :eax (:edi (:edi-offset scratch1))))
      (:popl :ecx)			; EAX
      (:call-local-pf box-u32-ecx)
      (:locally (:movl (:edi (:edi-offset scratch1)) :ebx))
      (:movl 5 :ecx)
      (:movl (:ebp -4) :esi)
      (:stc)
      ;; Exit atomical-mode
      (:locally (:movl 0 (:edi (:edi-offset atomically-continuation))))
      (:leal (:esp 16) :esp))))
