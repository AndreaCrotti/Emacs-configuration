;;;; -*- indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
;;;
;;; swank-abcl.lisp --- Armedbear CL specific code for SLIME. 
;;;
;;; Adapted from swank-acl.lisp, Andras Simon, 2004
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed. 
;;;  

(in-package :swank-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :collect) ;just so that it doesn't spoil the flying letters
  (require :pprint))

;;; The introduction of SYS::*INVOKE-DEBUGGER-HOOK* obliterates the
;;; need for redefining BREAK. The following should thus be removed at
;;; some point in the future.
#-#.(swank-backend:with-symbol '*invoke-debugger-hook* 'sys)
(defun sys::break (&optional (format-control "BREAK called") 
                   &rest format-arguments)
  (let ((sys::*saved-backtrace* 
         #+#.(swank-backend:with-symbol 'backtrace 'sys) 
         (sys:backtrace)
         #-#.(swank-backend:with-symbol 'backtrace 'sys)
         (ext:backtrace-as-list)))
    (with-simple-restart (continue "Return from BREAK.")
      (invoke-debugger
       (sys::%make-condition 'simple-condition
                             (list :format-control format-control
                                   :format-arguments format-arguments))))
    nil))

(defimplementation make-output-stream (write-string)
  (ext:make-slime-output-stream write-string))

(defimplementation make-input-stream (read-string)
  (ext:make-slime-input-stream read-string  
                               (make-synonym-stream '*standard-output*)))

(defimplementation call-with-compilation-hooks (function)
  (funcall function))

;;; swank-mop

;;dummies and definition

(defclass standard-slot-definition ()())

;(defun class-finalized-p (class) t)

(defun slot-definition-documentation (slot)
  (declare (ignore slot))
  #+nil (documentation slot 't))

(defun slot-definition-type (slot)
  (declare (ignore slot))
  t)

(defun class-prototype (class)
  (declare (ignore class))
  nil)

(defun generic-function-declarations (gf)
  (declare (ignore gf))
  nil)

(defun specializer-direct-methods (spec)
  (mop::class-direct-methods spec))

(defun slot-definition-name (slot)
  (mop::%slot-definition-name slot))

(defun class-slots (class)
  (mop::%class-slots class))

(defun method-generic-function (method)
  (mop::%method-generic-function method))

(defun method-function (method)
  (mop::%method-function method))

(defun slot-boundp-using-class (class object slotdef)
  (declare (ignore class))
  (system::slot-boundp object (slot-definition-name slotdef)))

(defun slot-value-using-class (class object slotdef)
  (declare (ignore class))
  (system::slot-value object (slot-definition-name slotdef)))

(import-to-swank-mop
 '( ;; classes
   cl:standard-generic-function
   standard-slot-definition ;;dummy
   cl:method
   cl:standard-class
   #+#.(swank-backend:with-symbol 'compute-applicable-methods-using-classes 'mop)
   mop::compute-applicable-methods-using-classes
   ;; standard-class readers
   mop::class-default-initargs
   mop::class-direct-default-initargs
   mop::class-direct-slots
   mop::class-direct-subclasses
   mop::class-direct-superclasses
   mop::eql-specializer
   mop::class-finalized-p 
   cl:class-name
   mop::class-precedence-list
   class-prototype ;;dummy
   class-slots
   specializer-direct-methods 
   ;; eql-specializer accessors
   mop::eql-specializer-object
   ;; generic function readers
   mop::generic-function-argument-precedence-order
   generic-function-declarations ;;dummy
   mop::generic-function-lambda-list
   mop::generic-function-methods
   mop::generic-function-method-class
   mop::generic-function-method-combination
   mop::generic-function-name
   ;; method readers
   method-generic-function
   method-function
   mop::method-lambda-list
   mop::method-specializers
   mop::method-qualifiers
   ;; slot readers
   mop::slot-definition-allocation
   slot-definition-documentation ;;dummy
   mop::slot-definition-initargs
   mop::slot-definition-initform
   mop::slot-definition-initfunction
   slot-definition-name
   slot-definition-type ;;dummy
   mop::slot-definition-readers
   mop::slot-definition-writers
   slot-boundp-using-class
   slot-value-using-class
   ))

;;;; TCP Server


(defimplementation preferred-communication-style ()
#+#.(cl:if (cl:find-package :threads) '(:and) '(:or))
  :spawn
#-#.(cl:if (cl:find-package :threads) '(:and) '(:or))
  nil
)

(defimplementation create-socket (host port)
  (ext:make-server-socket port))

(defimplementation local-port (socket)
  (java:jcall (java:jmethod "java.net.ServerSocket" "getLocalPort") socket))

(defimplementation close-socket (socket)
  (ext:server-socket-close socket))

(defimplementation accept-connection (socket 
                                      &key external-format buffering timeout)
  (declare (ignore buffering timeout))
  (ext:get-socket-stream (ext:socket-accept socket)
                         :external-format external-format))

;;;; External formats

(defvar *external-format-to-coding-system*
  '((:iso-8859-1 "latin-1" "iso-latin-1" "iso-8859-1")
    ((:iso-8859-1 :eol-style :lf) "latin-1-unix" "iso-latin-1-unix" "iso-8859-1-unix")
    (:utf-8 "utf-8")
    ((:utf-8 :eol-style :lf) "utf-8-unix")
    (:euc-jp "euc-jp")
    ((:euc-jp :eol-style :lf) "euc-jp-unix")
    (:us-ascii "us-ascii")
    ((:us-ascii :eol-style :lf) "us-ascii-unix")))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x)
                    (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

;;;; Unix signals

(defimplementation getpid ()
  (handler-case 
      (let* ((runtime 
              (java:jstatic "getRuntime" "java.lang.Runtime"))
             (command
              (java:jnew-array-from-array 
               "java.lang.String" #("sh" "-c" "echo $PPID")))
             (runtime-exec-jmethod 		
              ;; Complicated because java.lang.Runtime.exec() is
              ;; overloaded on a non-primitive type (array of
              ;; java.lang.String), so we have to use the actual
              ;; parameter instance to get java.lang.Class
              (java:jmethod "java.lang.Runtime" "exec" 
                            (java:jcall 
                             (java:jmethod "java.lang.Object" "getClass")
                             command)))
             (process 
              (java:jcall runtime-exec-jmethod runtime command))
             (output 
              (java:jcall (java:jmethod "java.lang.Process" "getInputStream")
                          process)))
         (java:jcall (java:jmethod "java.lang.Process" "waitFor")
                     process)
	 (loop :with b :do 
	    (setq b 
		  (java:jcall (java:jmethod "java.io.InputStream" "read")
			      output))
	    :until (member b '(-1 #x0a))	; Either EOF or LF
	    :collecting (code-char b) :into result
	    :finally (return 
		       (parse-integer (coerce result 'string)))))
    (t () 0))) 

(defimplementation lisp-implementation-type-name ()
  "armedbear")

(defimplementation set-default-directory (directory)
  (let ((dir (sys::probe-directory directory)))
    (when dir (setf *default-pathname-defaults* dir))
    (namestring dir)))


;;;; Misc

(defimplementation arglist (fun)
  (cond ((symbolp fun)
          (multiple-value-bind (arglist present) 
              (sys::arglist fun)
            (when (and (not present)
                       (fboundp fun)
                       (typep (symbol-function fun) 'standard-generic-function))
              (setq arglist
                    (mop::generic-function-lambda-list (symbol-function fun))
                    present
                    t))
            (if present arglist :not-available)))
        (t :not-available)))

(defimplementation function-name (function)
  (nth-value 2 (function-lambda-expression function)))

(defimplementation macroexpand-all (form)
  (macroexpand form))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind &optional (sym symbol))
             (or (documentation sym kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :function (if (fboundp symbol)
                     (doc 'function)))
      (maybe-push
       :class (if (find-class symbol nil)
                  (doc 'class)))
      result)))


(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable 
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable 
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))


;;;; Debugger

;;; Copied from swank-sbcl.lisp.
(defun make-invoke-debugger-hook (hook)
  #'(lambda (condition old-hook)
      ;; Notice that *INVOKE-DEBUGGER-HOOK* is tried before
      ;; *DEBUGGER-HOOK*, so we have to make sure that the latter gets
      ;; run when it was established locally by a user (i.e. changed
      ;; meanwhile.)
      (if *debugger-hook*
          (funcall *debugger-hook* condition old-hook)
          (funcall hook condition old-hook))))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        #+#.(swank-backend:with-symbol '*invoke-debugger-hook* 'sys)
        (sys::*invoke-debugger-hook* (make-invoke-debugger-hook hook)))
    (funcall fun)))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  #+#.(swank-backend:with-symbol '*invoke-debugger-hook* 'sys)
  (setq sys::*invoke-debugger-hook* (make-invoke-debugger-hook function)))

(defvar *sldb-topframe*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* ((magic-token (intern "SWANK-DEBUGGER-HOOK" 'swank))
         (*sldb-topframe* 
          #+#.(swank-backend:with-symbol 'backtrace 'sys)
          (second (member magic-token (sys:backtrace)
                          :key #'(lambda (frame) 
                                   (first (sys:frame-to-list frame)))))
          #-#.(swank-backend:with-symbol 'backtrace 'sys)
          (second (member magic-token (ext:backtrace-as-list)
                          :key #'(lambda (frame) 
                                   (first frame))))
          ))    
    (funcall debugger-loop-fn)))

(defun backtrace (start end)
  "A backtrace without initial SWANK frames."
  (let ((backtrace 
         #+#.(swank-backend:with-symbol 'backtrace 'sys)
         (sys:backtrace)
         #-#.(swank-backend:with-symbol 'backtrace 'sys)
         (ext:backtrace-as-list)
         ))
    (subseq (or (member *sldb-topframe* backtrace) backtrace)
            start end)))

(defun nth-frame (index)
  (nth index (backtrace 0 nil)))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (backtrace start end)))

(defimplementation print-frame (frame stream)
  (write-string
   #+#.(swank-backend:with-symbol 'backtrace 'sys)
   (sys:frame-to-string frame)
   #-#.(swank-backend:with-symbol 'backtrace 'sys)
   (string-trim '(#\space #\newline) (prin1-to-string frame))
   stream))

(defimplementation frame-locals (index)
  `(,(list :name "??" :id 0 :value "??")))

#+nil
(defimplementation disassemble-frame (index)
  (disassemble (debugger:frame-function (nth-frame index))))

(defimplementation frame-source-location (index)
  (list :error (format nil "Cannot find source for frame: ~A"
                       (nth-frame index))))

#+nil
(defimplementation eval-in-frame (form frame-number)
  (debugger:eval-form-in-context 
   form
   (debugger:environment-of-frame (nth-frame frame-number))))

#+nil
(defimplementation return-from-frame (frame-number form)
  (let ((frame (nth-frame frame-number)))
    (multiple-value-call #'debugger:frame-return 
      frame (debugger:eval-form-in-context 
             form 
             (debugger:environment-of-frame frame)))))
                         
;;; XXX doesn't work for frames with arguments 
#+nil
(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (debugger:frame-retry frame (debugger:frame-function frame))))
                          
;;;; Compiler hooks

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename*)

(in-package :swank-backend)

(defvar *abcl-signaled-conditions*)

(defun handle-compiler-warning (condition)
  (let ((loc (when (and jvm::*compile-file-pathname* 
                        system::*source-position*)
               (cons jvm::*compile-file-pathname* system::*source-position*))))
    ;; filter condition signaled more than once.
    (unless (member condition *abcl-signaled-conditions*) 
      (push condition *abcl-signaled-conditions*) 
      (signal (make-condition
               'compiler-condition
               :original-condition condition
               :severity :warning
               :message (format nil "~A" condition)
               :location (cond (*buffer-name*
                                (make-location 
                                 (list :buffer *buffer-name*)
                                 (list :offset *buffer-start-position* 0)))
                               (loc
                                (destructuring-bind (file . pos) loc
                                  (make-location
                                   (list :file (namestring (truename file)))
                                   (list :position (1+ pos)))))
                               (t  
                                (make-location
                                 (list :file (namestring *compile-filename*))
                                 (list :position 1)))))))))

(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format)
  (declare (ignore external-format))
  (let ((jvm::*resignal-compiler-warnings* t)
        (*abcl-signaled-conditions* nil))
    (handler-bind ((warning #'handle-compiler-warning))
      (let ((*buffer-name* nil)
            (*compile-filename* input-file))
        (multiple-value-bind (fn warn fail) 
            (compile-file input-file :output-file output-file)
          (values fn warn
                  (and fn load-p
                       (not (load fn)))))))))

(defimplementation swank-compile-string (string &key buffer position filename
                                         policy)
  (declare (ignore filename policy))
  (let ((jvm::*resignal-compiler-warnings* t)
        (*abcl-signaled-conditions* nil))
    (handler-bind ((warning #'handle-compiler-warning))                 
      (let ((*buffer-name* buffer)
            (*buffer-start-position* position)
            (*buffer-string* string))
        (funcall (compile nil (read-from-string
                               (format nil "(~S () ~A)" 'lambda string))))
        t))))

#|
;;;; Definition Finding

(defun find-fspec-location (fspec type)
  (let ((file (excl::fspec-pathname fspec type)))
    (etypecase file
      (pathname
       (let ((start (scm:find-definition-in-file fspec type file)))
         (make-location (list :file (namestring (truename file)))
                        (if start
                            (list :position (1+ start))
                            (list :function-name (string fspec))))))
      ((member :top-level)
       (list :error (format nil "Defined at toplevel: ~A" fspec)))
      (null 
       (list :error (format nil "Unkown source location for ~A" fspec))))))

(defun fspec-definition-locations (fspec)
  (let ((defs (excl::find-multiple-definitions fspec)))
    (loop for (fspec type) in defs 
          collect (list fspec (find-fspec-location fspec type)))))

(defimplementation find-definitions (symbol)
  (fspec-definition-locations symbol))

|#

(defun source-location (symbol)
  (when (pathnamep (ext:source-pathname symbol))
    (let ((pos (ext:source-file-position symbol)))
      `(((,symbol)
         (:location
           (:file ,(namestring (ext:source-pathname symbol)))
           ,(if pos
                (list :position (1+ pos))
                (list :function-name (string symbol)))
           (:align t)))))))

(defimplementation find-definitions (symbol)
  (source-location symbol))

#| 
Uncomment this if you have patched xref.lisp, as in 
http://article.gmane.org/gmane.lisp.slime.devel/2425
Also, make sure that xref.lisp is loaded by modifying the armedbear
part of *sysdep-pathnames* in swank.loader.lisp. 

;;;; XREF
(setq pxref:*handle-package-forms* '(cl:in-package))

(defmacro defxref (name function)
  `(defimplementation ,name (name)
    (xref-results (,function name))))

(defxref who-calls      pxref:list-callers)
(defxref who-references pxref:list-readers)
(defxref who-binds      pxref:list-setters)
(defxref who-sets       pxref:list-setters)
(defxref list-callers   pxref:list-callers)
(defxref list-callees   pxref:list-callees)

(defun xref-results (symbols)
  (let ((xrefs '()))
    (dolist (symbol symbols)
      (push (list symbol (cadar (source-location symbol))) xrefs))
    xrefs))
|#

;;;; Inspecting

(defmethod emacs-inspect ((slot mop::slot-definition))
  `("Name: " (:value ,(mop::%slot-definition-name slot))
             (:newline)
             "Documentation:" (:newline)
             ,@(when (slot-definition-documentation slot)
                     `((:value ,(slot-definition-documentation slot)) (:newline)))
             "Initialization:" (:newline)
             "  Args: " (:value ,(mop::%slot-definition-initargs slot)) (:newline)
             "  Form: "  ,(if (mop::%slot-definition-initfunction slot)
                              `(:value ,(mop::%slot-definition-initform slot))
                              "#<unspecified>") (:newline)
                              "  Function: " (:value ,(mop::%slot-definition-initfunction slot))
                              (:newline)))

(defmethod emacs-inspect ((f function))
  `(,@(when (function-name f)
            `("Name: " 
              ,(princ-to-string (function-name f)) (:newline)))
      ,@(multiple-value-bind (args present) 
                             (sys::arglist f)
                             (when present `("Argument list: " ,(princ-to-string args) (:newline))))
      (:newline)
      #+nil,@(when (documentation f t)
                   `("Documentation:" (:newline) ,(documentation f t) (:newline)))
      ,@(when (function-lambda-expression f)
              `("Lambda expression:" 
                (:newline) ,(princ-to-string (function-lambda-expression f)) (:newline)))))

#|
;;; XXX -- the default SLIME implementation looks ok.  Remove?  --ME 20100111
(defmethod emacs-inspect ((o t))
  (let* ((class (class-of o))
         (slots (mop::class-slots class)))
            (mapcar (lambda (slot)
                      (let ((name (mop::slot-definition-name slot)))
                        (cons (princ-to-string name)
                              (slot-value o name))))
                    slots)))
|#


;;; Although by convention toString() is supposed to be a
;;; non-computationally expensive operation this isn't always the
;;; case, so make its computation a user interaction.
(defparameter *to-string-hashtable* (make-hash-table))
(defmethod emacs-inspect ((o java:java-object))
    (let ((to-string (lambda ()
                      (handler-case
                          (setf (gethash o *to-string-hashtable*)
                                         (java:jcall "toString" o))
                        (t (e)
                          (setf (gethash o *to-string-hashtable*)
                                         (format nil "Could not invoke toString(): ~A"
                                                 e)))))))
      (append
       (if (gethash o *to-string-hashtable*)
           (label-value-line "toString()" (gethash o *to-string-hashtable*))
           `((:action "[compute toString()]" ,to-string) (:newline)))
       (loop :for (label . value) :in (sys:inspected-parts o)
          :appending (label-value-line label value)))))
  
;;;; Multithreading

#+#.(cl:if (cl:find-package :threads) '(:and) '(:or))
(progn
  (defimplementation spawn (fn &key name)
    (threads:make-thread (lambda () (funcall fn)) :name name))

  (defvar *thread-plists* (make-hash-table) ; should be a weak table
    "A hashtable mapping threads to a plist.")

  (defvar *thread-id-counter* 0)

  (defimplementation thread-id (thread)
    (threads:synchronized-on *thread-plists*
      (or (getf (gethash thread *thread-plists*) 'id)
          (setf (getf (gethash thread *thread-plists*) 'id)
              (incf *thread-id-counter*)))))

  (defimplementation find-thread (id)
    (find id (all-threads) 
        :key (lambda (thread)
                (getf (gethash thread *thread-plists*) 'id))))

  (defimplementation thread-name (thread)
    (threads:thread-name thread))

  (defimplementation thread-status (thread)
    (format nil "Thread is ~:[dead~;alive~]" (threads:thread-alive-p thread)))

  (defimplementation make-lock (&key name)
    (declare (ignore name))
    (threads:make-thread-lock))

  (defimplementation call-with-lock-held (lock function)
    (threads:with-thread-lock (lock) (funcall function)))

  (defimplementation current-thread ()
    (threads:current-thread))

  (defimplementation all-threads ()
    (copy-list (threads:mapcar-threads #'identity)))

  (defimplementation thread-alive-p (thread)
    (member thread (all-threads)))

  (defimplementation interrupt-thread (thread fn)
    (threads:interrupt-thread thread fn)) 

  (defimplementation kill-thread (thread)
    (threads:destroy-thread thread))

  (defstruct mailbox 
    (queue '()))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (threads:synchronized-on *thread-plists*
      (or (getf (gethash thread *thread-plists*) 'mailbox)
          (setf (getf (gethash thread *thread-plists*) 'mailbox)
                (make-mailbox)))))

  (defimplementation send (thread message)
    (let ((mbox (mailbox thread)))
      (threads:synchronized-on mbox
        (setf (mailbox-queue mbox) 
              (nconc (mailbox-queue mbox) (list message)))
        (threads:object-notify-all mbox))))

  (defimplementation receive-if (test &optional timeout)
    (let* ((mbox (mailbox (current-thread))))
      (assert (or (not timeout) (eq timeout t)))
      (loop
       (check-slime-interrupts)
       (threads:synchronized-on mbox
         (let* ((q (mailbox-queue mbox))
                (tail (member-if test q)))
           (when tail 
             (setf (mailbox-queue mbox) (nconc (ldiff q tail) (cdr tail)))
               (return (car tail)))
         (when (eq timeout t) (return (values nil t)))
         (threads:object-wait mbox 0.3)))))))

(defimplementation quit-lisp ()
  (ext:exit))
