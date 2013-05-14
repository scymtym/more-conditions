;;;; macros.lisp --- Macros provided by the more-conditions system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions)

(defmacro with-condition-translation (clauses &body body)
  "Execute BODY translating conditions as specified by CLAUSES when
they are signaled.

CLAUSES is a list of clauses of the form

  ((FROM-CONDITION TO-CONDITION
    &key
    var
    cause-initarg
    signal-via
    muffle?)
   INITARG1 VALUE1
   INITARG2 VALUE2
   ...)

FROM-CONDITION specifies a condition type (as e.g. in
`cl:handler-bind') instances of which should be translated to
instances of the condition class designated by TO-CONDITION.

The instance of TO-CONDITIONS is signaled via the value
of :SIGNAL-VIA (default is `cl:error') and receives the initargs
INITARG1 VALUE1, INITARG2 VALUE2, ... and the value of :CAUSE-INITARG
\(default is :CAUSE) with the original condition instance as its
value. When the value of :CAUSE-INITARG is nil, the original condition
is not passed to the constructed condition instance.

If supplied, the value of :VAR names a variable in which the original
condition instance should be received.

MUFFLE? controls whether the original condition should be muffled
after the translation has been performed. \(This is useful for
`cl:warning's and generic `cl:condition's which would not get handled
by resignaling via e.g. `cl:warn').  "
  (flet ((do-clause (clause)
           (destructuring-bind
                 ((from-condition to-condition
                   &key
                   (var            (gensym) var-supplied?)
                   (cause-initarg  :cause)
                   (signal-via     'error)
                   (muffle?        (subtypep from-condition 'warning)))
                  &body initargs)
               clause
             (when var-supplied?
               (check-type var symbol))
             (check-type from-condition (or symbol cons))
             (check-type to-condition   symbol "the name of a condition class")
             (check-type cause-initarg  symbol)
             (check-type signal-via     symbol)

             `((and ,from-condition (not ,to-condition))
               (lambda (,var)
                 ,@(unless (or var-supplied? cause-initarg muffle?)
                     `((declare (ignore ,var))))
                 (,signal-via ',to-condition
                              ,@initargs
                              ,@(when cause-initarg
                                  `(,cause-initarg ,var)))
                 ,@(when muffle?
                     `((muffle-warning ,var))))))))

    (multiple-value-bind (body declarations)
        (parse-body body)
      `(handler-bind (,@(mapcar #'do-clause clauses))
         (locally
             ,@declarations
           ,@body)))))

(defmacro define-condition-translating-method
    (name (&rest args) &body clauses)
  "Define a method on the generic function designated by NAME which
translates conditions according to CLAUSES (For a description of
CLAUSES, see `with-condition-translation')."
  (multiple-value-bind (required optional rest keys other? aux)
      (parse-ordinary-lambda-list args :allow-specializers t)
    (declare (ignore required other? aux))
    (flet ((just-the-var (arg-spec)
             (destructuring-bind (name &optional (var name))
                 (ensure-list (first arg-spec))
               var)))
     `(defmethod ,name :around (,@args)
        (declare (ignorable ,@(ensure-list rest)
                            ,@(mapcar #'just-the-var optional)
                            ,@(mapcar #'just-the-var keys)))
        (with-condition-translation (,@clauses)
          (call-next-method))))))

;;; Error behavior

(defmacro error-behavior-restart-case ((var (error-condition
                                             &rest initargs
                                             &key &allow-other-keys)
                                        &key
                                        warning-condition
                                        (allow-other-values? t))
                                       &body clauses)
  "Select error/warning signaling of ERROR-CONDITION or
WARNING-CONDITION according to VAR and establish restarts as specified
in CLAUSES.

CLAUSES use the same syntax as the restart clauses in
`cl:restart-case'.

INITARGS are passed to the constructed conditions.

ALLOW-OTHER-VALUES? controls whether the form should evaluate to the
value of VAR if it is not a function.

Example:

  (flet ((try-policy (policy)
           (error-behavior-restart-case
              (policy
               (simple-error
                :format-control   \"Example error: ~A\"
                :format-arguments (list :foo))
               :warning-condition   simple-warning
               :allow-other-values? t)
             (continue (&optional condition)
              :continue))))
    ;; (try-policy #'error) => Error: Example error: FOO
    ;; (try-policy 'error) => Error: Example error: FOO
    (mapcar #'try-policy (list warn #'warn continue #'continue 1 :foo nil)))
  |  WARNING: Example error: FOO
  |  WARNING: Example error: FOO
  => (nil nil :continue :continue 1 :foo nil)
"
  (once-only (var)
    `(,(if allow-other-values? 'typecase 'etypecase) ,var
       ((or function (and symbol (not (or keyword null))))
        (restart-case
            (funcall ,var
                     (make-condition
                      (cond
                        ((member ,var `(warn ,#'warn))
                         ,(if warning-condition
                              `',warning-condition
                              `(error
                                'simple-program-error
                                :format-control "~@<Requested behavior ~
                                                   ~S is not ~
                                                   allowed.~@:>"
                                :format-arguments `(,,var))))
                        (t
                         ',error-condition))
                      ,@initargs))
          ,@clauses))
      ,@(when allow-other-values?
          `((t ,var))))))

;;; Utility functions

(defmacro with-trivial-progress
    ((operation
      &optional format-control-or-condition-class
      &rest format-arguments-or-initargs)
     &body body)
  "Signal one progress condition for OPERATION for the start and end
of the execution of BODY respectively.

As with `cl:signal', `cl:error' and `cl:warn',
FORMAT-CONTROL-OR-CONDITION-CLASS and FORMAT-ARGUMENTS-OR-INITARGS
either specify a condition class and initargs or a report format
control string and format arguments."
  (once-only (operation)
    `(unwind-protect
          (progn
            (progress ,operation 0 ,format-control-or-condition-class
                      ,@format-arguments-or-initargs)
            ,@body)
       (progress ,operation t))))

(defmacro with-sequence-progress ((operation sequence) &body body)
  "Signal progress conditions for OPERATION on SEQUENCE during the
execution of BODY.

The function `progress' is shadowed in the lexical scope of BODY with
the following syntax:

  progress [ format-control-or-condition-class
             format-arguments-or-initargs* ]

Calling this function indicates that the processing of SEQUENCE
advanced by one element. As with `cl:signal', `cl:error' and
`cl:warn', FORMAT-CONTROL-OR-CONDITION-CLASS and
FORMAT-ARGUMENTS-OR-INITARGS either specify a condition class and
initargs or a report format control string and format arguments.

After the completion of or non-local exit from BODY, a condition
indicating the completion of OPERATION is signaled automatically."
  (with-gensyms (length)
    (once-only (operation sequence)
      `(unwind-protect
            (let ((,length (length ,sequence))
                  (i       0))
              (flet ((compute-progress ()
                       (/ (1- (incf i)) ,length))
                     (progress (&optional format-control-or-condition-class
                                &rest format-arguments-or-initargs)
                       (apply #'progress ,operation nil
                              format-control-or-condition-class
                              format-arguments-or-initargs)))
                (handler-bind
                    ((progress-condition
                       (lambda (condition)
                         (unless (progress-condition-progress condition)
                           (setf (progress-condition-progress condition)
                                 (compute-progress))))))
                  ,@body)))
         (progress ,operation t)))))
