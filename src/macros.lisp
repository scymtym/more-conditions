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
	       #'(lambda (,var)
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
