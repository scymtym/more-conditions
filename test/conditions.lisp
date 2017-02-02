;;;; conditions.lisp --- Unit tests for conditions provided by the more-conditions system.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions.test)

(in-suite :more-conditions)

(define-condition maybe-print-cause.print.error1 (error
                                                  chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Foo-error ~
                     occurred.~/more-conditions:maybe-print-cause/"
             condition))))

(define-condition maybe-print-cause.print.error2 (error
                                                  chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error occurred.~@:_~
                     ~2@T~@<Foo.~/more-conditions:maybe-print-cause/~:>~@:>"
             condition))))

(test maybe-print-cause.print
  "Test printing condition instances using the `maybe-print-cause'
   helper function."

  (mapc (lambda+ ((condition initargs expected))
          (is (string= expected
                       (princ-to-string
                        (apply #'make-condition condition initargs)))))

        `((maybe-print-cause.print.error1
           ()
           "Foo-error occurred.")
          ;; Cause is present. Not printing within a logical block =>
          ;; cause description should be printed on a fresh line but
          ;; without indentation.
          (maybe-print-cause.print.error1
           (:cause ,(make-condition
                     'simple-error
                     :format-control   "~@<The number was~@:_~S.~@:>"
                     :format-arguments '(1)))
           "Foo-error occurred. Caused by:
> The number was
> 1.")
          ;; Cause is present. Printing within a logical block =>
          ;; cause description should be printed on a fresh line and
          ;; with current indentation of the logical block.
          (maybe-print-cause.print.error2
           (:cause ,(make-condition
                     'simple-error
                     :format-control   "~@<The number was~@:_~S.~@:>"
                     :format-arguments '(1)))
           #+sbcl "Error occurred.
  Foo. Caused by:
  > The number was
  > 1."
           #-sbcl "Error occurred.
  Foo. Caused by:
> The number was
> 1."))))

(define-condition simple-foo-error (simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Foo-error ~
                     occurred~/more-conditions:maybe-print-explanation/"
             condition))))

(test maybe-print-explanation.print
  "Test printing condition instances using the
   `maybe-print-explanation' helper function."

  (mapc
   (lambda+ ((initargs expected))
     (is (string= expected
                  (princ-to-string
                   (apply #'make-condition 'simple-foo-error initargs)))))

   '(;; No format-control.
     #+sbcl (()
      "Foo-error occurred.")
     ;; With format-control.
     ((:format-control   "the number was ~S."
       :format-arguments (1))
      "Foo-error occurred: the number was 1."))))

;;; Program error conditions

(defun princ-to-string-with-special-bindings (bindings thing)
  (progv (mapcar #'first bindings)
      (mapcar #'second bindings)
    (princ-to-string thing)))

(defmacro define-condition-suite ((name
                                   &key
                                   (constructor name)
                                   (repeat      1))
                                  &body cases)
  (let ((suite-name            (make-keyword name))
        (condition-case-name   (symbolicate
                                name '#:.construct-and-print/make-condition))
        (constructor-case-name (symbolicate
                                name '#:.construct-and-print/constructor)))
    `(progn
       (def-suite ,suite-name
         :in :more-conditions
         :description
         ,(format nil "Unit tests for the `~(~A~)' condition class."
                  name))
       (in-suite ,suite-name)

       (test ,condition-case-name
         ,(format nil "Test printing instances of the `~(~A~)' condition class"
                  name)

         (mapc (lambda+ ((initargs expected
                          &key constructor-args special-bindings))
                 (declare (ignore constructor-args))
                 (let ((instance (apply #'make-condition ',name initargs)))
                   (loop :repeat ,repeat :do
                      (is (string= expected
                                   (princ-to-string-with-special-bindings
                                    special-bindings instance))))))
               (list ,@cases)))

       ,@(when (fboundp constructor)
           `((test ,constructor-case-name
               ,(format nil "Test printing instances of the `~(~A~)' condition class"
                        name)

               (mapc (lambda+ ((initargs expected
                                &key constructor-args special-bindings))
                       (declare (ignore initargs))
                       (unless (eq constructor-args :skip)
                         (is (string= expected
                                      (handler-case
                                          (apply #',constructor constructor-args)
                                        (,name (condition)
                                          (princ-to-string-with-special-bindings
                                           special-bindings condition)))))))
                     (list ,@cases))))))))

(define-condition-suite (missing-required-argument)
  '((:parameter :foo)
    "No value has been supplied for the required parameter :FOO."
    :constructor-args (:foo)))

(define-condition-suite (incompatible-arguments)
  '((:parameters () :values ())
    "No arguments are invalid.")
  '((:parameters (:foo) :values (1))
    "The combination of arguments

  :FOO 1

is invalid."
    :constructor-args (:foo 1))
  '((:parameters (:foo :barbar) :values (1 2))
    "The combination of arguments

  :FOO    1
  :BARBAR 2

is invalid."
    :constructor-args (:foo 1 :barbar 2))
  `((:parameters (:foo)
     :values     (1)
     :cause      ,(make-instance 'simple-error
                                 :format-control "foo"))
    "The combination of arguments

  :FOO 1

is invalid.
Caused by:
> foo"
    :constructor-args :skip))

(define-condition-suite (initarg-error)
  '((:class :foo)
    "Invalid initargs have been supplied for class :FOO."))

(define-condition-suite (missing-required-initarg)
  '((:class :foo :parameter :bar)
    "The initarg :BAR is required by class :FOO, but has not been supplied."
    :constructor-args (:foo :bar)))

(define-condition-suite (incompatible-initargs)
  '((:class :foo :parameters () :values ())
    "No initargs are invalid for class :FOO."
    :constructor-args (:foo))

  '((:class :foo :parameters (:bar) :values (1))
    "The combination of initargs

  :BAR 1

is invalid for class :FOO."
    :constructor-args (:foo :bar 1))

  '((:class :foo :parameters (:bar :bazbaz) :values (1 2))
    "The combination of initargs

  :BAR    1
  :BAZBAZ 2

is invalid for class :FOO."
    :constructor-args (:foo :bar 1 :bazbaz 2))

  `((:class      :foo
     :parameters (:bar)
     :values     (1)
     :cause      ,(make-instance 'simple-error
                                 :format-control "foo"))
    "The combination of initargs

  :BAR 1

is invalid for class :FOO.
Caused by:
> foo"
    :constructor-args :skip))

;;; `reference-condition'

(define-condition reference-error (error reference-condition)
  ()
  (:report "Reference Error."))

(define-condition-suite (reference-error
                         :repeat 2) ; because of caching

  ;; No initargs => no references are printed.
  '(()
    "Reference Error.")

  ;; Explicit empty list of references => no references are printed.
  '((:references ())
    "Reference Error.")

  ;; A single reference => it is printed.
  '((:references ((:foo "bar")))
    "Reference Error.
See also:
  FOO, bar")

  ;; *print-references* is nil => references should not be printed.
  '((:references ((:foo "bar")))
    "Reference Error."
    :special-bindings ((*print-references* nil)))

  ;; Multiple different references for the same document => they are
  ;; all printed.
  '((:references ((:foo "bar")
                  (:foo ("bar" "baz"))
                  (:foo ("bar" "baz") "http://fez.org")))
    "Reference Error.
See also:
  FOO, bar
  FOO, bar » baz
  FOO, bar » baz <http://fez.org>")

  ;; Reference is specified as a fucntion => the function is called
  ;; and the returned reference is printed.
  `((:references ((:bar "baz")
                  ,(lambda (condition)
                     (declare (ignore condition))
                     '((:foo "bar" "http://fez.org")))))
    "Reference Error.
See also:
  BAR, baz
  FOO, bar <http://fez.org>"))

(define-condition mock-error/reference-condition (error
                                                  reference-condition
                                                  chainable-condition)
  ()
  (:report (lambda (condition stream)
             (format stream "Mock Error.~
                             ~/more-conditions:maybe-print-cause/"
                     condition))))

(define-condition-suite (mock-error/reference-condition
                         :repeat 2) ; because of caching
  `((:references ((:foo "bar")
                  (:fez "whiz"))
     :cause      ,(make-condition 'reference-error
                                  :references '((:foo "bar")
                                                (:foo "baz")
                                                (:bar "fez" "http://whoop.org"))))
    "Mock Error. Caused by:
> Reference Error.
See also:
  FOO, bar
  FOO, baz
  BAR, fez <http://whoop.org>
  FEZ, whiz"))

(define-condition reference-condition.inheritance.super^2error
    (error reference-condition)
  ()
  (:default-initargs
   :direct-references '((:super^2 "super^2"))))

(define-condition reference-condition.inheritance.supererror
    (reference-condition.inheritance.super^2error)
  ()
  (:default-initargs
   :direct-references '((:super "super" "http://super.org"))))

(define-condition reference-condition.inheritance.suberror.1
    (reference-condition.inheritance.supererror)
  ()
  (:default-initargs
   :direct-references '((:sub "sub")))
  (:report "Mock Error."))

(define-condition-suite (reference-condition.inheritance.suberror.1
                         :repeat 2) ; because of caching

  ;; No initargs => :direct-references from transitive
  ;; superclass-closure are collected and printed.
  `(()
    "Mock Error.
See also:
  SUB, sub
  SUPER, super <http://super.org>
  SUPER^2, super^2")

  ;; :references initarg overwrites all default references.
  '((:references ((:no-defaults "nodefaults")))
    "Mock Error.
See also:
  NO-DEFAULTS, nodefaults"))

(define-condition reference-condition.inheritance.supererror.1
    (error reference-condition)
  ()
  (:default-initargs
   :direct-references '((:super   "super")
                        (:super.1 "super.1"))))

(define-condition reference-condition.inheritance.supererror.2
    (error reference-condition)
  ()
  (:default-initargs
   :direct-references '((:super   "super")
                        (:super.2 "super.2" "http://super.org"))))

(define-condition reference-condition.inheritance.suberror.2
    (reference-condition.inheritance.supererror.1
     reference-condition.inheritance.supererror.2)
  ()
  (:report "Mock Error."))

(define-condition-suite (reference-condition.inheritance.suberror.2
                         :repeat 2) ; because of caching

  ;; No initargs => :direct-references from transitive
  ;; superclass-closure are collected and printed.
  `(()
    "Mock Error.
See also:
  SUPER, super
  SUPER.1, super.1
  SUPER.2, super.2 <http://super.org>")

  ;; :references initarg overwrites all default references.
  '((:references ((:no-defaults "nodefaults")))
    "Mock Error.
See also:
  NO-DEFAULTS, nodefaults"))

(define-condition reference-condition.inheritance.supererror.3
    (reference-condition.inheritance.super^2error)
  ()
  (:default-initargs
   :references :compute))

(define-condition reference-condition.inheritance.suberror.3
    (reference-condition.inheritance.supererror.3)
  ()
  (:default-initargs
   :direct-references '((:sub "sub")))
  (:report "Mock Error."))

(define-condition-suite (reference-condition.inheritance.suberror.3
                         :repeat 2) ; because of caching

  ;; No initargs => :direct-references from transitive
  ;; superclass-closure are collected and printed.
  '(()
    "Mock Error.
See also:
  SUB, sub
  SUPER^2, super^2")

  ;; :references initarg overwrites all default references.
  '((:references ((:no-defaults "nodefaults")))
    "Mock Error.
See also:
  NO-DEFAULTS, nodefaults"))

(test reference-condition.print/logical-block
  "Test printing of `reference-condition' within a logical block."

  (mapc
   (lambda+ ((class initargs expected ))
     (is (string= expected
                  (format nil "~@<| ~@;~A~:>"
                          (apply #'make-condition class initargs)))))

   `((reference-condition.inheritance.suberror.1
      (:references ((:foo "bar")
                    (:fez "whiz")))
      #+sbcl "| Mock Error.
| See also:
|   FOO, bar
|   FEZ, whiz"
      #-sbcl "| Mock Error. See also:
|   FOO, bar
|   FEZ, whiz")

     (mock-error/reference-condition
      (:references ((:foo "bar")
                    (:fez "whiz"))
       :cause      ,(make-condition
                     'reference-error
                     :references '((:foo "bar")
                                   (:foo "baz")
                                   (:bar "fez" "http://whoop.org"))))
      #+sbcl "| Mock Error. Caused by:
| > Reference Error.
| See also:
|   FOO, bar
|   FOO, baz
|   BAR, fez <http://whoop.org>
|   FEZ, whiz"
      #-sbcl "| Mock Error. Caused by:
| > Reference Error. See also:
|   FOO, bar
|   FOO, baz
|   BAR, fez <http://whoop.org>
|   FEZ, whiz"))))

;;; Progress conditions

(define-condition-suite (progress-condition :constructor progress)
  `(()
    "???.?? %")

  `((:operation :foo)
    "???.?? % FOO"
    :constructor-args (:foo))

  `((:operation :foo :progress nil)
    "???.?? % FOO"
    :constructor-args (:foo nil))

  `((:operation :foo :progress 0)
    "  0.00 % FOO"
    :constructor-args (:foo 0))

  `((:operation :foo :progress 1)
    "100.00 % FOO"
    :constructor-args (:foo 1))

  `((:operation :foo :progress t)
    "100.00 % FOO"
    :constructor-args (:foo t))

  `((:operation :foo :progress .51234)
    " 51.23 % FOO"
    :constructor-args (:foo .51234 progress-condition)))

(define-condition-suite (simple-progress-condition :constructor progress)
  `((:operation :foo :progress .51234)
    " 51.23 % FOO"
    :constructor-args (:foo .51234 simple-progress-condition))

  `((:operation :foo :progress .51234 :format-control "bar")
    " 51.23 % FOO: bar"
    :constructor-args (:foo .51234 "bar"))

  `((:operation        :foo
     :progress         .51234
     :format-control   "bar: ~A"
     :format-arguments (:baz))
    " 51.23 % FOO: bar: BAZ"
    :constructor-args (:foo .51234 "bar: ~A" :baz)))
